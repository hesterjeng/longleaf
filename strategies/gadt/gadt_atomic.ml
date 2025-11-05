module Error = Longleaf_core.Error
module Signal = Longleaf_core.Signal
module Instrument = Longleaf_core.Instrument
module State = Longleaf_state
module Backend = Longleaf_backend
module Bars = Longleaf_bars
module Util = Longleaf_util
module Order = Longleaf_core.Order
module Data = Bars.Data
module Time = Longleaf_core.Time
module Options = Longleaf_core.Options

(* Re-use types and modules from the original gadt.ml *)
open Gadt

(* Communication types for Saturn-based worker approach (works across C boundary) *)
type work_request = {
  params : float array;
  strategy : Gadt_strategy.t;
  vars : (Uuidm.t * Type.shadow) array;
  iteration : int;
}

type work_result = (float option, Error.t) Result.t

(* Work request with Saturn queue for reply (domain-safe) *)
type work_with_reply = {
  work : work_request;
  result_queue : work_result Saturn.Queue.t;
}

module Worker = struct
  (* Worker loop that processes work from Saturn queue *)
  let top work_queue bars (options : Options.t) mutices =
    let rec worker_loop () =
      Printf.printf "WORKER: waiting for work\n%!";
      (* Block until work arrives - Saturn queue pop_opt with spin loop *)
      let { work; result_queue } =
        let rec wait () =
          match Saturn.Queue.pop_opt work_queue with
          | Some work ->
            Printf.printf "WORKER: got work!\n%!";
            work
          | None ->
            (* Yield to other fibers while waiting *)
            Eio.Fiber.yield ();
            wait ()
        in
        wait ()
      in

      Printf.printf "WORKER: processing iteration %d\n%!" work.iteration;
      Eio.traceln "=== OPTIMIZATION ITERATION %d ===" work.iteration;
      Eio.traceln "Parameters: %a" (Array.pp Float.pp) work.params;

      let result =
        (* Run strategy within its own switch to isolate cancellation context *)
        Eio.Switch.run (fun sw ->
          try
            let ( let* ) = Result.( let* ) in
            let env = Subst.env_of_arr work.params work.vars in
            let* instantiated_buy =
              Subst.instantiate env work.strategy.buy_trigger
            in
            let* instantiated_sell =
              Subst.instantiate env work.strategy.sell_trigger
            in
            let instantiated_strategy =
              {
                work.strategy with
                buy_trigger = instantiated_buy;
                sell_trigger = instantiated_sell;
              }
            in
            let options_with_sw = { options with Options.switch = sw } in
            let* res =
              try
                Gadt_strategy.run bars options_with_sw mutices instantiated_strategy
              with
              | Not_found as exn ->
                (* Log Not_found with more context *)
                Eio.traceln "=== NOT_FOUND EXCEPTION ===";
                Eio.traceln "This likely means Saturn.Htbl.find_exn or similar raised Not_found";
                Eio.traceln "Parameters: %a" (Array.pp Float.pp) work.params;
                Eio.traceln "Backtrace recording enabled: %b" (Printexc.backtrace_status ());
                Eio.traceln "Raw backtrace: %s" (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()));
                Eio.traceln "Get backtrace: %s" (Printexc.get_backtrace ());
                Eio.traceln "===========================";
                (* Try to get more info by re-raising in a controlled way *)
                Eio.traceln "Attempting to get backtrace by re-raising...";
                (try raise exn with Not_found ->
                  Eio.traceln "Re-raised backtrace: %s" (Printexc.get_backtrace ()));
                Ok 0.0
              | e ->
                (* Log exception details but continue *)
                Eio.traceln "=== STRATEGY EXECUTION EXCEPTION ===";
                Eio.traceln "Exception: %s" (Printexc.to_string e);
                Eio.traceln "Backtrace: %s" (Printexc.get_backtrace ());
                Eio.traceln "====================================";
                Ok 0.0
            in
            Eio.traceln "Iteration %d result: %f" work.iteration res;
            Ok (Some res)
          with
          | e ->
            (* Catch any other errors during substitution/instantiation *)
            Eio.traceln "=== INSTANTIATION ERROR ===";
            Eio.traceln "Exception: %s" (Printexc.to_string e);
            Eio.traceln "Backtrace: %s" (Printexc.get_backtrace ());
            Eio.traceln "Parameters: %a" (Array.pp Float.pp) work.params;
            Eio.traceln "===========================";
            Ok (Some 0.0)
        )
      in

      (* Send result back via Saturn queue and reset indicators *)
      Printf.printf "WORKER: pushing result back\n%!";
      Saturn.Queue.push result_queue result;
      Printf.printf "WORKER: result pushed, resetting indicators\n%!";
      Bars.reset_indicators bars;

      (* Continue processing *)
      Printf.printf "WORKER: loop iteration complete\n%!";
      worker_loop ()
    in
    worker_loop ()

  (* Objective function that communicates with worker via Saturn queue (works from C) *)
  let f work_queue strategy vars iteration_counter (l : float array) _grad =
    let iteration = Atomic.fetch_and_add iteration_counter 1 in
    Printf.printf "OBJ FUNC: iteration %d called from Nlopt\n%!" iteration;

    (* Create result queue for this iteration *)
    let result_queue = Saturn.Queue.create () in

    (* Create work request *)
    let work = { params = Array.copy l; strategy; vars; iteration } in
    let work_with_reply = { work; result_queue } in

    (* Send work to worker via Saturn queue (safe from C) *)
    Printf.printf "OBJ FUNC: pushing work to queue\n%!";
    Saturn.Queue.push work_queue work_with_reply;
    Printf.printf "OBJ FUNC: work pushed, waiting for result\n%!";

    (* Wait for result from Saturn queue (blocking spin loop) *)
    let work_result =
      let rec wait () =
        match Saturn.Queue.pop_opt result_queue with
        | Some result ->
          Printf.printf "OBJ FUNC: got result\n%!";
          result
        | None ->
          (* Spin wait - no Eio operations allowed here (called from C) *)
          Domain.cpu_relax ();
          wait ()
      in
      wait ()
    in

    (* Process result *)
    match work_result with
    | Ok (Some value) ->
      Printf.printf "OBJ FUNC: returning value %f\n%!" value;
      value
    | Ok None -> 0.0
    | Error _err -> 0.0
end

let opt_atomic bars (options : Options.t) mutices (strategy : Gadt_strategy.t) =
  let ( let* ) = Result.( let* ) in
  Eio.traceln "=== EIO-BASED OPTIMIZATION START ===";
  Eio.traceln "Strategy name: %s" strategy.name;

  let buy_vars = Subst.collect_variables strategy.buy_trigger in
  let sell_vars = Subst.collect_variables strategy.sell_trigger in
  let vars = buy_vars @ sell_vars |> Array.of_list in

  Eio.traceln "--- COLLECTED VARIABLES ---";
  let len = Array.length vars in
  Eio.traceln "Total unique variables: %d" len;

  Eio.traceln "Buy trigger: %a" Gadt.pp strategy.buy_trigger;
  Eio.traceln "Sell trigger: %a" Gadt.pp strategy.sell_trigger;

  (* Create Saturn queue for work requests (works across C boundary) *)
  let work_queue = Saturn.Queue.create () in
  let iteration_counter = Atomic.make 0 in

  (* Log the options being used for optimization *)
  Eio.traceln "Optimization options:";
  Eio.traceln "  start tick: %d" options.flags.start;
  Eio.traceln "  runtype: %a" Longleaf_core.Runtype.pp options.flags.runtype;

  (* Spawn worker in separate domain via executor pool (won't be blocked by Nlopt spin) *)
  Eio.traceln "Starting worker domain via executor pool";
  let _worker_promise =
    Eio.Executor_pool.submit_fork
      ~sw:options.switch
      ~weight:1.0
      options.executor_pool
      (fun () ->
        (* This runs in a separate domain with its own Eio context *)
        Eio_main.run (fun env ->
          Eio.Switch.run (fun sw ->
            let options_with_env = { options with Options.eio_env = env; switch = sw } in
            Printf.printf "Worker domain started\n%!";
            Worker.top work_queue bars options_with_env mutices
          )
        )
      )
  in
  Eio.traceln "Worker spawned in domain";

  let opt = Nlopt.create Nlopt.isres len in
  (* Set conservative bounds that work for most indicator periods and thresholds *)
  (* Lower bound: 5 avoids very unstable indicators and index out of bounds errors *)
  Nlopt.set_lower_bounds opt @@ Array.init len (fun _ -> 5.0);
  Nlopt.set_upper_bounds opt @@ Array.init len (fun _ -> 100.0);
  Nlopt.set_maxeval opt 15000;

  (* Set objective function *)
  Nlopt.set_max_objective opt
    (Worker.f work_queue strategy vars iteration_counter);
  let start =
    Array.init len (fun _ -> Float.random_range 5.0 100.0 Util.random_state)
  in
  Eio.traceln "Optimization start %a" (Array.pp Float.pp) start;
  let res, xopt, fopt = Nlopt.optimize opt start in

  (* Print detailed optimization results with instantiated strategy *)
  Eio.traceln "";
  Eio.traceln "=== OPTIMIZATION COMPLETE ===";
  Eio.traceln "Strategy: %s" strategy.name;
  Eio.traceln "Result: %s" (Nlopt.string_of_result res);
  Eio.traceln "Best objective value: %.6f" fopt;
  Eio.traceln "";

  (* Instantiate the winning strategy to show actual parameters *)
  let env = Subst.env_of_arr xopt vars in
  let instantiated_buy_result = Subst.instantiate env strategy.buy_trigger in
  let instantiated_sell_result = Subst.instantiate env strategy.sell_trigger in

  (match instantiated_buy_result with
   | Ok buy_expr ->
     Eio.traceln "Winning Buy Trigger:";
     Eio.traceln "  %s" (Gadt.to_string buy_expr)
   | Error e ->
     Eio.traceln "Error instantiating buy trigger: %a" Error.pp e);

  Eio.traceln "";

  (match instantiated_sell_result with
   | Ok sell_expr ->
     Eio.traceln "Winning Sell Trigger:";
     Eio.traceln "  %s" (Gadt.to_string sell_expr)
   | Error e ->
     Eio.traceln "Error instantiating sell trigger: %a" Error.pp e);

  Eio.traceln "";
  Eio.traceln "Raw parameters: %a" (Array.pp Float.pp) xopt;
  Eio.traceln "============================";
  Eio.traceln "";

  Result.return fopt
