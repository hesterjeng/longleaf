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

(* Communication types for Eio-based worker approach *)
type work_request = {
  params : float array;
  strategy : Gadt_strategy.t;
  vars : (Uuidm.t * Type.shadow) array;
}

type work_result = (float option, Error.t) Result.t

(* Work request with reply channel *)
type work_with_reply = {
  work : work_request;
  reply : work_result Eio.Promise.u;
}

module Worker = struct
  (* Worker loop that processes work from stream *)
  let top work_stream bars (options : Options.t) mutices =
    let rec worker_loop () =
      (* Block until work arrives *)
      let { work; reply } = Eio.Stream.take work_stream in

      let result =
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
          let* res =
            try
              Gadt_strategy.run bars options mutices instantiated_strategy
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
      in

      (* Send result back via promise and reset indicators *)
      Eio.Promise.resolve reply result;
      Bars.reset_indicators bars;

      (* Continue processing *)
      worker_loop ()
    in
    worker_loop ()

  (* Objective function that communicates with worker via stream/promise *)
  let f work_stream clock strategy vars iteration_counter timeout_seconds (l : float array) _grad =
    let iteration = Atomic.fetch_and_add iteration_counter 1 in
    Eio.traceln "=== OPTIMIZATION ITERATION %d ===" iteration;
    Eio.traceln "Parameters: %a" (Array.pp Float.pp) l;

    (* Create promise for result *)
    let result_promise, result_resolver = Eio.Promise.create () in

    (* Create work request *)
    let work = { params = Array.copy l; strategy; vars } in
    let work_with_reply = { work; reply = result_resolver } in

    (* Send work to worker *)
    Eio.Stream.add work_stream work_with_reply;

    (* Wait for result with timeout *)
    let result =
      match
        Eio.Time.with_timeout_exn clock timeout_seconds (fun () ->
          Eio.Promise.await result_promise)
      with
      | work_result -> (
        (* Got result from worker *)
        match work_result with
        | Ok (Some value) ->
          Eio.traceln "Iteration %d result: %f" iteration value;
          value
        | Ok None ->
          Eio.traceln "Iteration %d: no result" iteration;
          0.0
        | Error err ->
          Eio.traceln "=== ITERATION %d ERROR ===" iteration;
          Eio.traceln "Parameters tried: %a" (Array.pp Float.pp) l;
          Eio.traceln "Error: %a" Error.pp err;
          0.0)
      | exception Eio.Time.Timeout ->
        Eio.traceln "=== ITERATION %d TIMEOUT ===" iteration;
        Eio.traceln "Parameters tried: %a" (Array.pp Float.pp) l;
        Eio.traceln "Timed out after %.0f seconds" timeout_seconds;
        0.0
    in
    result
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

  (* Create Eio stream for work requests (rendezvous channel) *)
  let work_stream = Eio.Stream.create 0 in
  let iteration_counter = Atomic.make 0 in
  let timeout_seconds = 300.0 in  (* 5 minute timeout per iteration *)
  let clock = Eio.Stdenv.clock options.eio_env in

  (* Log the options being used for optimization *)
  Eio.traceln "Optimization options:";
  Eio.traceln "  start tick: %d" options.flags.start;
  Eio.traceln "  runtype: %a" Longleaf_core.Runtype.pp options.flags.runtype;
  Eio.traceln "  timeout per iteration: %.0f seconds" timeout_seconds;

  (* Spawn worker using executor pool *)
  Eio.traceln "Starting worker via executor pool";
  let _worker_promise =
    Eio.Executor_pool.submit_fork
      ~sw:options.switch
      ~weight:1.0
      options.executor_pool
      (fun () ->
        Eio.traceln "Worker started in executor pool";
        Worker.top work_stream bars options mutices
      )
  in
  Eio.traceln "Worker spawned";

  let opt = Nlopt.create Nlopt.isres len in
  (* Set conservative bounds that work for most indicator periods and thresholds *)
  (* Lower bound: 5 avoids very unstable indicators and index out of bounds errors *)
  Nlopt.set_lower_bounds opt @@ Array.init len (fun _ -> 5.0);
  Nlopt.set_upper_bounds opt @@ Array.init len (fun _ -> 100.0);
  Nlopt.set_maxeval opt 30000;

  (* Set objective function *)
  Nlopt.set_max_objective opt
    (Worker.f work_stream clock strategy vars iteration_counter timeout_seconds);
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
