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
module Pmutex = Longleaf_util.Pmutex

(* Re-use types and modules from the original gadt.ml *)
open Gadt

let num_iterations = 2000

(* Communication types for Saturn-based worker approach (works across C boundary) *)
type work_request = {
  params : float array;
  strategy : Gadt_strategy.t;
  vars : (Uuidm.t * (Type.shadow * Gadt.bounds)) array;
  iteration : int;
}

type work_result = (float option, Error.t) Result.t

(* Stats storage: iteration -> (objective_value, trade_stats, final_cash) *)
type stats_entry = {
  objective_value : float;
  final_cash : float;
  trade_stats : State.Stats.TradeStats.t option;
  params : float array;
}

(* Work request with Saturn queue for reply (domain-safe) *)
type work_with_reply = {
  work : work_request;
  result_queue : work_result Saturn.Queue.t;
  stats_htbl : (int, stats_entry) Saturn.Htbl.t;
}

(* Worker message: either work to do or terminate signal *)
type worker_message = Work of work_with_reply | Terminate

module Worker = struct
  (* Worker loop that processes work from Saturn queue *)
  let top work_queue bars (options : Options.t) mutices =
    let rec worker_loop () =
      Printf.printf "WORKER: waiting for work\n%!";
      (* Block until message arrives - Saturn queue pop_opt with spin loop *)
      let message =
        let rec wait () =
          match Saturn.Queue.pop_opt work_queue with
          | Some msg ->
            Printf.printf "WORKER: got message!\n%!";
            msg
          | None ->
            (* Yield to other fibers while waiting *)
            Eio.Fiber.yield ();
            wait ()
        in
        wait ()
      in
      match message with
      | Terminate ->
        Printf.printf "WORKER: received terminate signal, exiting\n%!";
        ()
      | Work { work; result_queue; stats_htbl } ->
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
                let* instantiated_score =
                  Subst.instantiate env work.strategy.score
                in
                let instantiated_strategy =
                  {
                    work.strategy with
                    buy_trigger = instantiated_buy;
                    sell_trigger = instantiated_sell;
                    score = instantiated_score;
                  }
                in
                let options_with_sw = { options with Options.switch = sw } in
                let* res =
                  try
                    Gadt_strategy.run bars options_with_sw mutices
                      instantiated_strategy
                  with
                  | Not_found as exn ->
                    (* Log Not_found with more context *)
                    Eio.traceln "=== NOT_FOUND EXCEPTION ===";
                    Eio.traceln
                      "This likely means Saturn.Htbl.find_exn or similar \
                       raised Not_found";
                    Eio.traceln "Parameters: %a" (Array.pp Float.pp) work.params;
                    Eio.traceln "Backtrace recording enabled: %b"
                      (Printexc.backtrace_status ());
                    Eio.traceln "Raw backtrace: %s"
                      (Printexc.raw_backtrace_to_string
                         (Printexc.get_raw_backtrace ()));
                    Eio.traceln "Get backtrace: %s" (Printexc.get_backtrace ());
                    Eio.traceln "===========================";
                    (* Try to get more info by re-raising in a controlled way *)
                    Eio.traceln "Attempting to get backtrace by re-raising...";
                    (try raise exn with
                    | Not_found ->
                      Eio.traceln "Re-raised backtrace: %s"
                        (Printexc.get_backtrace ()));
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

                (* Compute and store trade statistics *)
                (try
                   let state = Pmutex.get mutices.state_mutex in
                   let final_cash = State.cash state in
                   let all_orders = State.order_history state in
                   let trade_stats =
                     State.Stats.TradeStats.compute all_orders
                   in
                   let stats_entry =
                     {
                       objective_value = res;
                       final_cash;
                       trade_stats;
                       params = Array.copy work.params;
                     }
                   in
                   ignore
                     (Saturn.Htbl.try_add stats_htbl work.iteration stats_entry);
                   Eio.traceln
                     "Stored stats for iteration %d (cash: %.2f, objective: \
                      %.2f)"
                     work.iteration final_cash res
                 with
                | e ->
                  Eio.traceln
                    "Warning: Failed to compute stats for iteration %d: %s"
                    work.iteration (Printexc.to_string e));

                Ok (Some res)
              with
              | e ->
                (* Catch any other errors during substitution/instantiation *)
                Eio.traceln "=== INSTANTIATION ERROR ===";
                Eio.traceln "Exception: %s" (Printexc.to_string e);
                Eio.traceln "Backtrace: %s" (Printexc.get_backtrace ());
                Eio.traceln "Parameters: %a" (Array.pp Float.pp) work.params;
                Eio.traceln "===========================";
                Ok (Some 0.0))
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
  let f work_queue stats_htbl strategy vars iteration_counter (l : float array)
      _grad =
    let iteration = Atomic.fetch_and_add iteration_counter 1 in
    Printf.printf "OBJ FUNC: iteration %d called from Nlopt\n%!" iteration;

    (* Create result queue for this iteration *)
    let result_queue = Saturn.Queue.create () in

    (* Create work request *)
    let work = { params = Array.copy l; strategy; vars; iteration } in
    let work_with_reply = { work; result_queue; stats_htbl } in

    (* Send work to worker via Saturn queue (safe from C) *)
    Printf.printf "OBJ FUNC: pushing work to queue\n%!";
    Saturn.Queue.push work_queue (Work work_with_reply);
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

  let vars = Gadt_strategy.collect_all_variables strategy |> Array.of_list in

  Eio.traceln "--- COLLECTED VARIABLES ---";
  let len = Array.length vars in
  Eio.traceln "Total unique variables: %d" len;

  Eio.traceln "Buy trigger: %a" Gadt.pp strategy.buy_trigger;
  Eio.traceln "Sell trigger: %a" Gadt.pp strategy.sell_trigger;

  (* Create Saturn queue for work requests (works across C boundary) *)
  let work_queue = Saturn.Queue.create () in
  let iteration_counter = Atomic.make 0 in

  (* Create hash table for storing stats per iteration *)
  let module IntHashedType = struct
    type t = int

    let equal = Int.equal
    let hash = Hashtbl.hash
  end in
  let stats_htbl = Saturn.Htbl.create ~hashed_type:(module IntHashedType) () in

  (* Log the options being used for optimization *)
  Eio.traceln "Optimization options:";
  Eio.traceln "  start tick: %d" options.flags.start;
  Eio.traceln "  runtype: %a" Longleaf_core.Runtype.pp options.flags.runtype;

  (* Spawn worker in separate domain via executor pool (won't be blocked by Nlopt spin) *)
  Eio.traceln "Starting worker domain via executor pool";
  let _worker_promise =
    Eio.Executor_pool.submit_fork ~sw:options.switch ~weight:1.0
      options.executor_pool (fun () ->
        (* This runs in a separate domain with its own Eio context *)
        Eio_main.run (fun env ->
            Eio.Switch.run (fun sw ->
                let options_with_env =
                  { options with Options.eio_env = env; switch = sw }
                in
                Printf.printf "Worker domain started\n%!";
                Worker.top work_queue bars options_with_env mutices)))
  in
  Eio.traceln "Worker spawned in domain";

  (* Extract per-variable bounds from GADT variable definitions *)
  let lower_bounds = Array.map (fun (_, (_, bounds)) -> bounds.lower) vars in
  let upper_bounds = Array.map (fun (_, (_, bounds)) -> bounds.upper) vars in

  Eio.traceln "--- VARIABLE BOUNDS ---";
  Array.iteri
    (fun i (_, (Type.A ty, bounds)) ->
      let ty_str =
        match ty with
        | Type.Int -> "Int"
        | Type.Float -> "Float"
        | Type.Bool -> "Bool"
        | Type.Data -> "Data"
        | Type.Tacaml -> "Tacaml"
      in
      Eio.traceln "  Var %d (%s): [%.2f, %.2f]" i ty_str bounds.lower
        bounds.upper)
    vars;

  let opt = Nlopt.create Nlopt.isres len in
  (* Set per-variable bounds (extracted from GADT Var nodes) *)
  Nlopt.set_lower_bounds opt lower_bounds;
  Nlopt.set_upper_bounds opt upper_bounds;
  Nlopt.set_maxeval opt num_iterations;

  (* Set objective function *)
  Nlopt.set_max_objective opt
    (Worker.f work_queue stats_htbl strategy vars iteration_counter);
  (* Generate random start point within each variable's bounds *)
  let start =
    Array.map
      (fun (_, (_, bounds)) ->
        Float.random_range bounds.lower bounds.upper Util.random_state)
      vars
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
  | Error e -> Eio.traceln "Error instantiating buy trigger: %a" Error.pp e);

  Eio.traceln "";

  (match instantiated_sell_result with
  | Ok sell_expr ->
    Eio.traceln "Winning Sell Trigger:";
    Eio.traceln "  %s" (Gadt.to_string sell_expr)
  | Error e -> Eio.traceln "Error instantiating sell trigger: %a" Error.pp e);

  Eio.traceln "";
  Eio.traceln "Raw parameters: %a" (Array.pp Float.pp) xopt;
  Eio.traceln "============================";
  Eio.traceln "";

  (* Find and display the best trade statistics *)
  Eio.traceln "=== SEARCHING FOR BEST TRADE STATISTICS ===";
  let best_stats =
    Saturn.Htbl.to_seq stats_htbl
    |> Seq.fold_left
         (fun acc (_iter, entry) ->
           match acc with
           | None -> Some entry
           | Some best ->
             if Float.compare entry.objective_value best.objective_value > 0
             then Some entry
             else Some best)
         None
  in

  (match best_stats with
  | Some { trade_stats = Some stats; objective_value; final_cash; params = _ }
    ->
    Eio.traceln "";
    Eio.traceln "=== BEST TRADE STATISTICS ===";
    Eio.traceln "Final Cash: $%.2f" final_cash;
    Eio.traceln "Objective Value: %.6f" objective_value;
    Eio.traceln "";
    Eio.traceln "%s" (State.Stats.TradeStats.to_string stats);
    Eio.traceln "";

    (* Display edge analysis *)
    if State.Stats.TradeStats.has_edge stats then
      Eio.traceln "✓ Strategy has a statistically significant edge!"
    else Eio.traceln "✗ Strategy does not have a statistically significant edge";
    Eio.traceln ""
  | Some { trade_stats = None; final_cash; objective_value; params = _ } ->
    Eio.traceln "Best iteration had no completed trades";
    Eio.traceln "Final Cash: $%.2f | Objective: %.6f" final_cash objective_value
  | None -> Eio.traceln "No statistics available (no iterations completed)");

  Eio.traceln "============================================";
  Eio.traceln "";

  (* Signal worker to terminate *)
  Eio.traceln "Sending terminate signal to worker";
  Saturn.Queue.push work_queue Terminate;

  Result.return fopt
