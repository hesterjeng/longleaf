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

(* Communication types for atomic domain approach *)
type work_request = {
  params : float array;
  strategy : Gadt_strategy.t;
  vars : (Uuidm.t * Type.shadow) array;
}

type work_result = (float option, Error.t) Result.t

module Worker = struct
  (* Start worker domain that waits for work *)
  let top request_atomic result_atomic shutdown_atomic bars
      (options : Options.t) mutices =
   fun () ->
    let rec worker_loop () =
      match Atomic.get request_atomic with
      | None ->
        if Atomic.get shutdown_atomic then (
          Eio.traceln "Worker: Shutdown flag set, exiting";
          ()
        )
        else (
          (* No work, yield to Eio scheduler and try again *)
          Eio.Fiber.yield ();
          worker_loop ())
      | Some work ->

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

        (* Store result, clear request, THEN reset indicators *)
        Atomic.set result_atomic result;
        Atomic.set request_atomic None;

        (* Reset indicator state for next iteration *)
        Bars.reset_indicators bars;
        worker_loop ()
    in
    worker_loop ()

  (* Objective function that communicates with worker domain *)
  let f strategy vars request_atomic result_atomic iteration_counter (l : float array) _grad =
    let iteration = Atomic.fetch_and_add iteration_counter 1 in
    Eio.traceln "=== OPTIMIZATION ITERATION %d ===" iteration;
    Eio.traceln "Parameters: %a" (Array.pp Float.pp) l;

    (* Create work request *)
    let work = { params = Array.copy l; strategy; vars } in

    (* Reset result and submit work *)
    Atomic.set result_atomic (Ok None);
    Atomic.set request_atomic (Some work);

    (* Wait for result *)
    let rec wait_for_result () =
      match Atomic.get result_atomic with
      | Ok None ->
        Domain.cpu_relax ();
        wait_for_result ()
      | Ok (Some result) ->
        Eio.traceln "Iteration %d result: %f" iteration result;
        result
      | Error err ->
        (* Return 0.0 for errors instead of crashing optimization *)
        Eio.traceln "=== ITERATION %d ERROR ===" iteration;
        Eio.traceln "Parameters tried: %a" (Array.pp Float.pp) l;
        Eio.traceln "Error: %a" Error.pp err;
        0.0
    in
    wait_for_result ()
end

let opt_atomic bars (options : Options.t) mutices (strategy : Gadt_strategy.t) =
  let ( let* ) = Result.( let* ) in
  Eio.traceln "=== ATOMIC OPTIMIZATION DEBUG START ===";
  Eio.traceln "Strategy name: %s" strategy.name;

  let buy_vars = Subst.collect_variables strategy.buy_trigger in
  let sell_vars = Subst.collect_variables strategy.sell_trigger in
  let vars = buy_vars @ sell_vars |> Array.of_list in

  Eio.traceln "--- COLLECTED VARIABLES ---";
  let len = Array.length vars in
  Eio.traceln "Total unique variables: %d" len;

  Eio.traceln "Buy trigger: %a" Gadt.pp strategy.buy_trigger;
  Eio.traceln "Sell trigger: %a" Gadt.pp strategy.sell_trigger;

  (* Create atomic communication channels *)
  let work_request_atomic = Atomic.make None in
  let work_result_atomic = Atomic.make (Ok None) in
  let shutdown_atomic = Atomic.make false in
  let iteration_counter = Atomic.make 0 in

  (* Log the options being used for optimization *)
  Eio.traceln "Optimization options:";
  Eio.traceln "  start tick: %d" options.flags.start;
  Eio.traceln "  runtype: %a" Longleaf_core.Runtype.pp options.flags.runtype;

  (* Create the worker in a separate domain with its own Eio runtime *)
  (* This is necessary because NLopt blocks the main Eio event loop *)
  Eio.traceln "Starting worker in separate domain";
  let worker_domain = Domain.spawn (fun () ->
    (* Enable backtraces in the worker domain too *)
    Printexc.record_backtrace true;
    (* Run Eio in the worker domain *)
    Eio_main.run (fun env ->
      (* Create a switch for the worker *)
      Eio.Switch.run (fun sw ->
        let worker_options = { options with eio_env = env; switch = sw } in
        Worker.top work_request_atomic work_result_atomic shutdown_atomic bars
          worker_options mutices ()
      )
    )
  ) in
  Eio.traceln "Worker domain spawned";
  (* Give worker time to start *)
  Unix.sleepf 0.1;

  let opt = Nlopt.create Nlopt.isres len in
  (* Set conservative bounds that work for most indicator periods and thresholds *)
  (* Lower bound: 5 avoids very unstable indicators and index out of bounds errors *)
  Nlopt.set_lower_bounds opt @@ Array.init len (fun _ -> 5.0);
  Nlopt.set_upper_bounds opt @@ Array.init len (fun _ -> 100.0);
  Nlopt.set_maxeval opt 30000;  (* Reduced to debug state/bars reset issues *)
  (* Nlopt.set_population opt (len * 10); *)
  Nlopt.set_max_objective opt
    (Worker.f strategy vars work_request_atomic work_result_atomic iteration_counter);
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

  Atomic.set shutdown_atomic true;
  Result.return fopt
