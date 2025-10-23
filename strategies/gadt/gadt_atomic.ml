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
        if Atomic.get shutdown_atomic then ()
        else (
          (* No work, yield and try again *)
          Domain.cpu_relax ();
          worker_loop ())
      | Some work ->
        Eio.traceln "Worker domain processing request";
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
              | e ->
                (* Log error but return 0.0 instead of failing *)
                let e = Printexc.to_string e in
                Eio.traceln "Strategy execution error (returning 0.0): %s" e;
                Ok 0.0
            in
            Ok (Some res)
          with
          | e ->
            (* Catch any other errors during substitution/instantiation *)
            let error_msg = Printexc.to_string e in
            Eio.traceln "Optimization parameter error (returning 0.0): %s" error_msg;
            Ok (Some 0.0)
        in

        (* Store result and clear request *)
        Atomic.set result_atomic result;
        Atomic.set request_atomic None;
        worker_loop ()
    in
    worker_loop ()

  (* Objective function that communicates with worker domain *)
  let f strategy vars request_atomic result_atomic (l : float array) _grad =
    Eio.traceln "C callback requesting work from domain";

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
        Eio.traceln "C callback got result: %f" result;
        result
      | Error err ->
        (* Return 0.0 for errors instead of crashing optimization *)
        Eio.traceln "C callback got error (returning 0.0): %a" Error.pp err;
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

  (* Create the worker domain *)
  let worker =
    Worker.top work_request_atomic work_result_atomic shutdown_atomic bars
      options mutices
  in
  let domain_mgr = Eio.Stdenv.domain_mgr options.eio_env in
  let pool =
    Eio.Executor_pool.create ~sw:options.switch domain_mgr ~domain_count:2
  in
  Eio.traceln "Waiting on any fiber";
  ( Eio.Fiber.fork ~sw:options.switch @@ fun () ->
    Eio.Executor_pool.submit_exn ~weight:0.1 pool worker );
  Eio.traceln "Worker domain created";

  let opt = Nlopt.create Nlopt.isres len in
  Nlopt.set_lower_bounds opt @@ Array.init len (fun _ -> 2.0);
  Nlopt.set_upper_bounds opt @@ Array.init len (fun _ -> 100.0);
  Nlopt.set_maxeval opt 1000;  (* Increased from 10 to 1000 evaluations *)
  (* Nlopt.set_population opt (len * 10); *)
  Nlopt.set_max_objective opt
    (Worker.f strategy vars work_request_atomic work_result_atomic);
  let start =
    Array.init len (fun _ -> Float.random_range 2.0 100.0 Util.random_state)
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
