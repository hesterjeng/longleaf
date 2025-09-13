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

exception OptimizationException

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
              let e = Printexc.to_string e in
              Error.fatal @@ "[error: gadt_atomic]" ^ e
          in
          Ok (Some res)
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
        Eio.traceln "C callback got error: %a" Error.pp err;
        raise OptimizationException
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
  Nlopt.set_maxeval opt 10;
  (* Nlopt.set_population opt (len * 10); *)
  Nlopt.set_max_objective opt
    (Worker.f strategy vars work_request_atomic work_result_atomic);
  let start =
    Array.init len (fun _ -> Float.random_range 2.0 100.0 Util.random_state)
  in
  Eio.traceln "Optimization start %a" (Array.pp Float.pp) start;
  let* res, xopt, fopt =
    try Result.return @@ Nlopt.optimize opt start with
    | OptimizationException -> Error.fatal "OptimizationException"
  in
  Eio.traceln "optimization res: %s" (Nlopt.string_of_result res);
  Eio.traceln "%a : %f" (Array.pp Float.pp) xopt fopt;
  Atomic.set shutdown_atomic true;
  Result.return fopt
