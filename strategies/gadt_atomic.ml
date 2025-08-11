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
  strategy : strategy;
  vars : (Uuidm.t * Type.shadow) array;
}

type work_result = Pending | Success of float | Error of string

let opt_atomic bars (options : Options.t) mutices (strategy : strategy) =
  Eio.traceln "=== ATOMIC OPTIMIZATION DEBUG START ===";
  Eio.traceln "Strategy name: %s" strategy.name;

  let buy_vars = Subst.collect_variables strategy.buy_trigger in
  let sell_vars = Subst.collect_variables strategy.sell_trigger in
  let vars = buy_vars @ sell_vars |> Array.of_list in

  Eio.traceln "--- COLLECTED VARIABLES ---";

  (* Eio.traceln "Buy trigger variables: %d" (List.length buy_vars); *)
  (* List.iteri *)
  (*   (fun i (id, Type.A ty) -> *)
  (*     let ty_str = *)
  (*       match ty with *)
  (*       | Type.Float -> "Float" *)
  (*       | Type.Int -> "Int" *)
  (*     in *)
  (*     Eio.traceln "  Buy[%d]: %s (%s)" i (Uuidm.to_string id) ty_str) *)
  (*   buy_vars; *)

  (* Eio.traceln "Sell trigger variables: %d" (List.length sell_vars); *)
  (* List.iteri *)
  (*   (fun i (id, Type.A ty) -> *)
  (*     let ty_str = *)
  (*       match ty with *)
  (*       | Type.Float -> "Float" *)
  (*       | Type.Int -> "Int" *)
  (*     in *)
  (*     Eio.traceln "  Sell[%d]: %s (%s)" i (Uuidm.to_string id) ty_str) *)
  (*   sell_vars; *)
  let len = Array.length vars in
  Eio.traceln "Total unique variables: %d" len;

  Eio.traceln "Buy trigger: %a" pp_expr strategy.buy_trigger;
  Eio.traceln "Sell trigger: %a" pp_expr strategy.buy_trigger;

  (* Create atomic communication channels *)
  let work_request_atomic = Atomic.make None in
  let work_result_atomic = Atomic.make Pending in

  (* Get domain manager and create worker *)
  let domain_mgr = Eio.Stdenv.domain_mgr options.eio_env in

  Eio.Switch.run @@ fun sw ->
  let pool = Eio.Executor_pool.create ~sw domain_mgr ~domain_count:1 in

  (* Start worker domain that waits for work *)
  let _worker_promise =
    Eio.Executor_pool.submit_fork ~sw ~weight:1.0 pool @@ fun () ->
    let rec worker_loop () =
      match Atomic.get work_request_atomic with
      | None ->
        (* No work, yield and try again *)
        Domain.cpu_relax ();
        worker_loop ()
      | Some work ->
        Eio.traceln "Worker domain processing request";
        let result =
          try
            let env =
              let open Subst in
              Array.foldi
                (fun env i (id, Type.A ty) ->
                  match ty with
                  | Type.Float ->
                    {
                      env with
                      float_map = Bindings.add id work.params.(i) env.float_map;
                    }
                  | Type.Int ->
                    let int_val = Int.of_float work.params.(i) in
                    { env with int_map = Bindings.add id int_val env.int_map })
                { float_map = Bindings.empty; int_map = Bindings.empty }
                work.vars
            in

            let instantiated_buy =
              Subst.instantiate env work.strategy.buy_trigger |> function
              | Ok x -> x
              | Error _ -> failwith "Instantiation error in buy trigger"
            in

            let instantiated_sell =
              Subst.instantiate env work.strategy.sell_trigger |> function
              | Ok x -> x
              | Error _ -> failwith "Instantiation error in sell trigger"
            in

            let instantiated_strategy =
              {
                work.strategy with
                buy_trigger = instantiated_buy;
                sell_trigger = instantiated_sell;
              }
            in

            let res =
              run bars options mutices instantiated_strategy |> function
              | Ok x -> x
              | Error _ -> failwith "Strategy execution error"
            in
            Success (Float.sub 1.0 res)
          with
          | e ->
            let s = Printexc.to_string e in
            Eio.traceln "Worker error: %s" s;
            Error s
        in

        (* Store result and clear request *)
        Atomic.set work_result_atomic result;
        Atomic.set work_request_atomic None;
        worker_loop ()
    in
    worker_loop ()
  in

  (* Objective function that communicates with worker domain *)
  let f (l : float array) _grad =
    Eio.traceln "C callback requesting work from domain";

    (* Create work request *)
    let work = { params = Array.copy l; strategy; vars } in

    (* Reset result and submit work *)
    Atomic.set work_result_atomic Pending;
    Atomic.set work_request_atomic (Some work);

    (* Wait for result *)
    let rec wait_for_result () =
      match Atomic.get work_result_atomic with
      | Pending ->
        Domain.cpu_relax ();
        wait_for_result ()
      | Success result ->
        Eio.traceln "C callback got result: %f" result;
        result
      | Error err ->
        Eio.traceln "C callback got error: %s" err;
        raise OptimizationException
    in
    wait_for_result ()
  in

  let opt = Nlopt.create Nlopt.neldermead len in
  Nlopt.set_lower_bounds opt @@ Array.init len (fun _ -> 1.0);
  Nlopt.set_upper_bounds opt @@ Array.init len (fun _ -> 100.0);
  Nlopt.set_maxeval opt 2000;
  (* Nlopt.set_population opt (len * 10); *)
  Nlopt.set_min_objective opt f;
  let start = Array.init len (fun _ -> 10.0) in
  Eio.traceln "Optimization start %a" (Array.pp Float.pp) start;
  let res, xopt, fopt = Nlopt.optimize opt start in
  Eio.traceln "optimization res: %s" (Nlopt.string_of_result res);
  Eio.traceln "%a : %f" (Array.pp Float.pp) xopt fopt;
  Result.return fopt
