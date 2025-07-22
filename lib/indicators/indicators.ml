(* module TA = Tacaml.F *)
module Config = Indicators_config
module Bars = Longleaf_bars
module Tacaml_conv = Tacaml_conv
module Talib_binding = Talib_binding
module Util = Longleaf_util

type t = Tacaml of Tacaml.t

let ta_lib_common = List.map (fun x -> Tacaml x) Talib_binding.common
let ta_lib_all = List.map (fun x -> Tacaml x) Tacaml.Defaults.all

let compute ?i (indicators : t list) (config : Config.t) (bars : Bars.t) =
  match config.compute_live with
  | false ->
    Eio.traceln "Precomputing indicators because of Indicator_config.t";
    let ( let* ) = Result.( let* ) in
    Bars.fold bars (Ok ()) @@ fun _ data acc ->
    let* _ = acc in
    (* Compute standard indicators *)
    let* () =
      Result.fold_l
        (fun _ indicator ->
          match indicator with
          | Tacaml ind ->
            let* () = Talib_binding.calculate ?i ind data in
            Result.return ())
        () indicators
    in
    Result.return ()
  | true ->
    Eio.traceln "Not precomputing indicators because of Indicator_config.t";
    Result.return ()

let initialize () =
  match Tacaml.initialize () with
  | Ok () -> ()
  | Error e ->
    Eio.traceln "Problem when initializing TA-Lib";
    invalid_arg e

let compute_all ?i ?eio_env (config : Config.t) (bars : Bars.t) =
  let start_total = Unix.gettimeofday () in
  Eio.traceln "Starting indicator computation...";

  (* Check if we should use parallel computation *)
  let result =
    match (eio_env, config.compute_live) with
    | Some env, false ->
      (* Pre-extract all symbol-data pairs to avoid hashtable operations in domains *)
      let symbol_data_pairs =
        Bars.fold bars [] (fun symbol data acc -> (symbol, data) :: acc)
      in

      let compute_for_symbol_data (_symbol, data) =
        let ( let* ) = Result.( let* ) in
        (* Compute all indicators for this symbol using pre-extracted data *)
        Result.fold_l
          (fun _ indicator ->
            match indicator with
            | Tacaml ind ->
              let* () = Talib_binding.calculate ?i ind data in
              Result.return ())
          () ta_lib_all
      in

      (* Use Work_pool for parallel processing with pre-extracted data *)
      let domain_mgr = Eio.Stdenv.domain_mgr env in
      let clock = Eio.Stdenv.clock env in
      let domain_count = max 1 (Domain.recommended_domain_count () - 1) in
      Eio.Switch.run (fun sw ->
          let pool = Eio.Executor_pool.create ~sw domain_mgr ~domain_count in
          let results =
            Util.Work_pool.Work_pool.parallel_map_result ~pool ~clock
              ~log_performance:true ~f:compute_for_symbol_data symbol_data_pairs
          in

          (* Check if any computation failed *)
          List.fold_left
            (fun acc result ->
              match (acc, result) with
              | Ok (), Ok (Ok ()) -> Ok ()
              | Ok (), Ok (Error e) -> Error e
              | Error e, _ -> Error e
              | _, Error exn -> Error (`FatalError (Printexc.to_string exn)))
            (Ok ()) results)
    | _, _ ->
      (* Sequential computation (original behavior) *)
      compute ?i ta_lib_all config bars
  in
  let end_total = Unix.gettimeofday () in
  Eio.traceln "Total indicator computation took %.3fs" (end_total -. start_total);
  result
