(* module TA = Tacaml.F *)
module Config = Indicators_config
module Bars = Longleaf_bars
module Tacaml_conv = Tacaml_conv
module Talib_binding = Talib_binding
module Util = Longleaf_util

type t = Tacaml of Tacaml.t

let pp : t Format.printer =
 fun fmt (Tacaml x) -> Format.fprintf fmt "%a" Tacaml.pp x

let tacaml x = Tacaml x
let ta_lib_common = List.map (fun x -> Tacaml x) Talib_binding.common
let ta_lib_all = List.map (fun x -> Tacaml x) Tacaml.Defaults.all

let initialize () =
  match Tacaml.initialize () with
  | Ok () -> ()
  | Error e ->
    Eio.traceln "Problem when initializing TA-Lib";
    invalid_arg e

module Calc = struct
  let compute ?i (config : Config.t) (bars : Bars.t) =
    Eio.traceln "Precomputing indicators because of Indicator_config.t";
    let indicators = List.map tacaml config.tacaml_indicators in
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

  let compute_linear = compute

  let compute_parallel ?i eio_env (config : Config.t) (bars : Bars.t) =
    let start_total = Unix.gettimeofday () in

    let indicators = List.map tacaml config.tacaml_indicators in
    Eio.traceln "Starting indicator computation for... %a" (List.pp pp)
      indicators;
    (* Check if we should use parallel computation *)
    let result =
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
          () indicators
      in

      (* Use Work_pool for parallel processing with pre-extracted data *)
      let domain_mgr = Eio.Stdenv.domain_mgr eio_env in
      let clock = Eio.Stdenv.clock eio_env in
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
    in
    let end_total = Unix.gettimeofday () in
    Eio.traceln "Total indicator computation took %.3fs"
      (end_total -. start_total);
    result

  let compute_all eio_env (config : Config.t) bars =
    compute_parallel eio_env config bars

  let compute_single i eio_env (config : Config.t) bars =
    match config.compute_live with
    | true -> compute_parallel ~i eio_env config bars
    | false -> Result.return ()
end
