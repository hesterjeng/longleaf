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
    Bars.fold bars (Ok ()) @@ fun symbol data acc ->
    let* _ = acc in
    (* Compute standard indicators *)
    let* () =
      Result.fold_l
        (fun _ indicator ->
          match indicator with
          | Tacaml ind ->
            let* () =
              try Talib_binding.calculate ?i ind data with
              | e ->
                Eio.traceln "===== SEQUENTIAL INDICATOR ERROR =====";
                Eio.traceln "Symbol: %a" Longleaf_core.Instrument.pp symbol;
                Eio.traceln "Indicator: %a" Tacaml.pp ind;
                Eio.traceln "Data size: %d" (Bars.Data.size data);
                Eio.traceln "Exception: %s" (Printexc.to_string e);
                Eio.traceln "======================================";
                Error.fatal @@
                  Format.asprintf "Symbol %a, Indicator %a: %s"
                    Longleaf_core.Instrument.pp symbol
                    Tacaml.pp ind
                    (Printexc.to_string e)
            in
            Result.return ())
        () indicators
    in
    Result.return ()

  let compute_linear = compute

  let compute_parallel ?i ?pool eio_env (config : Config.t) (bars : Bars.t) =
    let start_total = Unix.gettimeofday () in

    let indicators = List.map tacaml config.tacaml_indicators in
    if config.print_tick_arg then
      Eio.traceln "Starting indicator computation for... %a" (List.pp pp)
        indicators;

    (* Check if we should use parallel computation *)
    let result =
      (* Pre-extract all symbol-data pairs to avoid hashtable operations in domains *)
      let symbol_data_pairs =
        Bars.fold bars [] (fun symbol data acc -> (symbol, data) :: acc)
      in

      let compute_for_symbol_data (symbol, data) =
        let ( let* ) = Result.( let* ) in
        (* Compute all indicators for this symbol using pre-extracted data *)
        Result.fold_l
          (fun _ indicator ->
            match indicator with
            | Tacaml ind ->
              let* () =
                try Talib_binding.calculate ?i ind data with
                | e ->
                  Eio.traceln "===== PARALLEL INDICATOR ERROR =====";
                  Eio.traceln "Symbol: %a" Longleaf_core.Instrument.pp symbol;
                  Eio.traceln "Indicator: %a" Tacaml.pp ind;
                  Eio.traceln "Data size: %d" (Bars.Data.size data);
                  Eio.traceln "Exception: %s" (Printexc.to_string e);
                  Eio.traceln "====================================";
                  Error.fatal @@
                    Format.asprintf "Symbol %a, Indicator %a: %s"
                      Longleaf_core.Instrument.pp symbol
                      Tacaml.pp ind
                      (Printexc.to_string e)
              in
              Result.return ())
          () indicators
      in

      let clock = Eio.Stdenv.clock eio_env in

      (* Use provided pool or create a temporary one *)
      match pool with
      | Some p ->
          (* Reuse existing pool - no io_uring allocation! *)
          Util.Work_pool.Work_pool.parallel_map ~pool:p ~clock
            ~log_performance:config.print_tick_arg ~f:compute_for_symbol_data symbol_data_pairs
          |> Result.map_l Fun.id
          |> Result.map @@ fun _ -> ()
      | None ->
          (* Create temporary pool only for initialization *)
          let domain_mgr = Eio.Stdenv.domain_mgr eio_env in
          let domain_count = max 1 (Domain.recommended_domain_count () - 1) in
          Eio.Switch.run (fun sw ->
              let pool = Eio.Executor_pool.create ~sw domain_mgr ~domain_count in
              Util.Work_pool.Work_pool.parallel_map ~pool ~clock
                ~log_performance:config.print_tick_arg ~f:compute_for_symbol_data symbol_data_pairs
              |> Result.map_l Fun.id
              |> Result.map @@ fun _ -> ())
    in
    let end_total = Unix.gettimeofday () in
    if config.print_tick_arg then
      Eio.traceln "%a Total indicator computation took %.3fs"
        (Result.pp' Format.cut Error.pp)
        result (end_total -. start_total);
    result

  let compute_all ?pool eio_env (config : Config.t) bars =
    match config.compute_all_parallel with
    | true -> compute_parallel ?pool eio_env config bars
    | false -> compute config bars

  let compute_single ?pool i eio_env (config : Config.t) bars =
    match config.compute_live with
    | true -> compute_parallel ~i ?pool eio_env config bars
    | false -> Result.return ()
end

(* let top (state : State.t) = *)
(*   let indicator_config = Stat *)
