module Ticker_collections = Ticker_collections
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
module Astar = Longleaf_util.Astar
module Error = Longleaf_core.Error
module Pmutex = Longleaf_util.Pmutex

(** GADT Strategy Registry All strategies are now defined as Gadt.strategy
    records. To add a new strategy, simply add it to this list. *)
let gadt_strategies : (string * Gadt.strategy) list =
  List.map (fun s -> (s.Gadt.name, s)) Gadt_examples.all_strategies

(** Get all available strategy names *)
let all_strategy_names = List.map fst gadt_strategies

(** Find a GADT strategy by name *)
let rec find_gadt_strategy name = function
  | [] -> None
  | (n, strategy) :: _ when String.equal n name -> Some strategy
  | _ :: rest -> find_gadt_strategy name rest

let find_gadt_strategy name = find_gadt_strategy name gadt_strategies

(** Function for Cmdliner use. *)
let of_string_res strategy_name =
  match find_gadt_strategy strategy_name with
  | Some _ -> Result.return strategy_name
  | None ->
    Result.fail
    @@ `Msg
         (Format.asprintf
            "@[Unknown strategy selected: %s@]@.@[Valid options are: %a@]@."
            strategy_name (List.pp String.pp) all_strategy_names)

(** Run a GADT strategy by name *)
(* let run_gadt_strategy strategy_name bars (context : Options.t) mutices = *)
(*   let ( let* ) = Result.( let* ) in *)
(*   let* strategy = *)
(*     match find_gadt_strategy strategy_name with *)
(*     | Some s -> Result.return s *)
(*     | None -> Error.fatal ("Unknown GADT strategy: " ^ strategy_name) *)
(*   in *)
(*   Gadt.run bars context mutices strategy *)

(** Based on the context, select and run the strategy. *)
(* let run_strat_ bars (context : Options.t) mutices = *)
(*   let ( let* ) = Result.( let* ) in *)
(*   let* strategy_name = of_string_res context.flags.strategy_arg in *)
(*   run_gadt_strategy strategy_name bars context mutices *)

(* let run_strat bars (context : Options.t) mutices = *)
(*   match context.flags.strategy_arg with *)
(*   | "E0" -> Enumerate.top bars context *)
(*   | _ -> ( *)
(*     match run_strat_ bars context mutices with *)
(*     | Ok x -> x *)
(*     | Error e -> *)
(*       Eio.traceln "longleaf_strategies.ml: %a" Error.pp e; *)
(*       Error.raise e) *)

(** Function for Cmdliner use. *)
let conv = Cmdliner.Arg.conv (of_string_res, String.pp)

type multitest = { mean : float; min : float; max : float; std : float }
[@@deriving show]
(** Track some statistics if we are doing multiple backtests. *)

module Run = struct
  module Target = Longleaf_core.Target

  let run_server eio_env (flags : Options.CLI.t) mutices () =
    match flags.no_gui with
    | true -> ()
    | false -> Longleaf_server.Server.top ~mutices eio_env

  let run_strategy eio_env flags target mutices () =
    (* Load target bars with eio_env if needed *)
    let ( let* ) = Result.( let* ) in
    Eio.Switch.run @@ fun sw ->
    let options = Strategy.mk_options sw eio_env flags target [] in
    let* bars =
      match target with
      | Longleaf_core.Target.File s ->
        let bars = Bars.of_file ~eio_env s in
        Result.return bars
      | Download ->
        let module TF = Longleaf_core.Trading_types.Timeframe in
        let module D = Longleaf_apis.Downloader in
        let request = D.previous_30_days (TF.Min 10) options.symbols in
        let* bars = D.download eio_env request (Some Tiingo) true in
        Eio.traceln "Returning bars from download...";
        Result.return bars
    in
    (* Use the strategy specified in flags instead of hardcoding *)
    let strategy_name = flags.strategy_arg in
    match find_gadt_strategy strategy_name with
    | Some strategy -> Gadt.run bars options mutices strategy
    | None -> Error.fatal "Unknown strategy selected"

  let server env flags target mutices =
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    let domain_count = max 1 (Domain.recommended_domain_count () - 1) in
    Eio.Switch.run @@ fun sw ->
    let pool = Eio.Executor_pool.create ~sw domain_mgr ~domain_count in
    let strat_result =
      Eio.Executor_pool.submit_fork ~sw ~weight:1.0 pool
      @@ run_strategy env flags target mutices
    in
    run_server env flags mutices ();
    match Eio.Promise.await strat_result with
    | Ok x -> x
    | Error e ->
      Eio.traceln "longleaf_strategies: strategy error";
      raise e

  let top (flags : Options.CLI.t) target =
    Eio_main.run @@ fun eio_env ->
    let mutices = Longleaf_state.Mutex.create [] in
    let res = server eio_env flags target mutices in
    res
end
