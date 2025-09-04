module Bars = Longleaf_bars
module Options = Longleaf_core.Options
module Error = Longleaf_core.Error
module Gadt = Longleaf_gadt.Gadt
module Gadt_examples = Longleaf_gadt.Gadt_examples
module Gadt_strategy = Longleaf_gadt.Gadt_strategy

(** GADT Strategy Registry All strategies are now defined as Gadt.strategy
    records. To add a new strategy, simply add it to this list. *)
let gadt_strategies : (string * Gadt_strategy.t) list =
  List.map (fun s -> (s.Gadt_strategy.name, s)) Gadt_examples.all_strategies

(** Get all available strategy names *)
let all_strategy_names = List.map fst gadt_strategies

(** Find a GADT strategy by name *)
let rec find_gadt_strategy name = function
  | [] -> None
  | (n, strategy) :: _ when String.equal n name -> Some strategy
  | _ :: rest -> find_gadt_strategy name rest

let find_gadt_strategy name =
  match name with
  | "random"
  | "Random"
  | "R0" ->
    Some (Gadt_strategy.random ())
  | _ -> find_gadt_strategy name gadt_strategies

module Run = struct
  module Target = Longleaf_core.Target

  let run_strategy eio_env flags target mutices () =
    (* Load target bars with eio_env if needed *)
    let ( let* ) = Result.( let* ) in
    Eio.Switch.run @@ fun sw ->
    let options = Longleaf_template.mk_options sw eio_env flags target [] in
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
    let* strategy =
      match find_gadt_strategy flags.strategy_arg with
      | Some strategy ->
        Result.return strategy
        (* Gadt.run bars options mutices strategy *)
        (* Gadt_atomic.opt_atomic bars options mutices strategy *)
      | None -> Error.fatal "Unknown strategy selected"
    in
    match flags.runtype with
    | Longleaf_core.Runtype.Live
    | Paper ->
      Eio.traceln "Using Gadt.run";
      Gadt_strategy.run bars options mutices strategy
    | _ ->
      Eio.traceln "Using Gadt_atomic.opt_atomic";
      Longleaf_gadt.Gadt_atomic.opt_atomic bars options mutices strategy

  let top (flags : Options.CLI.t) target =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    let domain_count = max 1 (Domain.recommended_domain_count () - 1) in
    let pool = Eio.Executor_pool.create ~sw domain_mgr ~domain_count in
    let mutices = Longleaf_state.Mutex.create [] in
    let strat_result =
      Eio.Executor_pool.submit_fork ~sw ~weight:1.0 pool
      @@ run_strategy env flags target mutices
    in
    match Eio.Promise.await strat_result with
    | Ok x ->
      Eio.traceln "longleaf_strategies: got result";
      x
    | Error e ->
      Eio.traceln "longleaf_strategies: strategy error";
      raise e
end
