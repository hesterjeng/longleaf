module Bars = Longleaf_bars
module Options = Longleaf_core.Options
module Error = Longleaf_core.Error
module Gadt = Longleaf_gadt.Gadt
module Gadt_examples = Longleaf_gadt.Gadt_examples
module Gadt_strategy = Longleaf_gadt.Gadt_strategy
module Battery_runner = Battery_runner

(** GADT Strategy Registry All strategies are now defined as Gadt.strategy
    records. To add a new strategy, simply add it to this list. *)
let gadt_strategies : unit -> (string * Gadt_strategy.t) list =
 fun () ->
  List.map (fun s -> (s.Gadt_strategy.name, s)) Gadt_examples.all_strategies

(** Get all available strategy names *)
let all_strategy_names () = List.map fst @@ gadt_strategies ()

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
  | _ -> find_gadt_strategy name @@ gadt_strategies ()

module Run = struct
  module Target = Longleaf_core.Target

  let run_battery eio_env executor_pool flags battery_name mutices () =
    let ( let* ) = Result.( let* ) in
    Eio.Switch.run @@ fun sw ->
    let options =
      Longleaf_template.mk_options sw eio_env executor_pool flags Target.Download []
    in
    let* battery =
      match Battery_runner.get_battery battery_name with
      | Some b -> Result.return b
      | None ->
        Error.fatal
          (Format.asprintf "Unknown battery: %s. Available: %a" battery_name
             (List.pp String.pp) Battery_runner.available_batteries)
    in
    let* strategy =
      match find_gadt_strategy flags.strategy_arg with
      | Some s -> Result.return s
      | None -> Error.fatal ("Unknown strategy: " ^ flags.strategy_arg)
    in
    let builder = Gadt_strategy.Builder.top strategy in
    let* result = Battery_runner.run builder options mutices battery in
    (* Print per-dataset results *)
    Eio.traceln "";
    Eio.traceln "=== Battery Results by Dataset ===";
    List.iter
      (fun (e : Longleaf_battery.Eval_result.t) ->
        let source_name =
          match e.source with
          | Target.File s -> Filename.basename s
          | _ -> Target.show e.source
        in
        match e.trade_stats with
        | Some ts ->
          Eio.traceln "  %s: Sharpe=%.3f  Return=%.2f%%  Drawdown=%.2f%%  Trades=%d"
            source_name ts.sharpe (e.stats.total_return *. 100.0)
            (e.stats.max_drawdown *. 100.0) ts.num_trades
        | None ->
          Eio.traceln "  %s: No trades" source_name)
      result.eval_results;
    (* Print summary *)
    Eio.traceln "";
    Eio.traceln "=== Summary ===";
    Eio.traceln "Periods evaluated: %d" (Longleaf_battery.num_periods result);
    Eio.traceln "Avg Sharpe: %.3f (std: %.3f)"
      (Longleaf_battery.avg_sharpe result) (Longleaf_battery.std_sharpe result);
    Eio.traceln "Avg Return: %.2f%% (std: %.2f%%)"
      (Longleaf_battery.avg_return result *. 100.0)
      (Longleaf_battery.std_return result *. 100.0);
    Eio.traceln "Worst Drawdown: %.2f%%"
      (Longleaf_battery.worst_drawdown result *. 100.0);
    Eio.traceln "Consistency: %.1f%%" (Longleaf_battery.consistency result *. 100.0);
    Result.return 0.0

  let run_strategy eio_env executor_pool flags target mutices () =
    (* Load target bars with eio_env if needed *)
    let ( let* ) = Result.( let* ) in
    Eio.Switch.run @@ fun sw ->
    let options =
      Longleaf_template.mk_options sw eio_env executor_pool flags target []
    in
    let* bars =
      match target with
      | Longleaf_core.Target.File s ->
        let bars = Bars.of_file ~eio_env s in
        let* () = Bars.validate_no_nan bars in
        Result.return bars
      | Download ->
        let module TF = Longleaf_core.Trading_types.Timeframe in
        let module D = Longleaf_apis.Downloader in
        (* Convert tick duration (in seconds) to timeframe *)
        let tick_minutes = int_of_float (options.tick /. 60.0) in
        let timeframe = TF.Min tick_minutes in
        Eio.traceln
          "Downloading 14 days of data with %d-minute intervals (tick=%.1fs)"
          tick_minutes options.tick;
        let request = D.previous_14_days timeframe options.symbols in
        let* bars = D.download eio_env request (Some Massive) true in
        Eio.traceln "Validating downloaded data...";
        let* () = Bars.validate_no_nan bars in
        Eio.traceln "Returning bars from download...";
        Result.return bars
      | BatteryName _ ->
        Error.fatal "BatteryName target should use Battery runtype"
    in
    (* Use the strategy specified in flags instead of hardcoding *)
    let* strategy =
      match find_gadt_strategy flags.strategy_arg with
      | Some strategy ->
        Result.return strategy
        (* Gadt.run bars options mutices strategy *)
        (* Gadt_atomic.opt_atomic bars options mutices strategy *)
      | None ->
        let msg =
          "longleaf_strategies.ml: Unknown strategy selected"
          ^ flags.strategy_arg
        in
        Error.fatal msg
    in
    match flags.runtype with
    | Longleaf_core.Runtype.Live
    | Paper ->
      Eio.traceln "Using Gadt.run";
      Gadt_strategy.run bars options mutices strategy
    | _ ->
      Eio.traceln "Using Gadt_atomic.opt_atomic";
      let res =
        Longleaf_gadt.Gadt_atomic.opt_atomic bars options mutices strategy
      in
      Eio.traceln "Done using Gadt_atomic.opt_atomic";
      res

  let top mutices env (flags : Options.CLI.t) target =
    let ( let* ) = Result.( let* ) in
    Eio.Switch.run @@ fun sw ->
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    let domain_count = max 1 (Domain.recommended_domain_count () - 1) in
    let pool = Eio.Executor_pool.create ~sw domain_mgr ~domain_count in
    let mutices =
      match mutices with
      | None -> Longleaf_state.Mutex.create []
      | Some m -> m
    in
    let handler =
      match flags.runtype, target with
      | Longleaf_core.Runtype.Battery, Target.BatteryName battery_name ->
        run_battery env pool flags battery_name mutices
      | Longleaf_core.Runtype.Battery, _ ->
        fun () -> Error.fatal "Battery runtype requires a battery name as target"
      | _, Target.BatteryName _ ->
        fun () -> Error.fatal "BatteryName target requires Battery runtype"
      | _ ->
        run_strategy env pool flags target mutices
    in
    let strat_result =
      Eio.Executor_pool.submit_fork ~sw ~weight:1.0 pool handler
    in
    Eio.traceln "longleaf_strategies: left fork";
    match Eio.Promise.await strat_result with
    | Ok x ->
      Eio.traceln "longleaf_strategies: got result";
      let* x = x in
      Result.return x
    | Error e ->
      let e = Printexc.to_string e in
      Eio.traceln "longleaf_strategies: strategy error";
      Error.fatal e
end
