module Collections = Ticker_collections

(** Type of strategies that have been defined. To add a new strategy, you must
    first add a corresponding variant to this type. Afterwards, you must add a
    handler for your strategy in the strats value below. *)
type t =
  | BuyAndHold
  | Listener
  | DoubleTop
  | LowBoll
  | LowBoll2
  | Challenge1
  | Scalper
  | TemplateExample
  | TemplateExample2
  | Crossover
  | SpyTrader
  | SlowCrossover
  | ConfirmedCrossover
  | ThrowingCrossover
  | LiberatedCrossover
  | Monaspa
  | Channel
  | Astar
  | Astarexample
  | RsiMeanReversion
  | MacdBollingerMomentum
  | VolumeBreakout
  | AdaptiveMomentumRegime
  | SimpleAdaptiveRegime
  | CandlestickPatterns
[@@deriving show, eq, yojson, variants]

let all = List.map fst Variants.descriptions

(** Add a handler for your strategy here, imitating the styles of the others.
    There must be a handler or your strategy will not work. *)
let strats : (t * (Options.t -> (_, _) result)) list =
  let ( --> ) x y = (x, Strategy.run y) in
  [
    (* BuyAndHold --> (module Buy_and_hold.Make); *)
    (* Listener --> (module Listener.Make); *)
    (* Monaspa --> (module Monaspa.Make); *)
    (* DoubleTop --> (module Double_top.DoubleTop); *)
    (* LowBoll --> (module Buy_low_bollinger.BuyLowBollinger); *)
    (* LowBoll2 --> (module Lowboll2.Make); *)
    (* Challenge1 --> (module Challenge1.Make); *)
    (* Scalper --> (module Scalper.Make); *)
    (* TemplateExample --> (module Template_example.Make); *)
    (* TemplateExample2 --> (module Template_example2.Make); *)
    (* Crossover --> (module Crossover.Make); *)
    (* SlowCrossover --> (module Slow_crossover.Make); *)
    (* ConfirmedCrossover --> (module Confirmed_crossover.Make); *)
    (* ThrowingCrossover --> (module Throwing_crossover.Make); *)
    (* LiberatedCrossover --> (module Liberated_crossover.Make); *)
    (* Channel --> (module Channel.Make); *)
    (* SpyTrader --> (module Spytrader.Make); *)
    Astarexample --> (module Astar_example.Make) (* (val Astar_example.m) *);
    RsiMeanReversion --> (module Rsi_mean_reversion.Make);
    MacdBollingerMomentum --> (module Macd_bollinger_momentum.Make);
    VolumeBreakout --> (module Volume_breakout.Make);
    AdaptiveMomentumRegime --> (module Adaptive_momentum_regime.Make);
    SimpleAdaptiveRegime --> (module Simple_adaptive_regime.Make);
    CandlestickPatterns --> (module Candlestick_patterns.Make);
  ]

(** Function for Cmdliner use. *)
let of_string_res x =
  let j = `List [ `String x ] in
  try Result.return @@ t_of_yojson j with
  | _ ->
    Result.fail
    @@ `Msg
         (Format.asprintf
            "@[Unknown strategy selected: %s@]@.@[Valid options are: %a@]@." x
            (List.pp String.pp) all)

(** Based on the context, select and run the strategy. *)
let run_strat_ (context : Options.t) strategy =
  let ( let* ) = Result.( let* ) in
  let* strategy = of_string_res strategy in
  let f = List.Assoc.get ~eq:equal strategy strats in
  let* strat =
    match f with
    | None -> Error.fatal "Unable to find strategy implementation"
    | Some f -> Result.return f
  in
  let* res = strat context in
  Result.return res

let run_strat context =
  match run_strat_ context context.flags.strategy_arg with
  | Ok x -> x
  | Error e -> Error.raise e

(** Function for Cmdliner use. *)
let conv = Cmdliner.Arg.conv (of_string_res, pp)

type multitest = { mean : float; min : float; max : float; std : float }
[@@deriving show]
(** Track some statistics if we are doing multiple backtests. *)

(** Top level function for running strategies based on a context.*)
let run (context : Options.t) =
  (* let strategy = of_string_res context.flags.strategy_arg in *)
  match context.flags.runtype with
  | AstarSearch ->
    (* Eio.traceln "Loading context..."; *)
    (* let context = Options.Context.load context in *)
    (* Eio.traceln "Loading indicators..."; *)
    (* let preload = Options.Preload.bars context.preload in *)
    (* let target = Options.Preload.bars context.target in *)
    (* Indicators.precompute preload target; *)
    (* assert (Options.Preload.is_loaded context.preload); *)
    (* assert (Options.Preload.is_loaded context.target); *)
    Eio.traceln "Running A*...";
    (* let context = { context with indicator_type = Precomputed } in *)
    let res = Astar_run.top context in
    Eio.traceln "A* completed: %a" (Option.pp Astar_run.StrategySearch.pp) res;
    0.0
  | Live
  | Paper
  | Backtest
  | Manual
  | Montecarlo
  | RandomSliceBacktest
  | RandomTickerBacktest ->
    run_strat context
  | Multitest
  | MultiMontecarlo
  | MultiRandomSliceBacktest
  | MultiRandomTickerBacktest ->
    let init = Array.make 30 () in
    let res = Array.map (fun _ -> run_strat context) init in
    Array.sort Float.compare res;
    let mean = Owl_stats.mean res in
    let std = Owl_stats.std res in
    let min, max = Owl_stats.minmax res in
    let result = { mean; min; max; std } in
    Eio.traceln "@[%a@]@.@[%a@]@." pp_multitest result (Array.pp Float.pp) res;
    let histogram = Owl_stats.histogram (`N 10) res in
    let percent_profitable =
      Array.filter (fun x -> x >=. 100000.0) res |> Array.length |> Float.of_int
      |> fun f -> f /. (Float.of_int @@ Array.length res)
    in
    let percent_great =
      Array.filter (fun x -> x >=. 110000.0) res |> Array.length |> Float.of_int
      |> fun f -> f /. (Float.of_int @@ Array.length res)
    in
    Eio.traceln "@[percent profitable: %f@]@." percent_profitable;
    Eio.traceln "@[percent great: %f@]@." percent_great;
    let normalised_histogram = Owl_stats.normalise histogram in
    Eio.traceln "@[%a@]@." Owl_stats.pp_hist histogram;
    Eio.traceln "@[%a@]@." Owl_stats.pp_hist normalised_histogram;
    0.0

module Run = struct
  open Longleaf_lib

  let mk_options switch eio_env flags target : Options.t =
    let longleaf_env = Util.Environment.make () in
    let mutices = Server.Longleaf_mutex.create () in
    {
      symbols = Collections.sp100;
      eio_env;
      longleaf_env;
      switch;
      flags;
      tick = 600.0;
      indicators_config = Indicators.Config.default;
      target;
      mutices;
    }

  let run_server eio_env (flags : Options.CLI.t) mutices () =
    let domain_manager = Eio.Stdenv.domain_mgr eio_env in
    match flags.no_gui with
    | true -> ()
    | false ->
      Eio.Domain_manager.run domain_manager @@ fun () ->
      Server.top ~mutices eio_env

  let run_strategy eio_env flags target () =
    let domain_manager = Eio.Stdenv.domain_mgr eio_env in
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    (* Load target bars with eio_env if needed *)
    let loaded_target =
      match target with
      | Target.File _ ->
        let bars = Target.load_bars ~eio_env target in
        Target.Loaded bars
      | Target.Download
      | Target.Loaded _ ->
        target
    in
    let options = mk_options switch eio_env flags loaded_target in
    let _res = run options in
    ()

  let top (flags : Options.CLI.t) target =
    Eio_main.run @@ fun eio_env ->
    let mutices = Server.Longleaf_mutex.create () in
    let run_strategy = run_strategy eio_env flags target in
    let run_data_server = run_server eio_env flags mutices in
    let _ = Eio.Fiber.both run_strategy run_data_server in
    ()
end
