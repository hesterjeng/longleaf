module Collections = Ticker_collections
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
  | ModeE
[@@deriving show, eq, yojson, variants]

let all = List.map fst Variants.descriptions

(** Add a handler for your strategy here, imitating the styles of the others.
    There must be a handler or your strategy will not work. *)
let strats :
    (t
    * (Bars.t option -> Options.t -> Longleaf_state.Mutex.t -> (_, _) result))
    list =
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
    (* Astarexample --> (module Astar_example.Make) (\* (val Astar_example.m) *\); *)
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
let run_strat_ bars (context : Options.t) mutices =
  let ( let* ) = Result.( let* ) in
  let* strategy = of_string_res context.flags.strategy_arg in
  let f = List.Assoc.get ~eq:equal strategy strats in
  let* strat =
    match f with
    | None -> Error.fatal "Unable to find strategy implementation"
    | Some f -> Result.return f
  in
  let* res = strat bars context mutices in
  Result.return res

let run_strat bars (context : Options.t) mutices =
  match context.flags.strategy_arg with
  | "E0" -> Enumerate.top bars context
  | _ -> (
    match run_strat_ bars context mutices with
    | Ok x -> x
    | Error e ->
      Eio.traceln "longleaf_strateies.ml: %a" Error.pp e;
      Error.raise e)

(** Function for Cmdliner use. *)
let conv = Cmdliner.Arg.conv (of_string_res, pp)

type multitest = { mean : float; min : float; max : float; std : float }
[@@deriving show]
(** Track some statistics if we are doing multiple backtests. *)

(** Top level function for running strategies based on a context.*)
let run bars (context : Options.t) mutices =
  (* let strategy = of_string_res context.flags.strategy_arg in *)
  match context.flags.runtype with
  | Live
  | Paper
  | Backtest
  | Manual
  | Montecarlo
  | RandomSliceBacktest
  | RandomTickerBacktest ->
    run_strat bars context mutices
  | Multitest
  | MultiMontecarlo
  | MultiRandomSliceBacktest
  | MultiRandomTickerBacktest ->
    let init = Array.make 30 () in
    let res = Array.map (fun _ -> run_strat bars context mutices) init in
    Array.sort Float.compare res;
    (* let mean = Owl_stats.mean res in *)
    (* let std = Owl_stats.std res in *)
    (* let min, max = Owl_stats.minmax res in *)
    (* let result = { mean; min; max; std } in *)
    (* Eio.traceln "@[%a@]@.@[%a@]@." pp_multitest result (Array.pp Float.pp) res; *)
    (* let histogram = Owl_stats.histogram (`N 10) res in *)
    (* let percent_profitable = *)
    (*   Array.filter (fun x -> x >=. 100000.0) res |> Array.length |> Float.of_int *)
    (*   |> fun f -> f /. (Float.of_int @@ Array.length res) *)
    (* in *)
    (* let percent_great = *)
    (*   Array.filter (fun x -> x >=. 110000.0) res |> Array.length |> Float.of_int *)
    (*   |> fun f -> f /. (Float.of_int @@ Array.length res) *)
    (* in *)
    (* Eio.traceln "@[percent profitable: %f@]@." percent_profitable; *)
    (* Eio.traceln "@[percent great: %f@]@." percent_great; *)
    (* let normalised_histogram = Owl_stats.normalise histogram in *)
    (* Eio.traceln "@[%a@]@." Owl_stats.pp_hist histogram; *)
    (* Eio.traceln "@[%a@]@." Owl_stats.pp_hist normalised_histogram; *)
    0.0

module Run = struct
  module Target = Longleaf_core.Target

  let run_server eio_env (flags : Options.CLI.t) mutices () =
    match flags.no_gui with
    | true -> ()
    | false -> Longleaf_server.Server.top ~mutices eio_env

  let run_strategy eio_env flags target mutices () =
    (* Load target bars with eio_env if needed *)
    Eio.Switch.run @@ fun sw ->
    let bars =
      match target with
      | Longleaf_core.Target.File s ->
        let bars = Bars.of_file ~eio_env s in
        bars
      | Download -> invalid_arg "Download bars NYI"
      (* | Loaded bars -> (target, bars) *)
    in
    let options = Strategy.mk_options sw eio_env flags target in
    run (Some bars) options mutices

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
      Eio.traceln "longleaf_strategies: strategy did not return before server";
      raise e

  let top (flags : Options.CLI.t) target =
    Eio_main.run @@ fun eio_env ->
    let mutices = Longleaf_state.Mutex.create [] in
    let res = server eio_env flags target mutices in
    res
end
