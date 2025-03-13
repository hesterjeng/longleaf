module Context = Options.Context
module Collections = Ticker_collections

(** Function for creating the options given a context, it has some sensible
    defaults. If you want to use other options, you may need to create your own
    Options.t value. *)
let run_options (context : Context.t) : Options.t =
  let symbols =
    match context.runtype with
    | RandomTickerBacktest | MultiRandomTickerBacktest ->
        let arr = Array.of_list Collections.sp100 in
        let eighty_percent =
          (Array.length arr |> Float.of_int) *. 0.8 |> Int.of_float
        in
        Owl_stats.choose arr eighty_percent |> Array.to_list
    | _ -> Collections.sp100
  in
  {
    symbols;
    tick = 600.0;
    overnight = true;
    resume_after_liquidate = true;
    indicators_config : Indicators.Config.t = { fft = false };
    dropout = false;
    randomized_backtest_length = 1000;
    context;
  }

(* let original_bars = ref None *)
(* let original_target = ref None *)

(* let check_bars options = *)
(*   match (!original_bars, !original_target) with *)
(*   | Some _, Some _ -> () *)
(*   | _ -> *)
(*       let bars, target = Backend.make_bars options in *)
(*       original_bars := Some bars; *)
(*       original_target := Some target; *)
(*       () *)

(** Helper function to reduce code duplication. *)
let run_generic ?(run_options = run_options) ?bars ?target
    (module Strat : Strategy.BUILDER) context =
  Eio.traceln "@[Starting Doubletop@]@.";
  let options = run_options context in
  (* let () = check_bars options in *)
  let module Backend = (val Backend.make options bars target) in
  let module S = Strat (Backend) in
  Eio.traceln "Applied strategy functor to backend, running.";
  let res = S.run () in
  Backend.shutdown ();
  res

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
[@@deriving show, eq, yojson, variants]

let all = List.map fst Variants.descriptions

(** Add a handler for your strategy here, imitating the styles of the others.
    There must be a handler or your strategy will not work. *)
let strats =
  let ( --> ) x y = (x, run_generic y) in
  [
    BuyAndHold --> (module Buy_and_hold.Make);
    Listener --> (module Listener.Make);
    DoubleTop --> (module Double_top.DoubleTop);
    LowBoll --> (module Buy_low_bollinger.BuyLowBollinger);
    LowBoll2 --> (module Lowboll2.Make);
    Challenge1 --> (module Challenge1.Make);
    Scalper --> (module Scalper.Make);
    TemplateExample --> (module Template_example.Make);
    TemplateExample2 --> (module Template_example2.Make);
    Crossover --> (module Crossover.Make);
    SlowCrossover --> (module Slow_crossover.Make);
    ConfirmedCrossover --> (module Confirmed_crossover.Make);
    ThrowingCrossover --> (module Throwing_crossover.Make);
    SpyTrader --> (module Spytrader.Make);
  ]

(** Based on the context, select and run the strategy. *)
let run_strat (context : Context.t) strategy =
  let f = List.Assoc.get ~eq:equal strategy strats in
  match f with
  | Some f -> f context
  | None ->
      invalid_arg
      @@ Format.asprintf "Did not find a strategy implementation for %a" pp
           strategy

(** Function for Cmdliner use. *)
let of_string_res x =
  let j = `List [ `String x ] in
  try Result.return @@ t_of_yojson j
  with _ ->
    let all = List.map fst Variants.descriptions in
    Result.fail
    @@ `Msg
         (Format.asprintf
            "@[Unknown runtype selected: %s@]@.@[Valid options are: %a@]@." x
            (List.pp String.pp) all)

(** Function for Cmdliner use. *)
let conv = Cmdliner.Arg.conv (of_string_res, pp)

type multitest = { mean : float; min : float; max : float; std : float }
[@@deriving show]
(** Track some statistics if we are doing multiple backtests. *)

(** Top level function for running strategies based on a context.*)
let run (context : Context.t) strategy =
  match context.runtype with
  | Live | Paper | Backtest | Manual | Montecarlo | RandomSliceBacktest
  | RandomTickerBacktest ->
      run_strat context strategy
  | Multitest | MultiMontecarlo | MultiRandomSliceBacktest
  | MultiRandomTickerBacktest ->
      let init = Array.make 30 () in
      let res = Array.map (fun _ -> run_strat context strategy) init in
      Array.sort Float.compare res;
      let mean = Owl_stats.mean res in
      let std = Owl_stats.std res in
      let min, max = Owl_stats.minmax res in
      let result = { mean; min; max; std } in
      Eio.traceln "@[%a@]@.@[%a@]@." pp_multitest result (Array.pp Float.pp) res;
      let histogram = Owl_stats.histogram (`N 10) res in
      let percent_profitable =
        Array.filter (fun x -> x >=. 100000.0) res
        |> Array.length |> Float.of_int
        |> fun f -> f /. (Float.of_int @@ Array.length res)
      in
      let percent_great =
        Array.filter (fun x -> x >=. 110000.0) res
        |> Array.length |> Float.of_int
        |> fun f -> f /. (Float.of_int @@ Array.length res)
      in
      Eio.traceln "@[percent profitable: %f@]@." percent_profitable;
      Eio.traceln "@[percent great: %f@]@." percent_great;
      let normalised_histogram = Owl_stats.normalise histogram in
      Eio.traceln "@[%a@]@." Owl_stats.pp_hist histogram;
      Eio.traceln "@[%a@]@." Owl_stats.pp_hist normalised_histogram;
      0.0
