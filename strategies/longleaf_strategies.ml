module Context = Options.Context
module Collections = Ticker_collections

let run_options context : Options.t =
  {
    symbols = Collections.sp100;
    tick = 600.0;
    overnight = true;
    resume_after_liquidate = true;
    indicators_config : Indicators.Config.t = { fft = false };
    dropout = false;
    randomized_backtest_length = 1000;
    context;
  }

let run_generic (module Strat : Strategy.BUILDER) context =
  Eio.traceln "@[Starting Doubletop@]@.";
  let options = run_options context in
  let module Backend = (val Backend.make options) in
  let module S = Strat (Backend) in
  Eio.traceln "Applied strategy functor to backend, running.";
  let res = S.run () in
  Backend.shutdown ();
  res

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
[@@deriving show, eq, yojson, variants]

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
  ]

let run_strat (context : Context.t) strategy =
  let f = List.Assoc.get ~eq:equal strategy strats in
  match f with
  | Some f -> f context
  | None ->
      invalid_arg
      @@ Format.asprintf "Did not find a strategy implementation for %a" pp
           strategy

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

let conv = Cmdliner.Arg.conv (of_string_res, pp)

type multitest = { mean : float; min : float; max : float; std : float }
[@@deriving show]

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
