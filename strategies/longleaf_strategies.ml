module Run_options = Backend_intf.Run_options
module Collections = Ticker_collections

let run_generic ~runtype ~context ~run_options (module Strat : Strategy.BUILDER)
    =
  Eio.traceln "@[Starting Doubletop@]@.";
  let options = run_options runtype in
  let module Backend = (val Backend.make options context) in
  let module S = Strat (Backend) in
  let res = S.run () in
  Backend.shutdown ();
  res

module DoubleTop = struct
  let run_options runtype : Run_options.t =
    {
      (* symbols = Collections.some_symbols; *)
      symbols = Collections.sp100;
      tick = 600.0;
      overnight = false;
      resume_after_liquidate = true;
      runtype;
      indicators_config : Indicators.Config.t = { fft = false };
      dropout = false;
      randomized_backtest_length = 1000;
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options (module Double_top.DoubleTop)
end

module LowBall = struct
  let run_options runtype : Run_options.t =
    {
      (* symbols = Collections.some_symbols; *)
      symbols = Collections.sp100;
      (* symbols = [ "AAPL" ]; *)
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
      runtype;
      indicators_config : Indicators.Config.t = { fft = false };
      dropout = false;
      randomized_backtest_length = 1000;
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options
      (module Buy_low_bollinger.BuyLowBollinger)
end

module Listener = struct
  let run_options runtype : Run_options.t =
    {
      symbols = Collections.some_symbols;
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
      runtype;
      indicators_config : Indicators.Config.t = { fft = false };
      dropout = false;
      randomized_backtest_length = 1000;
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options (module Listener.Make)
end

module BuyAndHold = struct
  let run_options runtype : Run_options.t =
    {
      symbols = [ "SPY" ];
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
      runtype;
      indicators_config : Indicators.Config.t = { fft = false };
      dropout = false;
      randomized_backtest_length = 1000;
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options (module Buy_and_hold.Make)
end

module Challenge1 = struct
  let run_options runtype : Run_options.t =
    {
      symbols = Collections.sp100_spy;
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
      runtype;
      indicators_config : Indicators.Config.t = { fft = false };
      dropout = false;
      randomized_backtest_length = 1000;
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options (module Challenge1.Make)
end

type t = BuyAndHold | Listener | DoubleTop | LowBoll | Challenge1
[@@deriving show, eq]

let of_string_res x =
  let x = String.uncapitalize_ascii x in
  match x with
  | "buyandhold" | "buyhold" -> Ok BuyAndHold
  | "listener" | "listen" -> Ok Listener
  | "doubletop" -> Ok DoubleTop
  | "lowball" | "lowboll" -> Ok LowBoll
  | "challenge1" -> Ok Challenge1
  | _ -> Error (`Msg "Expected a valid strategy")

let conv = Cmdliner.Arg.conv (of_string_res, pp)

let run_strat runtype context x =
  match x with
  | BuyAndHold -> BuyAndHold.top runtype context
  | Listener -> Listener.top runtype context
  | DoubleTop -> DoubleTop.top runtype context
  | LowBoll -> LowBall.top runtype context
  | Challenge1 -> Challenge1.top runtype context

type multitest = { mean : float; min : float; max : float; std : float }
[@@deriving show]

let run (runtype : Options.Runtype.t) context x =
  match runtype with
  | Live | Paper | Backtest | Manual | Montecarlo | RandomSliceBacktest ->
      run_strat runtype context x
  | Multitest | MultiMontecarlo | MultiRandomSliceBacktest ->
      let init = Array.make 10 () in
      let res = Array.map (fun _ -> run_strat runtype context x) init in
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
      Eio.traceln "@[percent profitable: %f@]@." percent_profitable;
      let normalised_histogram = Owl_stats.normalise histogram in
      Eio.traceln "@[%a@]@." Owl_stats.pp_hist histogram;
      Eio.traceln "@[%a@]@." Owl_stats.pp_hist normalised_histogram;
      0.0
