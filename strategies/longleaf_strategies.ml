module Run_options = Backend_intf.Run_options
module Run_context = Backend_intf.Run_context
module Collections = Ticker_collections

let run_options : Run_options.t =
  {
    symbols = Collections.sp100;
    tick = 600.0;
    overnight = true;
    resume_after_liquidate = true;
    indicators_config : Indicators.Config.t = { fft = false };
    dropout = false;
    randomized_backtest_length = 1000;
  }

let run_generic ~context (module Strat : Strategy.BUILDER) =
  Eio.traceln "@[Starting Doubletop@]@.";
  let module Backend = (val Backend.make run_options context) in
  let module S = Strat (Backend) in
  Eio.traceln "Applied strategy functor to backend, running.";
  let res = S.run () in
  Backend.shutdown ();
  res

module DoubleTop = struct
  let top context = run_generic ~context (module Double_top.DoubleTop)
end

module LowBall = struct
  let top context =
    run_generic ~context (module Buy_low_bollinger.BuyLowBollinger)
end

module Listener = struct
  let top context = run_generic ~context (module Listener.Make)
end

module BuyAndHold = struct
  let top context = run_generic ~context (module Buy_and_hold.Make)
end

module Challenge1 = struct
  let top context = run_generic ~context (module Challenge1.Make)
end

module Scalper = struct
  let top context = run_generic ~context (module Scalper.Make)
end

module Template_example = struct
  let top context = run_generic ~context (module Template_example.Make)
end

module Template_example2 = struct
  let top context = run_generic ~context (module Template_example2.Make)
end

module LowBall2 = struct
  let top context = run_generic ~context (module Lowboll2.Make)
end

module Crossover = struct
  let top context = run_generic ~context (module Crossover.Make)
end

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
[@@deriving show, eq]

let of_string_res x =
  let x = String.uncapitalize_ascii x in
  match x with
  | "buyandhold" | "buyhold" -> Ok BuyAndHold
  | "listener" | "listen" -> Ok Listener
  | "doubletop" -> Ok DoubleTop
  | "lowball" | "lowboll" -> Ok LowBoll
  | "lowball2" | "lowboll2" -> Ok LowBoll2
  | "challenge1" -> Ok Challenge1
  | "scalper" -> Ok Scalper
  | "template_example" -> Ok TemplateExample
  | "template_example2" -> Ok TemplateExample2
  | "crossover" -> Ok Crossover
  | _ -> Error (`Msg "Expected a valid strategy")

let conv = Cmdliner.Arg.conv (of_string_res, pp)

let run_strat (context : t Run_context.t) =
  match context.strategy with
  | BuyAndHold -> BuyAndHold.top context
  | Listener -> Listener.top context
  | DoubleTop -> DoubleTop.top context
  | LowBoll -> LowBall.top context
  | LowBoll2 -> LowBall2.top context
  | Challenge1 -> Challenge1.top context
  | Scalper -> Scalper.top context
  | TemplateExample -> Template_example.top context
  | TemplateExample2 -> Template_example2.top context
  | Crossover -> Crossover.top context

type multitest = { mean : float; min : float; max : float; std : float }
[@@deriving show]

let run (context : t Run_context.t) =
  match context.runtype with
  | Live | Paper | Backtest | Manual | Montecarlo | RandomSliceBacktest ->
      run_strat context
  | Multitest | MultiMontecarlo | MultiRandomSliceBacktest ->
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
