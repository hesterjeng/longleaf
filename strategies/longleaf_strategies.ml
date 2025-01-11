module Run_options = Backend.Run_options

let run_generic ~runtype ~context ~run_options (module Strat : Strategy.BUILDER)
    =
  Eio.traceln "@[Starting Doubletop@]@.";
  let options = run_options runtype in
  let module Backend = (val Backend.create_backend options context) in
  let module S = Strat (Backend) in
  let res = S.run () in
  Backend.shutdown ();
  res

module DoubleTop = struct
  let run_options runtype : Run_options.t =
    {
      symbols =
        [
          "NVDA";
          "TSLA";
          "AAPL";
          "MSFT";
          "NFLX";
          "META";
          "AMZN";
          "AMD";
          "AVGO";
          "ELV";
          "UNH";
          "MU";
          "V";
          "GOOG";
          "SMCI";
          "MSTR";
          "UBER";
          "LLY";
          "SPY";
        ];
      tick = 600.0;
      overnight = false;
      resume_after_liquidate = true;
      runtype;
      indicators_config : Indicators.Config.t = { fft = false };
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options (module Double_top.DoubleTop)
end

module LowBall = struct
  let run_options runtype : Run_options.t =
    {
      symbols =
        [
          "NVDA";
          "TSLA";
          "AAPL";
          "MSFT";
          "NFLX";
          "META";
          "AMZN";
          "AMD";
          "AVGO";
          "ELV";
          "UNH";
          "MU";
          "V";
          "GOOG";
          "SMCI";
          "MSTR";
          "UBER";
          "LLY";
        ];
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
      runtype;
      indicators_config : Indicators.Config.t = { fft = false };
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options
      (module Buy_low_bollinger.BuyLowBollinger)
end

module Listener = struct
  let run_options runtype : Run_options.t =
    {
      symbols =
        [
          "NVDA";
          "TSLA";
          "AAPL";
          "MSFT";
          "NFLX";
          "META";
          "AMZN";
          "AMD";
          "AVGO";
          "ELV";
          "UNH";
          "MU";
          "V";
          "GOOG";
          "SMCI";
          "MSTR";
          "UBER";
          "LLY";
        ];
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
      runtype;
      indicators_config : Indicators.Config.t = { fft = false };
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
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options (module Buy_and_hold.Make)
end

type t = BuyAndHold | Listener | DoubleTop | LowBoll [@@deriving show, eq]

let of_string_res x =
  let x = String.uncapitalize_ascii x in
  match x with
  | "buyandhold" | "buyhold" -> Ok BuyAndHold
  | "listener" | "listen" -> Ok Listener
  | "doubletop" -> Ok DoubleTop
  | "lowball" | "lowboll" -> Ok LowBoll
  | _ -> Error (`Msg "Expected a valid strategy")

let conv = Cmdliner.Arg.conv (of_string_res, pp)

let run runtype context x =
  match x with
  | BuyAndHold -> BuyAndHold.top runtype context
  | Listener -> Listener.top runtype context
  | DoubleTop -> DoubleTop.top runtype context
  | LowBoll -> LowBall.top runtype context
