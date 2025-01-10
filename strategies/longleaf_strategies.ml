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
    }

  let top runtype context =
    run_generic ~runtype ~context ~run_options (module Buy_and_hold.Make)
end
