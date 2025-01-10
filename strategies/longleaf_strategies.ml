module Run_options = Backend.Run_options

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
    Eio.traceln "@[Starting Doubletop@]@.";
    let options = run_options runtype in
    let module Backend = (val Backend.create_backend options context) in
    let module S = Double_top.DoubleTop (Backend) in
    let res = S.run () in
    Backend.shutdown ();
    res
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
    Eio.traceln "@[Starting Lowball@]@.";
    let options = run_options runtype in
    let module Backend = (val Backend.create_backend options context) in
    let module S = Buy_low_bollinger.BuyLowBollinger (Backend) in
    Eio.traceln "@[Lowball running...@]@.";
    let res = S.run () in
    Backend.shutdown ();
    res
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
    Eio.traceln "@[Starting listener@]@.";
    let options = run_options runtype in
    let module Backend = (val Backend.create_backend options context) in
    let module S = Listener.Make (Backend) in
    let res = S.run () in
    Backend.shutdown ();
    res
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
    Eio.traceln "@[Starting BuyAndHold@]@.";
    let options = run_options runtype in
    let module Backend = (val Backend.create_backend options context) in
    let module S = Buy_and_hold.Make (Backend) in
    let res = S.run () in
    Backend.shutdown ();
    res
end
