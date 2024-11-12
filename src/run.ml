module type RUN_DATA = sig
  val bars : Bars.t
  val symbols : string list
  val tick : float
end

module type RUN_CONTEXT = sig
  val eio_env : Eio_unix.Stdenv.base
  val longleaf_env : Environment.t
  val switch : Eio.Switch.t

  (* module StrategyBuilder : Strategies.STRAT_BUILDER *)
end

module type S = sig
  val top : bool -> unit
end

module Make
    (Data : RUN_DATA)
    (StrategyBuilder : Strategies.STRAT_BUILDER)
    (LongleafMutex : Backend.LONGLEAF_MUTEX)
    (Context : RUN_CONTEXT) : S = struct
  module Input = struct
    include Data
    include Context
  end

  module Backtesting () : Backend.S =
    Backend.Backtesting (Input) (LongleafMutex)

  module Alpaca () : Backend.S =
    Backend.Alpaca
      (Input)
      (Ticker.Make (struct
        let time = Input.tick
      end))
      (LongleafMutex)

  let top backtesting =
    let backend =
      if backtesting then (module Backtesting () : Backend.S)
      else (module Alpaca () : Backend.S)
    in
    let module Backend = (val backend) in
    let module Strategy = StrategyBuilder (Backend) in
    let res = Strategy.run () in
    Backend.shutdown ();
    Eio.traceln "Result: %s" res;
    ()
end

module DoubleTop = struct
  module Data : RUN_DATA = struct
    let bars =
      Yojson.Safe.from_file "data/download_forcing_Dardistan"
      |> Bars.t_of_yojson

    let symbols =
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
      ]

    let tick = 60.0
  end

  module Make = Make (Data) (Double_top.DoubleTop)
end
