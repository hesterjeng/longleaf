module DoubleTopRun (Input : sig
  val eio_env : Eio_unix.Stdenv.base
  val longleaf_env : Environment.t
  val switch : Eio.Switch.t
end) (LongleafMutex : Backend.LONGLEAF_MUTEX)
=
struct
  module Ingredients : Backend.BACKEND_INPUT = struct
    module LongleafMutex = LongleafMutex

    let eio_env = Input.eio_env
    let longleaf_env = Input.longleaf_env
    let switch = Input.switch

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
  end

  module Backtesting () : Backend.S = Backend.Backtesting (Ingredients)

  module Alpaca () : Backend.S =
    Backend.Alpaca
      (Ingredients)
      (Ticker.Make (struct
        let time = 60.0
      end))

  let top backtesting =
    let backend =
      if backtesting then (module Backtesting () : Backend.S)
      else (module Alpaca () : Backend.S)
    in
    let module Backend = (val backend) in
    let module Strategy = Double_top.DoubleTop (Backend) in
    let res = Strategy.run () in
    Backend.shutdown ();
    Eio.traceln "Result: %s" res;
    ()
end
