module Log = (val Logs.src_log Logs.(Src.create "longleaf"))
module Util = Util
module Backend = Backend
module Environment = Environment
module Trading_types = Trading_types
module Market_data_api = Market_data_api
module Ticker = Ticker
module Time = Time
module Lots_of_words = Lots_of_words
module Bars = Bars
module LongleafMutex = Backend.LongleafMutex ()

module DoubleTopRun (Input : sig
  val eio_env : Eio_unix.Stdenv.base
  val longleaf_env : Environment.t
  val switch : Eio.Switch.t
end) =
struct
  module Ingredients : Backend.BACKEND_INPUT = struct
    module LongleafMutex = LongleafMutex

    let eio_env = Input.eio_env
    let longleaf_env = Input.longleaf_env
    let switch = Input.switch
    let bars = Bars.empty

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

  module Backtesting () = Backend.Backtesting (Ingredients)

  module Alpaca () =
    Backend.Alpaca
      (Ingredients)
      (Ticker.Make (struct
        let time = 60.0
      end))

  let top () =
    let module Backend = Backtesting () in
    let module Strategy = Double_top.DoubleTop (Backend) in
    let res = Strategy.run () in
    Backend.shutdown ();
    Eio.traceln "Result: %s" res;
    ()
end

let top eio_env =
  Util.yojson_safe @@ fun () ->
  CalendarLib.Time_Zone.change (UTC_Plus (-5));
  let longleaf_env = Environment.make () in
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  let shutdown_mutex = LongleafMutex.shutdown_mutex in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    let module Run = DoubleTopRun (struct
      let eio_env = eio_env
      let longleaf_env = longleaf_env
      let switch = switch
    end) in
    Run.top ()
  in
  let run_server () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Gui.top ~shutdown_mutex eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
