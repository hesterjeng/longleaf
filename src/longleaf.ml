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

module DoubleTopRun = struct
  let top ~eio_env ~longleaf_env =
    Eio.Switch.run @@ fun switch ->
    let module Common_eio_stuff = struct
      let switch = switch
      let longleaf_env = longleaf_env
      let eio_env = eio_env

      module LongleafMutex = Backend.LongleafMutex
    end in
    let module Backtesting = Backend.Backtesting (struct
      include Common_eio_stuff

      let bars =
        Yojson.Safe.from_file "data/download_forcing_Dardistan"
        |> Bars.t_of_yojson

      let symbols = Bars.tickers bars
    end) in
    let module Alpaca =
      Backend.Alpaca
        (struct
          include Common_eio_stuff

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
        end)
        (Ticker.Make (struct
          let time = 60.0
        end))
    in
    (* let module Backend = Alpaca in *)
    let module Backend = Backtesting in
    let module Strategy = Double_top.DoubleTop (Backend) in
    let res = Strategy.run () in
    Backend.shutdown ();
    Alpaca.shutdown ();
    Eio.traceln "Result: %s" res;
    ()
end

let top eio_env =
  Util.yojson_safe @@ fun () ->
  CalendarLib.Time_Zone.change (UTC_Plus (-5));
  let longleaf_env = Environment.make () in
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  let module Run = DoubleTopRun in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Run.top ~eio_env ~longleaf_env
  in
  let run_server () =
    let set_mutex = Run.false in
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Gui.top ~set_mutex eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
