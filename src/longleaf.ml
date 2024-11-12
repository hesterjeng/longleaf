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

let top eio_env backtesting =
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
    Run.top backtesting
  in
  let run_server () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Gui.top ~shutdown_mutex eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
