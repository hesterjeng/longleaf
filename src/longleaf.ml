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
module Options = Options
module LongleafMutex = Backend.LongleafMutex ()

let top ~runtype ~stacktrace ~output eio_env =
  if stacktrace then Printexc.record_backtrace true;
  Util.yojson_safe stacktrace @@ fun () ->
  let longleaf_env = Environment.make () in
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  (* let shutdown_mutex = LongleafMutex.shutdown_mutex in *)
  (* let data_mutex = LongleafMutex.data_mutex in *)
  let mutices : Gui.mutices =
    {
      shutdown_mutex = LongleafMutex.shutdown_mutex;
      data_mutex = LongleafMutex.data_mutex;
    }
  in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    let module Context : Run.RUN_CONTEXT = struct
      let eio_env = eio_env
      let longleaf_env = longleaf_env
      let switch = switch
    end in
    let module Run = Run.DoubleTop.Make (LongleafMutex) (Context) in
    Run.top runtype
  in
  let run_server () =
    Eio.Domain_manager.run domain_manager @@ fun () -> Gui.top ~mutices eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
