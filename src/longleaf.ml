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

let runtype_target_check ~runtype ~target : unit =
  match target with
  | Some _ -> (
      match runtype with
      | Options.Runtype.Backtest -> ()
      | _ ->
          Eio.traceln "Must be in a backtest if we have a specified target.";
          exit 1)
  | None -> ()

let save_received_check ~runtype ~save_received : unit =
  if save_received then
    match runtype with
    | Options.Runtype.Live | Options.Runtype.Paper -> ()
    | _ ->
        Eio.traceln "Must be live or paper to save received data.";
        exit 1

let top ~runtype ~preload ~stacktrace ~no_gui ~target ~save_received eio_env =
  runtype_target_check ~runtype ~target;
  save_received_check ~runtype ~save_received;
  if stacktrace then Printexc.record_backtrace true;
  let longleaf_env = Environment.make () in
  if Options.Runtype.is_manual runtype then (
    Manual.top eio_env longleaf_env;
    exit 0);
  Util.yojson_safe stacktrace @@ fun () ->
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  (* let shutdown_mutex = LongleafMutex.shutdown_mutex in *)
  (* let data_mutex = LongleafMutex.data_mutex in *)
  let mutices : Gui.mutices =
    {
      shutdown_mutex = LongleafMutex.shutdown_mutex;
      data_mutex = LongleafMutex.data_mutex;
      orders_mutex = LongleafMutex.orders_mutex;
      symbols_mutex = LongleafMutex.symbols_mutex;
    }
  in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    let module Context : Run.RUN_CONTEXT = struct
      let eio_env = eio_env
      let longleaf_env = longleaf_env
      let switch = switch
      let preload = preload
      let target = target
    end in
    let module Run = Run.DoubleTop.Make (LongleafMutex) (Context) in
    Run.top runtype
  in
  let run_server () =
    if no_gui then ()
    else
      Eio.Domain_manager.run domain_manager @@ fun () ->
      Gui.top ~mutices eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
