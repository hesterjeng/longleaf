open Longleaf_lib

let runtype_target_check ~runtype ~target : unit =
  match target with
  | Some _ -> (
      match runtype with
      | Options.Runtype.Backtest | Multitest | Montecarlo -> ()
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

let top ~runtype ~preload ~stacktrace ~no_gui ~target ~save_received ~eio_env
    ~strategy_arg ~save_to_file =
  runtype_target_check ~runtype ~target;
  save_received_check ~runtype ~save_received;
  if stacktrace then Printexc.record_backtrace true;
  let longleaf_env = Environment.make () in
  if Options.Runtype.is_manual runtype then (
    Manual.top eio_env longleaf_env;
    exit 0);
  Util.yojson_safe stacktrace @@ fun () ->
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  let mutices = Longleaf_mutex.create () in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    let context : Backend.Run_context.t =
      {
        eio_env;
        longleaf_env;
        switch;
        preload;
        target;
        save_received;
        mutices;
        save_to_file;
      }
    in
    Eio.traceln "@[Context: %a@]@." Backend.Run_context.pp context;
    let res = Longleaf_strategies.run runtype context strategy_arg in
    Eio.traceln "@[Final response: %f@]@." res;
    ()
  in
  let run_server () =
    match no_gui with
    | true -> ()
    | false ->
        Eio.Domain_manager.run domain_manager @@ fun () ->
        Gui.top ~mutices eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
