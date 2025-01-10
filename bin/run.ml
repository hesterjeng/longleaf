open Longleaf
module Strats = Longleaf_strategies

[@@@warning "-26-27"]

let runtype_target_check ~runtype ~target : unit =
  match target with
  | Some _ -> (
      match runtype with
      | Options.Runtype.Backtest | Options.Runtype.BuyAndHold -> ()
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

let top ~runtype ~preload ~stacktrace ~no_gui ~target ~save_received ~eio_env =
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
      { eio_env; longleaf_env; switch; preload; target; save_received; mutices }
    in
    let res =
      match runtype with
      | Listener -> Strats.Listener.top runtype context
      | BuyAndHold -> Strats.BuyAndHold.top runtype context
      | _ -> Strats.LowBall.top runtype context
    in
    Eio.traceln "@[Final response: %s@]@." res;
    ()
  in
  let run_server () =
    if no_gui then ()
    else
      Eio.Domain_manager.run domain_manager @@ fun () ->
      Gui.top ~mutices eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
