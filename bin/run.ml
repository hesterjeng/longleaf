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
    let module Context : Strats.RUN_CONTEXT = struct
      let eio_env = eio_env
      let longleaf_env = longleaf_env
      let switch = switch
      let preload = preload
      let target = target
      let save_received = save_received
      let mutices = mutices
    end in
    match runtype with
    | Listener ->
        let module Run = Strats.Listener.Make (Context) in
        Run.top runtype
    | BuyAndHold ->
        let module Run = Strats.BuyAndHold.Make (Context) in
        Run.top runtype
    | _ ->
        (* let module Run = Run.DoubleTop.Make (LongleafMutex) (Context) in *)
        let module Run = Strats.LowBall.Make (Context) in
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
