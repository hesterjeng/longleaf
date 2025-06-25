open Longleaf_lib

let mk_options switch eio_env flags target : Options.t =
  let longleaf_env = Environment.make () in
  let mutices = Longleaf_mutex.create () in
  {
    (* indicators; *)
    eio_env;
    longleaf_env;
    switch;
    flags;
    (* preload; *)
    target;
    mutices;
  }

(* type cli_args = Longleaf_strategies.t Options.CLI.t *)

let top ~eio_env (flags : Options.CLI.t) target =
  (* let options : Options.t = *)
  (*   { *)
  (*     target; *)
  (*     flags; *)
  (*     (\* runtype : Options.RunType.t; *\) *)
  (*     (\* (\\* preload : string; *\\) *\) *)
  (*     (\* stacktrace : bool; *\) *)
  (*     (\* no_gui : bool; *\) *)
  (*     (\* target : Target.t; *\) *)
  (*     (\* save_received : bool; *\) *)
  (*     (\* strategy_arg : Longleaf_strategies.t; *\) *)
  (*     (\* save_to_file : bool; *\) *)
  (*     (\* nowait_market_open : bool; *\) *)
  (*     (\* print_tick_arg : bool; *\) *)
  (*     (\* precompute_indicators_arg : bool; *\) *)
  (*     (\* compare_preloaded : bool; *\) *)
  (*   } *)
  (* in *)
  (* let _ = stacktrace in *)
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  let mutices = Longleaf_mutex.create () in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    let options =
      mk_options switch eio_env flags target
      (* mk_context ~runtype ~stacktrace ~no_gui ~target ~save_received ~eio_env *)
      (*   ~strategy_arg ~save_to_file ~nowait_market_open ~print_tick_arg *)
      (*   ~precompute_indicators_arg ~switch ~compare_preloaded () *)
    in
    (* Eio.traceln "@[Context: %a@]@." Options.Context.pp context; *)
    let res = Longleaf_strategies.run context strategy_arg in
    (* Eio.traceln "@[Final response: %f@]@." res; *)
    ()
  in
  let run_server () =
    match no_gui with
    | true -> ()
    | false ->
      Eio.Domain_manager.run domain_manager @@ fun () ->
      Server.top ~mutices eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
