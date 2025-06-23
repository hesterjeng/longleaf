open Longleaf_lib

let mk_context ~runtype ~stacktrace ~no_gui ~target ~save_received ~eio_env
    ~strategy_arg ~save_to_file ~nowait_market_open ~print_tick_arg
    ~precompute_indicators_arg ~compare_preloaded ~switch () : Options.Context.t
    =
  let _ = stacktrace in
  (* let _ = preload in *)
  (* let target : Preload.t = *)
  (*   match target with *)
  (*   | Some s -> Preload.(Loaded (load @@ File s)) *)
  (*   | None -> None *)
  (* in *)
  let _ = precompute_indicators_arg in
  (* let target = Options.Preload.(Loaded (load target)) in *)
  let longleaf_env = Environment.make () in
  let mutices = Longleaf_mutex.create () in
  {
    strategy = Longleaf_strategies.show strategy_arg;
    runtype;
    (* indicators; *)
    no_gui;
    eio_env;
    longleaf_env;
    switch;
    (* preload; *)
    target;
    save_received;
    compare_preloaded;
    nowait_market_open;
    mutices;
    save_to_file;
    print_tick_arg;
  }

type cli_args = Longleaf_strategies.t Options.CLI.t

let top ~eio_env (x : cli_args) =
  let {
    runtype : Options.RunType.t;
    (* preload : string; *)
    stacktrace : bool;
    no_gui : bool;
    target : Target.t;
    save_received : bool;
    strategy_arg : Longleaf_strategies.t;
    save_to_file : bool;
    nowait_market_open : bool;
    print_tick_arg : bool;
    precompute_indicators_arg : bool;
    compare_preloaded : bool;
  } : cli_args =
    x
  in
  let _ = stacktrace in
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  let mutices = Longleaf_mutex.create () in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    let context =
      mk_context ~runtype ~stacktrace ~no_gui ~target ~save_received ~eio_env
        ~strategy_arg ~save_to_file ~nowait_market_open ~print_tick_arg
        ~precompute_indicators_arg ~switch ~compare_preloaded ()
    in
    Eio.traceln "@[Context: %a@]@." Options.Context.pp context;
    let res = Longleaf_strategies.run context strategy_arg in
    Eio.traceln "@[Final response: %f@]@." res;
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
