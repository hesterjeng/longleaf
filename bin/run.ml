open Longleaf_lib

let runtype_target_check ~runtype ~target : unit =
  match target with
  | Some _ -> (
    match runtype with
    | Options.RunType.Backtest
    | Multitest
    | Montecarlo
    | MultiMontecarlo
    | RandomSliceBacktest
    | MultiRandomSliceBacktest
    | RandomTickerBacktest
    | MultiRandomTickerBacktest
    | AstarSearch ->
      ()
    | _ ->
      Eio.traceln "Must be in a backtest if we have a specified target.";
      exit 1)
  | None -> ()

let save_received_check ~runtype ~save_received : unit =
  if save_received then
    match runtype with
    | Options.RunType.Live
    | Options.RunType.Paper ->
      ()
    | _ ->
      Eio.traceln "Must be live or paper to save received data.";
      exit 1

let mk_context ~runtype ~preload ~stacktrace ~no_gui ~target ~save_received
    ~eio_env ~strategy_arg ~save_to_file ~nowait_market_open ~print_tick_arg
    ~precompute_indicators_arg ~switch () : Options.Context.t =
  let _ = stacktrace in
  let indicator_type, preload, target =
    match precompute_indicators_arg with
    | true ->
      Eio.traceln "Precomputing indicators...";
      let preload : Options.Preload.t = Loaded (Options.Preload.load preload) in
      let target : Options.Preload.t =
        match target with
        | Some s -> Loaded (Options.Preload.load (File s))
        | None -> Options.Preload.None
      in
      Indicators.precompute
        (Options.Preload.bars preload)
        (Options.Preload.bars target);
      (Options.IndicatorType.Precomputed, preload, target)
    | false ->
      ( Live,
        preload,
        match target with
        | Some s -> File s
        | None -> Options.Preload.None )
  in
  let longleaf_env = Environment.make () in
  let mutices = Longleaf_mutex.create () in
  {
    strategy = Longleaf_strategies.show strategy_arg;
    runtype;
    indicator_type;
    no_gui;
    eio_env;
    longleaf_env;
    switch;
    preload;
    target;
    save_received;
    nowait_market_open;
    mutices;
    save_to_file;
    print_tick_arg;
  }

let top ~runtype ~preload ~stacktrace ~no_gui ~target ~save_received ~eio_env
    ~strategy_arg ~save_to_file ~nowait_market_open ~print_tick_arg
    ~precompute_indicators_arg =
  runtype_target_check ~runtype ~target;
  save_received_check ~runtype ~save_received;
  let _ = stacktrace in
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  let mutices = Longleaf_mutex.create () in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    let context =
      mk_context ~runtype ~preload ~stacktrace ~no_gui ~target ~save_received
        ~eio_env ~strategy_arg ~save_to_file ~nowait_market_open ~print_tick_arg
        ~precompute_indicators_arg ~switch ()
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
