module Longleaf = Longleaf_lib
module Options = Longleaf.Options

module Cmd = struct
  module Args = Options.CLI.Args

  let run runtype stacktrace output no_gui target save_received strategy_arg
      save_to_file nowait_market_open print_tick_arg precompute_indicators_arg
      compare_preloaded start =
    Fmt_tty.setup_std_outputs ();
    Longleaf.Util.handle_output output;
    (* let reporter = Logs_fmt.reporter () in *)
    (* Logs.set_reporter reporter; *)
    (* Logs.set_level ~all:true (Some Logs.Info); *)
    Eio_main.run @@ fun eio_env ->
    let cli_args : Options.CLI.t =
      {
        runtype;
        stacktrace;
        no_gui;
        save_received;
        strategy_arg;
        save_to_file;
        nowait_market_open;
        print_tick_arg;
        precompute_indicators_arg;
        compare_preloaded;
        start;
      }
    in
    Run.top ~eio_env cli_args target

  let top =
    let term =
      Cmdliner.Term.(
        const run $ Args.runtype_arg $ Args.stacktrace_arg
        $ Args.output_file_arg $ Args.no_gui_arg $ Args.target_arg
        $ Args.save_received_arg $ Args.strategy_arg $ Args.save_to_file
        $ Args.nowait_market_open $ Args.print_tick_arg
        $ Args.precompute_indicators_arg $ Args.compare_preload $ Args.start_arg)
    in
    let doc =
      "This is the OCaml algorithmic trading platform longleaf.  It relies on \
       having a backend instantiated, with an appropriate.  The user must \
       select a backend and a strategy to run.  For an example of how to \
       create a new strategy, look at template_example.ml.  In \
       longleaf_strategies.ml, you can instantiate the functors to create your \
       strategy."
    in
    let info = Cmdliner.Cmd.info ~doc "longleaf" in
    Cmdliner.Cmd.v info term
end

let () = Stdlib.exit @@ Cmdliner.Cmd.eval Cmd.top
