module Longleaf = Longleaf_lib

module Args = struct
  (* Define the CLI arguments *)
  let runtype_arg =
    let runtype_conv = Longleaf.Options.Runtype.conv in
    let doc =
      "The type of run.  Valid choices are \"live\", \"paper\", \"listener\" \
       or \"backtest\"."
    in
    Cmdliner.Arg.(
      required & pos 0 (some runtype_conv) None & info [] ~docv:"runtype" ~doc)

  let strategy_arg =
    let doc =
      "The selected strategy.  These can be seen in \
       strategies/longleaf_strategies.ml"
    in
    Cmdliner.Arg.(
      required
      & pos 1 (some Longleaf_strategies.conv) None
      & info [] ~docv:"selected strategy" ~doc)

  let preload_arg =
    let preload_conv = Longleaf.Options.Preload.conv in
    let doc =
      "The type of preloading.  Valid choices are \"none\", \"download\", or \
       \"%s\" where %s is the file you want preloaded as bars.  This data will \
       be in the background, as historical information."
    in
    Cmdliner.Arg.(value & opt preload_conv None & info [ "p"; "preload" ] ~doc)

  let target_arg =
    let doc =
      "The data file to actually backtest on.  This is only for use with \
       backtesting.  The algorithm will process this information as if it is \
       being received over the wire."
    in
    Cmdliner.Arg.(value & opt (some string) None & info [ "t"; "target" ] ~doc)

  let output_file_arg =
    let doc = "Output file for a log." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc)

  let stacktrace_arg =
    let doc = "Print a stacktrace if an exception occurs." in
    Cmdliner.Arg.(value & flag & info [ "g" ] ~doc)

  let no_gui_arg =
    let doc = "Disable the gui process." in
    Cmdliner.Arg.(value & flag & info [ "nogui" ] ~doc)

  let save_received_arg =
    let doc = "Save received data." in
    Cmdliner.Arg.(value & flag & info [ "sr"; "save-received" ] ~doc)

  let save_to_file =
    let doc = "Save data to files." in
    Cmdliner.Arg.(value & flag & info [ "s"; "save" ] ~doc)

  let nowait_market_open =
    let doc = "Don't wait for market open to try running the strategy." in
    Cmdliner.Arg.(value & flag & info [ "nowait-market-open" ] ~doc)
end

module Cmd = struct
  let run runtype preload stacktrace output no_gui target save_received
      strategy_arg save_to_file nowait_market_open =
    Fmt_tty.setup_std_outputs ();
    Longleaf.Util.handle_output output;
    (* let reporter = Logs_fmt.reporter () in *)
    (* Logs.set_reporter reporter; *)
    (* Logs.set_level ~all:true (Some Logs.Info); *)
    Eio_main.run @@ fun eio_env ->
    Run.top ~stacktrace ~preload ~runtype ~no_gui ~target ~save_received
      ~eio_env ~strategy_arg ~save_to_file ~nowait_market_open

  let top =
    let term =
      Cmdliner.Term.(
        const run $ Args.runtype_arg $ Args.preload_arg $ Args.stacktrace_arg
        $ Args.output_file_arg $ Args.no_gui_arg $ Args.target_arg
        $ Args.save_received_arg $ Args.strategy_arg $ Args.save_to_file
        $ Args.nowait_market_open)
    in
    let doc =
      "This is the OCaml algorithmic trading platform longleaf.  It relies on \
       having a backend instantiated, with appropriate strategies.  The \
       overall structure of the project is very functor heavy.  For an example \
       of how to create a new strategy, look at double_top.ml and run.ml.  In \
       longleaf.ml, you can instantiate the functors to create your strategy."
    in
    let info = Cmdliner.Cmd.info ~doc "longleaf" in
    Cmdliner.Cmd.v info term
end

let () = Stdlib.exit @@ Cmdliner.Cmd.eval Cmd.top
