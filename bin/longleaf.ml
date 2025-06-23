module Longleaf = Longleaf_lib
module Options = Longleaf.Options

module Args = struct
  module Runtype = Longleaf.Options.RunType

  (* Define the CLI arguments *)
  let runtype_arg =
    let runtype_conv = Runtype.conv in
    let doc =
      Format.asprintf "The type of run.  Valid choices are %a."
        (List.pp String.pp) Runtype.all
    in
    Cmdliner.Arg.(
      required & pos 0 (some runtype_conv) None & info [] ~docv:"runtype" ~doc)

  let strategy_arg =
    let doc =
      Format.asprintf "The selected strategy.  Valid choices are %a."
        (List.pp String.pp) Longleaf_strategies.all
    in
    Cmdliner.Arg.(
      required
      & pos 1 (some Longleaf_strategies.conv) None
      & info [] ~docv:"strategy" ~doc)

  (* let preload_arg = *)
  (*   let preload_conv = Preload.conv in *)
  (*   let doc = *)
  (*     "The data used to \"warmup\" indicators.  This data should be just \ *)
  (*      before the target data.  Valid choices are \"none\", \"download\", or \ *)
  (*      \"%s\" where %s is the file you want preloaded as bars.  This data will \ *)
  (*      be in the background, as historical information.  If this value is \ *)
  (*      None, the strategy will run on the target data as if there were no \ *)
  (*      preloaded data.  If the argument is Download, an attempt to download \ *)
  (*      some market data will be made.  Otherwise, `--preload $file will` \ *)
  (*      attempt to use $file, which is expects to be a file in Alpaca market \ *)
  (*      data JSON format." *)
  (*   in *)
  (*   Cmdliner.Arg.(value & opt preload_conv None & info [ "p"; "preload" ] ~doc) *)

  let target_arg =
    let preload_conv = Longleaf.Target.conv in
    let doc =
      "The data file to actually backtest on.  This is only for use with \
       backtesting.  The algorithm will process this information as if it is \
       being received over the wire."
    in
    Cmdliner.Arg.(
      value
      & opt preload_conv (invalid_arg "Must select target")
      & info [ "t"; "target" ] ~doc)

  let output_file_arg =
    let doc = "Output file for a log." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc)

  let stacktrace_arg =
    let doc = "Print a stacktrace if an exception occurs." in
    Cmdliner.Arg.(value & flag & info [ "g" ] ~doc)

  let print_tick_arg =
    let doc = "Print the current tick." in
    Cmdliner.Arg.(value & flag & info [ "pt" ] ~doc)

  let no_gui_arg =
    let doc = "Disable the gui process." in
    Cmdliner.Arg.(value & flag & info [ "nogui" ] ~doc)

  let precompute_indicators_arg =
    let doc = "Precompute indicators." in
    Cmdliner.Arg.(value & flag & info [ "indicators" ] ~doc)

  let compare_preload =
    let doc = "Compare live with preloaded indicators" in
    Cmdliner.Arg.(value & flag & info [ "compare-preload" ] ~doc)

  let save_received_arg =
    let doc = "Save received data." in
    Cmdliner.Arg.(value & flag & info [ "sr"; "save-received" ] ~doc)

  let save_to_file =
    let doc = "Save data to files." in
    Cmdliner.Arg.(value & flag & info [ "s"; "save" ] ~doc)

  let nowait_market_open =
    let doc = "Don't wait for market open to try running the strategy." in
    Cmdliner.Arg.(value & flag & info [ "nowait-market-open" ] ~doc)

  let start_arg =
    let doc = "Starting index for backtest" in
    Cmdliner.Arg.(value & opt (some int) None & info [ "i"; "index" ] ~doc)
end

module Cmd = struct
  let run runtype stacktrace output no_gui target save_received strategy_arg
      save_to_file nowait_market_open print_tick_arg precompute_indicators_arg
      compare_preloaded =
    Fmt_tty.setup_std_outputs ();
    Longleaf.Util.handle_output output;
    (* let reporter = Logs_fmt.reporter () in *)
    (* Logs.set_reporter reporter; *)
    (* Logs.set_level ~all:true (Some Logs.Info); *)
    Eio_main.run @@ fun eio_env ->
    let cli_args =
      Options.CLI.make ~runtype ~stacktrace ~no_gui ~target ~save_received
        ~strategy_arg ~save_to_file ~nowait_market_open ~print_tick_arg
        ~precompute_indicators_arg ~compare_preloaded
    in
    Run.top ~eio_env cli_args

  let top =
    let term =
      Cmdliner.Term.(
        const run $ Args.runtype_arg $ Args.stacktrace_arg
        $ Args.output_file_arg $ Args.no_gui_arg $ Args.target_arg
        $ Args.save_received_arg $ Args.strategy_arg $ Args.save_to_file
        $ Args.nowait_market_open $ Args.print_tick_arg
        $ Args.precompute_indicators_arg $ Args.compare_preload)
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
