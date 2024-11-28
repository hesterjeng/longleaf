module Args = struct
  (* Define the CLI arguments *)
  let runtype_arg =
    let runtype_conv = Longleaf.Options.Runtype.conv in
    let doc =
      "The type of run.  Valid choices are \"live\", \"paper\", or \
       \"backtest\"."
    in
    Cmdliner.Arg.(
      required & pos 0 (some runtype_conv) None & info [] ~docv:"runtype" ~doc)

  let preload_arg =
    let preload_conv = Longleaf.Options.Preload.conv in
    let doc =
      "The type of preloading.  Valid choices are \"none\", \"download\", or \
       \"%s\" where %s is the file you want preloaded as bars."
    in
    Cmdliner.Arg.(value & opt preload_conv None & info [ "p"; "preload" ] ~doc)

  let output_file_arg =
    let doc = "Output file for a log." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc)

  let stacktrace_arg =
    let doc = "Print a stacktrace if an exception occurs" in
    Cmdliner.Arg.(value & flag & info [ "g" ] ~doc)
end

module Cmd = struct
  let run runtype preload stacktrace output =
    Fmt_tty.setup_std_outputs ();
    Longleaf.Util.handle_output output;
    let reporter = Logs_fmt.reporter () in
    Logs.set_reporter reporter;
    Logs.set_level ~all:true (Some Logs.Info);
    Eio_main.run @@ fun eio_env ->
    Longleaf.top ~stacktrace ~preload ~runtype eio_env

  let top =
    let term =
      Cmdliner.Term.(
        const run $ Args.runtype_arg $ Args.preload_arg $ Args.stacktrace_arg
        $ Args.output_file_arg)
    in
    let doc =
      "This is the OCaml algorithmic trading platform longleaf.  It relies on \
       having a backend instantiated, with appropriate strategies.  The \
       overall structure of the project is very functor heavy.  For an example \
       of how to create a new strategy, look at double_top.ml and run.ml.  In \
       longleaf.ml, you can instantiate the functors to create your strategy."
    in
    let info = Cmdliner.Cmd.info ~doc "./main.exe" in
    Cmdliner.Cmd.v info term
end

let () = Stdlib.exit @@ Cmdliner.Cmd.eval Cmd.top
