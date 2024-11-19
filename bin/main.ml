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

  let output_file_arg =
    let doc = "Output file for a log." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc)
end

module Cmd = struct
  let run runtype output =
    Fmt_tty.setup_std_outputs ();
    let reporter = Logs_fmt.reporter () in
    Logs.set_reporter reporter;
    Logs.set_level ~all:true (Some Logs.Info);
    Eio_main.run @@ fun eio_env -> Longleaf.top ~runtype ~output eio_env

  let top =
    let term =
      Cmdliner.Term.(const run $ Args.runtype_arg $ Args.output_file_arg)
    in
    let info = Cmdliner.Cmd.info "./main.exe" in
    Cmdliner.Cmd.v info term
end

let () = Stdlib.exit @@ Cmdliner.Cmd.eval Cmd.top
