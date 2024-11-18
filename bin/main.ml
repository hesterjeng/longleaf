module Args = struct
  (* Define the CLI arguments *)
  let runtype =
    let doc = "The type of run." in
    Cmdliner.Arg.(
      required & pos 0 (some float) None & info [] ~docv:"WIDTH" ~doc)

  let height_arg =
    let doc = "The height of the rectangle." in
    Cmdliner.Arg.(
      required & pos 1 (some float) None & info [] ~docv:"HEIGHT" ~doc)
end

let run runtype output_file =
  let backtesting = true in
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Eio_main.run @@ fun eio_env -> Longleaf.top eio_env backtesting
