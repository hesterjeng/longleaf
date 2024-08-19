let () =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Py.initialize ();
  Longleaf.run ();
  Format.printf "@[Hello! %d@]@." Longleaf.A.x;
  exit (if Logs.err_count () > 0 then 1 else 0)
