let () =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Py.initialize ();
  let () = Longleaf.Handler.top () in
  exit 0
