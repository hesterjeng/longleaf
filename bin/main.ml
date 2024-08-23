let () =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Py.initialize ();
  (* Longleaf.run (); *)
  let file = "data/da.json" in
  let json = Yojson.Safe.from_file file in
  let _ = Longleaf.process_json json in
  exit (if Logs.err_count () > 0 then 1 else 0)
