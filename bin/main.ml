let _old () =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Py.initialize ();
  let file = "data/da.json" in
  let json = Yojson.Safe.from_file file in
  let _res = Longleaf.process_json json in
  exit (if Logs.err_count () > 0 then 1 else 0)

let () =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Py.initialize ();
  let result = Lwt_main.run @@ Longleaf.top () in
  exit (if Logs.err_count () > 0 then 1 else Cohttp.Code.code_of_status result)
