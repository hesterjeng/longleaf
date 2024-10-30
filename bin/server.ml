let _ =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Eio_main.run @@ fun eio_env ->
  Eio.Switch.run (fun sw -> Longleaf.top sw eio_env)
(* Lwt_eio.with_event_loop ~clock:eio_env#clock ~debug:true @@ fun () -> *)
(* Longleaf.top eio_env *)
