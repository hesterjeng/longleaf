(* open Eio.Std *)

(* Lwt_main.run @@ Longleaf.top *)
let _ =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Eio_main.run @@ fun eio_env -> Longleaf.top eio_env
(* Eio_main.run @@ fun env -> *)
(* Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> *)
(* Lwt_eio.run_lwt @@ fun () -> Longleaf.top *)

(* Eio_main.run @@ fun env -> *)
(* Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> *)
(* Lwt_eio.run_lwt @@ fun () -> Dream.serve ~port:8080 @@ Longleaf.Handler.top () *)
