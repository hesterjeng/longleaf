let handler : Dream.handler =
  Dream.router [ (Dream.get "/" @@ fun _ -> Dream.html "Hello John!") ]

let top () =
  Format.printf "Running";
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop @@ fun () -> Dream.run @@ Dream.logger @@ handler

(* let clock = Eio.Stdenv.clock env in *)
(* Lwt_eio.with_event_loop ~clock @@ fun () -> *)
(* Lwt *)
(* invalid_arg "NYI" *)
(* Lwt_eio.run_lwt @@ fun () -> *)
(* Lwt_eio.run_lwt_in_main @@ *)
(* Dream.serve invalid_arg "NYI" *)
