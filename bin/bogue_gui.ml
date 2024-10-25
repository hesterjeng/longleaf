open Longleaf
open Bogue

[@@@warning "-32"]

let requests_sent = ref 0
let incr () = incr requests_sent
let decr () = decr requests_sent
let set_number w i = Widget.set_text w @@ Int.to_string i

let send_get_request () =
  let open Lwt_result.Syntax in
  let uri = Uri.of_string "http://localhost:8080/run_dead" in
  let headers = Cohttp.Header.init () in
  let* promise = Util.get ~headers ~uri in
  Lwt_result.return promise

let top () =
  let open Lwt_result.Syntax in
  let count = Widget.label "0" in
  let* action =
    Lwt_result.return @@ fun _ ->
    (* How can we make this NOT an Lwt.t? *)
    let _response = send_get_request () in
    incr ();
    set_number count !requests_sent;
    ()
  in
  let start_button = Widget.button ~action "Start" in
  let stop_button = Widget.button "Stop" in
  let label = Widget.label "Requests sent" in
  let w =
    Layout.flat_of_w ~align:Draw.Center
      [ label; count; start_button; stop_button ]
  in
  let layout = Bogue.of_layout w in
  Lwt_result.return @@ Bogue.run layout

let () = match Lwt_main.run @@ top () with Ok _ -> () | Error _ -> ()

let run_client ~net ~addr =
  let open Eio.Std in
  Switch.run ~name:"client" @@ fun sw ->
  traceln "Client: connecting to server";
  let flow = Eio.Net.connect ~sw net addr in
  (* Read all data until end-of-stream (shutdown): *)
  traceln "Client: received %S" (Eio.Flow.read_all flow)

(* let top = *)
(*   try Lwt_main.run @@ top () *)
(*   with Unix.Unix_error (Unix.ECONNREFUSED, "connect", "") -> *)
(*     Format.printf "@[Failed to connect with the server, is it running?@]@."; *)
(*     Ok () *)