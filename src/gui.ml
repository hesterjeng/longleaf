open Bogue

let requests_sent = ref 0
let incr () = incr requests_sent
let decr () = decr requests_sent

let top () =
  let set_number w i = Widget.set_text w @@ Int.to_string i in
  let count = Widget.label "0" in
  let action _ =
    let open Lwt.Syntax in
    Lwt_main.run
    @@
    let uri = Uri.of_string "http://localhost:8080/run_backtest" in
    let headers = Cohttp.Header.init () in
    let* res = Util.get ~headers ~uri in
    match res with
    | Ok _ ->
        incr ();
        set_number count !requests_sent;
        Lwt.return_unit
    | Error e ->
        decr ();
        Format.printf "%s" e;
        set_number count !requests_sent;
        Lwt.return_unit
  in
  let start_button = Widget.button ~action "Start" in
  let stop_button = Widget.button "Stop" in
  let label = Widget.label "Requests sent" in
  let w =
    Layout.flat_of_w ~align:Draw.Center
      [ label; count; start_button; stop_button ]
  in
  let layout = Bogue.of_layout w in
  Bogue.run layout

let top () =
  try top ()
  with Unix.Unix_error (Unix.ECONNREFUSED, "connect", "") ->
    Format.printf "Failed to connect with the server, is it running?";
    ()
