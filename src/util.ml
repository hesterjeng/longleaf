let pyprint x =
  let open Pyops in
  let builtins = Py.import "builtins" in
  let p = builtins.&("print") in
  let _ = p [| x |] in
  ()

include Ppx_yojson_conv_lib.Yojson_conv
module Get_log = (val Logs.src_log Logs.(Src.create "get-log"))
module Util_log = (val Logs.src_log Logs.(Src.create "util-log"))

let handle_response json response =
  let open Cohttp in
  match Response.status response with
  | #Code.success_status -> Lwt.return_ok json
  | #Code.informational_status as status ->
      Get_log.err (fun k ->
          k "informational_status: %s" (Code.string_of_status status));
      Lwt.return_error @@ Code.string_of_status status
  | #Code.redirection_status as status ->
      Get_log.err (fun k ->
          k "redirection_status: %s" (Code.string_of_status status));
      Lwt.return_error @@ Code.string_of_status status
  | #Code.client_error_status as status ->
      Get_log.err (fun k ->
          k "client_error_status: %s" (Code.string_of_status status));
      Lwt.return_error @@ Code.string_of_status status
  | #Code.server_error_status as status ->
      Get_log.err (fun k ->
          k "server_error_status: %s" (Code.string_of_status status));
      Lwt.return_error @@ Code.string_of_status status
  | `Code _ as status ->
      Get_log.err (fun k -> k "unknown code: %s" (Code.string_of_status status));
      Lwt.return_error @@ Code.string_of_status status

let get ~headers ~uri : (Yojson.Safe.t, string) Lwt_result.t =
  let open Lwt.Syntax in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let* response, body_stream = Client.get ~headers uri in
  let* resp_body_raw = Cohttp_lwt.Body.to_string body_stream in
  let json = Yojson.Safe.from_string resp_body_raw in
  handle_response json response

let delete ~headers ~uri : (Yojson.Safe.t, string) Lwt_result.t =
  let open Lwt.Syntax in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let* response, body_stream = Client.delete ~headers uri in
  let* resp_body_raw = Cohttp_lwt.Body.to_string body_stream in
  let json = Yojson.Safe.from_string resp_body_raw in
  handle_response json response

let post ~headers ~body ~uri : (Yojson.Safe.t, string) Lwt_result.t =
  let open Lwt.Syntax in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let* response, body_stream = Client.post ~headers ~body uri in
  let* resp_body_raw = Cohttp_lwt.Body.to_string body_stream in
  let json = Yojson.Safe.from_string resp_body_raw in
  handle_response json response

let get_next_page_token (x : Yojson.Safe.t) =
  Option.(
    let* npt = Yojson.Safe.Util.(to_option (member "next_page_token") x) in
    match npt with
    | `String s -> Some s
    | `Null -> None
    | _ -> invalid_arg "next_page_token must be a string or null")

let rfc339 () =
  let open CalendarLib in
  let datetime = Calendar.now () in
  let year = Calendar.year datetime in
  let month = Date.int_of_month @@ Calendar.month datetime in
  let day = Calendar.day_of_month datetime in
  let hour = Calendar.hour datetime in
  let minute = Calendar.minute datetime in
  let second = Calendar.second datetime in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" year month day hour minute
    second

let show_calendar_t datetime =
  let open CalendarLib in
  let year = Calendar.year datetime in
  let month = Date.int_of_month @@ Calendar.month datetime in
  let day = Calendar.day_of_month datetime in
  let hour = Calendar.hour datetime in
  let minute = Calendar.minute datetime in
  let second = Calendar.second datetime in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" year month day hour minute
    second

let show_calendar_time_t time =
  let open CalendarLib in
  let hour = Calendar.Time.hour time in
  let minute = Calendar.Time.minute time in
  let second = Calendar.Time.second time in
  Printf.sprintf "%02d:%02d:%02d" hour minute second

let listen_for_input () =
  let open Lwt.Syntax in
  let rec loop () =
    let* input = Lwt_io.read_line Lwt_io.stdin in
    if String.equal input "q" then Lwt.return "q" else loop ()
  in
  loop ()
