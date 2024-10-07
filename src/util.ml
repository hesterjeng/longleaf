let pyprint x =
  let open Pyops in
  let builtins = Py.import "builtins" in
  let p = builtins.&("print") in
  let _ = p [| x |] in
  ()

include Ppx_yojson_conv_lib.Yojson_conv
module Get_log = (val Logs.src_log Logs.(Src.create "get-log"))
module Util_log = (val Logs.src_log Logs.(Src.create "util-log"))

let get ~headers ~uri : (Yojson.Safe.t, string) Lwt_result.t =
  let open Lwt.Syntax in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let* response, body_stream = Client.get ~headers uri in
  let* resp_body_raw = Cohttp_lwt.Body.to_string body_stream in
  let resp_body_json = Yojson.Safe.from_string resp_body_raw in
  match Response.status response with
  | #Code.success_status -> Lwt.return_ok resp_body_json
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

let delete ~headers ~uri : (Yojson.Safe.t, string) Lwt_result.t =
  let open Lwt.Syntax in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let* response, body_stream = Client.delete ~headers uri in
  let* resp_body_raw = Cohttp_lwt.Body.to_string body_stream in
  let resp_body_json = Yojson.Safe.from_string resp_body_raw in
  match Response.status response with
  | #Code.success_status -> Lwt.return_ok resp_body_json
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

let post ~headers ~body ~uri : (Yojson.Safe.t, string) Lwt_result.t =
  let open Lwt.Syntax in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let* response, body_stream = Client.post ~headers ~body uri in
  let* resp_body_raw = Cohttp_lwt.Body.to_string body_stream in
  let resp_body_json = Yojson.Safe.from_string resp_body_raw in
  match Response.status response with
  | #Code.success_status -> Lwt.return_ok resp_body_json
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

let get_next_page_token (x : Yojson.Safe.t) =
  Option.(
    let* npt = Yojson.Safe.Util.(to_option (member "next_page_token") x) in
    match npt with
    | `String s -> Some s
    | `Null -> None
    | _ -> invalid_arg "next_page_token must be a string or null")
