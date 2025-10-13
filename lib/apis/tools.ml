module Headers = Cohttp.Header
module Error = Longleaf_core.Error

let get_cohttp ~client ~headers ~endpoint : (Yojson.Safe.t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  let headers = Headers.to_list headers in
  let uri = Uri.of_string endpoint in
  match Cohttp_eio.Client.get ~headers client uri with
  | resp, body ->
    let status = Cohttp.Response.status resp in
    let _status_code = Cohttp.Code.code_of_status status in
    let body_string = Eio.Buf_read.parse_exn Eio.Buf_read.take_all body ~max_size:max_int in
    (try
      let res = Result.return (Yojson.Safe.from_string body_string) in
      res
    with
    | Yojson.Json_error s as e ->
      let resp_headers = Cohttp.Response.headers resp in
      Eio.traceln "@[%s@]@." body_string;
      Eio.traceln
        "@[Error converting body of response to json in get_cohttp.@]@.@[reason: \
         %s@]@.@[headers: %a@]@.@[endpoint: %s@]@."
        s Headers.pp_hum resp_headers endpoint;
      let s = Printexc.to_string e in
      Result.fail @@ `Msg s)
  | exception e ->
    let s = Printexc.to_string e in
    Eio.traceln "@[Error in get_cohttp: %s@]@." s;
    Result.fail @@ `Msg s

let delete_cohttp ~client ~headers ~endpoint =
  let headers = Headers.to_list headers in
  let uri = Uri.of_string endpoint in
  match Cohttp_eio.Client.delete ~headers client uri with
  | resp, body ->
    let _status = Cohttp.Response.status resp in
    let body_string = Eio.Buf_read.parse_exn Eio.Buf_read.take_all body ~max_size:max_int in
    Yojson.Safe.from_string body_string
  | exception e ->
    invalid_arg @@ Format.asprintf "delete_cohttp: %s" (Printexc.to_string e)

let post_cohttp ~client ~body:json_body ~headers ~endpoint =
  let headers = Headers.to_list headers in
  let body_string = Yojson.Safe.to_string json_body in
  let body = Cohttp_eio.Body.of_string body_string in
  let uri = Uri.of_string endpoint in
  match Cohttp_eio.Client.post ~headers ~body client uri with
  | resp, _body -> resp
  | exception e ->
    invalid_arg @@ Format.asprintf "post_cohttp: %s" (Printexc.to_string e)

(* Backward compatibility aliases *)
let get_piaf = get_cohttp
let delete_piaf = delete_cohttp
let post_piaf = post_cohttp
