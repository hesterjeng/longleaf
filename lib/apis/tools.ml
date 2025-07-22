module Headers = Piaf.Headers
module Client = Piaf.Client
module Response = Piaf.Response
module Body = Piaf.Body
module Error = Longleaf_core.Error

let get_piaf ~client ~headers ~endpoint : (Yojson.Safe.t, Error.t) result =
  (* let open Piaf in *)
  let ( let* ) = Result.( let* ) in
  let headers = Headers.to_list headers in
  let* resp = Client.get client ~headers endpoint in
  let _status = Response.status resp in
  let body = Response.body resp in
  let* json = Body.to_string body in
  try
    let res = Result.return (Yojson.Safe.from_string json) in
    (* let resp_headers = Response.headers resp in *)
    (* Eio.traceln "response headers: %a" Headers.pp_hum resp_headers; *)
    res
  with
  | Yojson.Json_error s as e ->
    let resp_headers = Response.headers resp in
    Eio.traceln "@[%s@]@." json;
    Eio.traceln
      "@[Error converting body of response to json in get_piaf.@]@.@[reason: \
       %s@]@.@[headers: %a@]@.@[endpoint: %s@]@."
      s Headers.pp_hum resp_headers endpoint;
    let s = Printexc.to_string e in
    Result.fail @@ `Msg s

let delete_piaf ~client ~headers ~endpoint =
  let open Piaf in
  let headers = Headers.to_list headers in
  let resp =
    match Client.delete client ~headers endpoint with
    | Ok x -> x
    | Error e -> invalid_arg @@ Format.asprintf "%a" Piaf.Error.pp_hum e
  in
  let _status = Response.status resp in
  let body = Response.body resp in
  let json =
    match Body.to_string body with
    | Ok x -> x
    | Error e -> invalid_arg @@ Format.asprintf "%a" Piaf.Error.pp_hum e
  in
  Yojson.Safe.from_string json

let post_piaf ~client ~body ~headers ~endpoint =
  let open Piaf in
  let headers = Headers.to_list headers in
  let body = Yojson.Safe.to_string body |> Body.of_string in
  match Client.post client ~headers ~body endpoint with
  | Ok x -> x
  | Error e ->
    invalid_arg @@ Format.asprintf "post_piaf: %a" Piaf.Error.pp_hum e
