module Headers = Cohttp.Header
module Error = Longleaf_core.Error
open Cohttp_eio

let read_body body =
  try
    Result.return
    @@ Eio.Buf_read.parse_exn Eio.Buf_read.take_all body ~max_size:max_int
  with
  | _ -> Error.fatal "problem reading body in tools.ml"

let get_cohttp ~client ~headers ~endpoint : (Yojson.Safe.t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  Eio.Switch.run @@ fun sw ->
  let uri = Uri.of_string endpoint in
  let* resp, body =
    match Client.get ~headers ~sw client uri with
    | resp, body -> Ok (resp, body)
    | exception e ->
      let s = Printexc.to_string e in
      Eio.traceln "@[Error in get_cohttp: %s@]@." s;
      Result.fail @@ `Msg s
  in
  let* () =
    if Http.Status.compare resp.status `OK = 0 then Ok ()
    else Error.fatal "tools.ml:  bad response status get"
  in
  let* body_string = read_body body in
  Result.return @@ Yojson.Safe.from_string body_string

let delete_cohttp ~client ~headers ~endpoint : (Yojson.Safe.t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  Eio.Switch.run @@ fun sw ->
  let uri = Uri.of_string endpoint in
  let* resp, body =
    match Client.delete ~headers ~sw client uri with
    | resp, body -> Ok (resp, body)
    | exception e ->
      let s = Printexc.to_string e in
      Eio.traceln "@[Error in delete_cohttp: %s@]@." s;
      Result.fail @@ `Msg s
  in
  let* () =
    if Http.Status.compare resp.status `OK = 0 then Ok ()
    else Error.fatal "tools.ml:  bad response status delete"
  in
  let* body_string = read_body body in
  Result.return @@ Yojson.Safe.from_string body_string

let post_cohttp ~client ~body:json_body ~headers ~endpoint :
    (Yojson.Safe.t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  Eio.Switch.run @@ fun sw ->
  let body_string = Yojson.Safe.to_string json_body in
  let body = Body.of_string body_string in
  let uri = Uri.of_string endpoint in
  let* resp, body =
    match Client.post ~headers ~body ~sw client uri with
    | resp, body -> Result.return @@ (resp, body)
    | exception e ->
      invalid_arg @@ Format.asprintf "post_cohttp: %s" (Printexc.to_string e)
  in
  let* () =
    if Http.Status.compare resp.status `OK = 0 then Ok ()
    else Error.fatal "tools.ml:  bad response status post"
  in
  let* body_string = read_body body in
  Result.return @@ Yojson.Safe.from_string body_string

(* Backward compatibility aliases *)
let get_piaf = get_cohttp
let delete_piaf = delete_cohttp
let post_piaf = post_cohttp
