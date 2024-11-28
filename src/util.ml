(* let pyprint x = *)
(*   let open Pyops in *)
(*   let builtins = Py.import "builtins" in *)
(*   let p = builtins.&("print") in *)
(*   let _ = p [| x |] in *)
(*   () *)

include Ppx_yojson_conv_lib.Yojson_conv
module Get_log = (val Logs.src_log Logs.(Src.create "get-log"))
module Util_log = (val Logs.src_log Logs.(Src.create "util-log"))

let get_piaf ~client ~headers ~endpoint =
  let open Piaf in
  let headers = Headers.to_list headers in
  let resp =
    match Client.get client ~headers endpoint with
    | Ok x -> x
    | Error e -> invalid_arg @@ Format.asprintf "%a" Error.pp_hum e
  in
  let _status = Response.status resp in
  let body = Response.body resp in
  let json =
    match Body.to_string body with
    | Ok x -> x
    | Error e -> invalid_arg @@ Format.asprintf "%a" Error.pp_hum e
  in
  try Yojson.Safe.from_string json
  with Yojson.Json_error s as e ->
    let resp_headers = Response.headers resp in
    Eio.traceln
      "@[Error converting body of response to json in get_piaf.@]@.@[reason: \
       %s@]@.@[headers: %a@]@."
      s Headers.pp_hum resp_headers;
    raise e

let delete_piaf ~client ~headers ~endpoint =
  let open Piaf in
  let headers = Headers.to_list headers in
  let resp =
    match Client.delete client ~headers endpoint with
    | Ok x -> x
    | Error e -> invalid_arg @@ Format.asprintf "%a" Error.pp_hum e
  in
  let _status = Response.status resp in
  let body = Response.body resp in
  let json =
    match Body.to_string body with
    | Ok x -> x
    | Error e -> invalid_arg @@ Format.asprintf "%a" Error.pp_hum e
  in
  Yojson.Safe.from_string json

let post_piaf ~client ~body ~headers ~endpoint =
  let open Piaf in
  let headers = Headers.to_list headers in
  let body = Yojson.Safe.to_string body |> Body.of_string in
  match Client.post client ~headers ~body endpoint with
  | Ok x -> x
  | Error e -> invalid_arg @@ Format.asprintf "post_piaf: %a" Error.pp_hum e

let get_next_page_token (x : Yojson.Safe.t) =
  Option.(
    let* npt = Yojson.Safe.Util.(to_option (member "next_page_token") x) in
    match npt with
    | `String s -> Some s
    | `Null -> None
    | _ -> invalid_arg "next_page_token must be a string or null")

let read_file_as_string filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  (* Close the input channel *)
  content (* Return the content *)

let yojson_safe stacktrace (f : unit -> 'a) : 'a =
  try f ()
  with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, j) ->
    (if stacktrace then
       let str =
         Printexc.get_callstack 40 |> Printexc.raw_backtrace_to_string
       in
       Eio.traceln "@[%s@]@." str);
    Eio.traceln "Yojson error in main longleaf program!";
    Eio.traceln "@[%a@]@." Yojson.Safe.pp j;
    let err = Printexc.to_string e in
    invalid_arg @@ Format.asprintf "%s" err

let handle_output output =
  (* Redirect stdout and stderr to the selected file *)
  match output with
  | None -> ()
  | Some file_path ->
      let fd =
        Unix.openfile file_path
          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
          0o644
      in
      Unix.dup2 fd Unix.stdout;
      Unix.dup2 fd Unix.stderr;
      Unix.close fd

let read_file_as_string filename =
  let in_channel = open_in filename in
  let len = in_channel_length in_channel in
  let content = really_input_string in_channel len in
  close_in in_channel;
  content

module type CLIENT = sig
  val longleaf_env : Environment.t
  val client : Piaf.Client.t
end
