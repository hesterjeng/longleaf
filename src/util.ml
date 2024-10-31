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
  Yojson.Safe.from_string json

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
  let resp =
    match Client.post client ~headers ~body endpoint with
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

let read_file_as_string filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  (* Close the input channel *)
  content (* Return the content *)

module type ALPACA_SERVER = sig
  val longleaf_env : Environment.t
  val client : Piaf.Client.t
end
