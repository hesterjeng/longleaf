(* let pyprint x = *)
(*   let open Pyops in *)
(*   let builtins = Py.import "builtins" in *)
(*   let p = builtins.&("print") in *)
(*   let _ = p [| x |] in *)
(*   () *)

include Ppx_yojson_conv_lib.Yojson_conv

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
    Eio.traceln "@[%s@]@." json;
    Eio.traceln
      "@[Error converting body of response to json in get_piaf.@]@.@[reason: \
       %s@]@.@[headers: %a@]@.@[endpoint: %s@]@."
      s Headers.pp_hum resp_headers endpoint;
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
  try
    let ic = open_in filename in
    let res = IO.read_all ic in
    close_in ic;
    res
  with
  | Sys_error e ->
      Eio.traceln "%s" e;
      invalid_arg "Util.read_file_as_string"
  | End_of_file ->
      Eio.traceln "Util.read_file_as_string: EOF";
      invalid_arg "Util.read_file_as_string"

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

let last_n (n : int) (vec : ('a, _) Vector.t) : 'a Iter.t =
  let length = Vector.length vec in
  Vector.slice_iter vec (Int.max (length - n) 0) (Int.min n length)

let random_state = Random.State.make_self_init ()

(* mean and sigma chosen so that P (x >= 1.0) ~ 0.2 *)
let one_in_five =
  let rv = Owl_stats.lognormal_rvs ~mu:(-0.84) ~sigma:1.0 in
  rv >=. 1.0

let coin_flip () = Random.State.bool random_state

module type CLIENT = sig
  val longleaf_env : Environment.t
  val client : Piaf.Client.t
end
