(* let pyprint x = *)
(*   let open Pyops in *)
(*   let builtins = Py.import "builtins" in *)
(*   let p = builtins.&("print") in *)
(*   let _ = p [| x |] in *)
(*   () *)

(* module Environment = Environment *)
module Lots_of_words = Lots_of_words
module Astar = Astar
module Work_pool = Work_pool
module Pmutex = Pmutex
module Json = Json

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
  assert (n >= 0);
  let length = Vector.length vec in
  Vector.slice_iter vec (Int.max (length - n) 0) (Int.min n length)

let random_state = Random.State.make_self_init ()

let random_choose_opt l =
  match l with
  | [] -> None
  | l -> Some (List.random_choose l random_state)

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

let random_state = Random.State.make_self_init ()

let qty ~current_cash ~pct ~price =
  match current_cash >=. 0.0 with
  | true ->
    let tenp = current_cash *. pct in
    let max_amt = tenp /. price in
    if max_amt >=. 1.0 then floor max_amt |> Float.to_int else 0
  | false -> 0

let random_filename () =
  Lots_of_words.select random_state ^ "_" ^ Lots_of_words.select random_state

let apca_api_base_url ty =
  match ty with
  | `Live -> Uri.of_string "https://api.alpaca.markets/v2"
  | `Paper -> Uri.of_string "https://paper-api.alpaca.markets/v2"

let apca_api_data_url = Uri.of_string "https://data.alpaca.markets/v2"
