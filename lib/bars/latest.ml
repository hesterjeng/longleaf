module Hashtbl_make = Hashtbl.Make
module Hashtbl = Hashtbl_make (Instrument)
module Column = Data.Column

let fold (x : 'a Hashtbl.t) init f = Hashtbl.fold f x init

type t = Column.t Hashtbl.t

let fold (x : t) init f = fold x init f
let get_opt : t -> Instrument.t -> Column.t option = Hashtbl.find_opt
let empty () : t = Hashtbl.create 100

let pp : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp Instrument.pp Column.pp in
  Format.fprintf fmt "@[%d@]@." (Seq.length seq);
  Format.fprintf fmt "@[%a@]@." pp seq

let show x = Format.asprintf "%a" pp x

let get x (symbol : Instrument.t) : (Column.t, Error.t) result =
  match Hashtbl.find_opt x symbol with
  | Some x -> Ok x
  | None ->
    let err =
      Format.asprintf "[error] Unable to find price data for %a in Bars.get"
        Instrument.pp symbol
    in
    Eio.traceln "%a" pp x;
    Error.missing_data err

let iter f (x : t) : unit = Hashtbl.iter (fun x y -> f x y) x

let timestamp (x : t) =
  let ( let* ) = Result.( let* ) in
  let* res =
    fold x (Ok None) @@ fun _ item prev ->
    let* prev = prev in
    match prev with
    | None ->
      let* t = Column.timestamp item in
      Result.return @@ Option.return t
    | Some prev_time -> (
      let* current_timestamp = Column.timestamp item in
      match Ptime.compare prev_time current_timestamp with
      | 0 -> Result.return prev
      | -1
      | 1 ->
        Error.fatal "Time mismatch in Bars.timestamp"
      | _ -> Error.fatal "Impossible return value from Ptime.compare")
  in
  match res with
  | None -> Error.missing_data "No values in Bars.Latest.t"
  | Some t -> Result.return t

let of_seq x = Hashtbl.of_seq x
let set x symbol (value : Column.t) = Hashtbl.replace x symbol value
