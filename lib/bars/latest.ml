module Column = Data.Column

(* Use Saturn for thread-safe access *)
type t = (Instrument.t, Column.t) Saturn.Htbl.t

(* Instrument hashed module for Saturn *)
module InstrumentHashedType = struct
  type t = Instrument.t
  let equal = Instrument.equal
  let hash = Instrument.hash
end

let create_table () = Saturn.Htbl.create ~hashed_type:(module InstrumentHashedType) ()

let fold (x : t) init f =
  Saturn.Htbl.to_seq x
  |> Seq.fold_left (fun acc (k, v) -> f k v acc) init

let get_opt : t -> Instrument.t -> Column.t option = Saturn.Htbl.find_opt
let empty () : t = create_table ()

let pp : t Format.printer =
 fun fmt x ->
  let seq = Saturn.Htbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp Instrument.pp Column.pp in
  Format.fprintf fmt "@[%d@]@." (Seq.length seq);
  Format.fprintf fmt "@[%a@]@." pp seq

let show x = Format.asprintf "%a" pp x

let get x (symbol : Instrument.t) : (Column.t, Error.t) result =
  match Saturn.Htbl.find_opt x symbol with
  | Some x -> Ok x
  | None ->
    let err =
      Format.asprintf "[error] Unable to find price data for %a in Bars.get"
        Instrument.pp symbol
    in
    Eio.traceln "%a" pp x;
    Error.missing_data err

let iter f (x : t) : unit =
  Saturn.Htbl.to_seq x |> Seq.iter (fun (k, v) -> f k v)

let timestamp (x : t) =
  let ( let* ) = Result.( let* ) in
  let* res =
    fold x (Ok None) @@ fun _ item prev ->
    let* prev = prev in
    match prev with
    | None ->
      let* t = Column.timestamp item in
      Result.return @@ Option.return t
    | Some prev_time ->
      let* current_timestamp = Column.timestamp item in
      (match Ptime.compare prev_time current_timestamp with
      | 0 -> Result.return prev
      | -1
      | 1 ->
        Error.fatal "Time mismatch in Bars.timestamp"
      | _ -> Error.fatal "Impossible return value from Ptime.compare")
  in
  match res with
  | None -> Error.missing_data "No values in Bars.Latest.t"
  | Some t -> Result.return t

let of_seq seq =
  let tbl = create_table () in
  Seq.iter (fun (k, v) -> ignore (Saturn.Htbl.try_add tbl k v)) seq;
  tbl

let set x symbol (value : Column.t) =
  ignore (Saturn.Htbl.set_exn x symbol value)
