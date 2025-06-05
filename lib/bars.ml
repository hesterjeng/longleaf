module Hashtbl_make = Hashtbl.Make
module Hashtbl = Hashtbl_make (Instrument)

let fold (x : 'a Hashtbl.t) init f = Hashtbl.fold f x init

module Latest = Latest

type t = Price_history.t Hashtbl.t

let of_seq x = Hashtbl.of_seq x

let pp : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp Instrument.pp Price_history.pp in
  Format.fprintf fmt "@[%a@]@." pp seq

let show x = Format.asprintf "%a" pp x

let pp_stats : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Pair.pp Instrument.pp Int.pp in
  Seq.iter
    (fun (symbol, v) ->
      Format.fprintf fmt "@[%a@]@." pp (symbol, Vector.length v))
    seq

let length (x : t) =
  let seq = Hashtbl.to_seq x in
  let folder length (symbol, vec) =
    match length with
    | 0 -> Vector.length vec
    | _ -> (
      let new_length = Vector.length vec in
      match length = new_length with
      | true -> length
      | false ->
        Eio.traceln
          "error: bars.ml: Length mistmatch at symbol %a: previous length: %d \
           current length: %d"
          Instrument.pp symbol length new_length;
        Int.min length new_length)
  in
  Seq.fold folder 0 seq

let split ~midpoint ~target_length ~combined_length (x : t) : t * t =
  assert (0 <= midpoint && midpoint <= combined_length);
  let seq = Hashtbl.to_seq x in
  let first_part =
    Seq.map
      ( Pair.map_snd @@ fun vec ->
        Vector.mapi (fun i p -> if i <= midpoint then Some p else None) vec
        |> Vector.filter_map Fun.id )
      seq
  in
  let second_part =
    Seq.map
      ( Pair.map_snd @@ fun vec ->
        Vector.mapi
          (fun i p ->
            if i > midpoint && i < midpoint + target_length then Some p
            else None)
          vec
        |> Vector.filter_map Fun.id )
      seq
  in
  (Hashtbl.of_seq first_part, Hashtbl.of_seq second_part)

let get (x : t) symbol = Hashtbl.find_opt x symbol

let get_i (x : t) i : (Latest.t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  let tbl = Hashtbl.create 100 in
  let* () =
    fold x (Ok ()) @@ fun instrument history acc ->
    let* _ = acc in
    let* elt =
      try Result.return @@ Vector.get history i with
      | _ -> Error.missing_data "bars.ml: attempted illegal bars index access"
    in
    Hashtbl.replace tbl instrument elt;
    Result.return ()
  in
  Result.return tbl

let get_res (x : t) symbol =
  match get x symbol with
  | None ->
    Result.fail
    @@ `MissingData
         (Format.asprintf "[error: Bars.get_err] Symbol history for %a"
            Instrument.pp symbol)
  | Some x -> Result.return x

let sort cmp (x : t) = Hashtbl.iter (fun _ vector -> Vector.sort' cmp vector) x
let empty () : t = Hashtbl.create 100

let add_order (order : Order.t) (data : t) =
  let ( let* ) = Result.( let* ) in
  let symbol = order.symbol in
  let* symbol_history = get_res data symbol in
  let* top =
    Vector.pop symbol_history |> function
    | Some x -> Result.return x
    | None ->
      Result.fail
      @@ `MissingData
           (Format.asprintf "[error: Bars.add_order] No data for %a in bars"
              Instrument.pp symbol)
  in
  let* _ =
    match Ptime.equal order.timestamp @@ Item.timestamp top with
    | true -> Ok ()
    | false -> Result.fail @@ `FatalError "[error: bars.ml] Timestamp mismatch"
  in
  invalid_arg "Bars.add_order deprecated"
(* let* order_added = Item.add_order order top in *)
(* Vector.push symbol_history order_added; *)
(* Ok () *)

let t_of_yojson (json : Yojson.Safe.t) : (t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  let bars = Yojson.Safe.Util.member "bars" json in
  let assoc = Yojson.Safe.Util.to_assoc bars in
  let res =
    let* mapped =
      Result.map_l
        (fun (sym, data) ->
          match data with
          | `List datapoints ->
            let* instrument = Instrument.of_string_res sym in
            let res = List.map Item.t_of_yojson datapoints |> Vector.of_list in
            Result.return (instrument, res)
          | _ -> Error.json "Expected a list of datapoints in Bars.t_of_yojson")
        assoc
    in
    let seq = Seq.of_list mapped in
    Result.return @@ Hashtbl.of_seq seq
  in
  res

let of_file file =
  Yojson.Safe.from_file file |> t_of_yojson |> function
  | Ok x -> x
  | Error e -> invalid_arg @@ Error.show e

let yojson_of_t (x : t) : Yojson.Safe.t =
  `Assoc
    [
      ( "bars",
        `Assoc
          (Hashtbl.to_seq x |> Seq.to_list
          |> List.map (fun (sym, data) ->
                 ( Instrument.symbol sym,
                   Vector.to_list data |> List.map Item.yojson_of_t |> fun l ->
                   `List l ))) );
    ]

let keys (x : t) = Hashtbl.to_seq_keys x |> Seq.to_list
let iter f (x : t) : unit = Hashtbl.iter f x

(* FIXME: This function does a lot of work to ensure that things are in the correct order *)
let combine (l : t list) : t =
  let keys = List.flat_map keys l |> List.uniq ~eq:Instrument.equal in
  let get_data key : Price_history.t =
    let data =
      Vector.flat_map
        (fun (x : t) ->
          match Hashtbl.find_opt x key with
          | Some found -> found
          | None -> Vector.create ())
        (Vector.of_list l)
    in
    Vector.uniq_sort Item.compare data;
    (* Vector.sort' Item.compare data; *)
    data
  in
  let new_table = Hashtbl.create @@ List.length keys in
  List.iter (fun key -> Hashtbl.replace new_table key (get_data key)) keys;
  new_table

let copy (x : t) : t =
  let seq = Hashtbl.to_seq x in
  let tbl = Seq.map (fun (symbol, h) -> (symbol, Vector.copy h)) seq in
  Hashtbl.of_seq tbl

let append (latest : Latest.t) (x : t) =
  Hashtbl.iter
    (fun symbol item ->
      match get x symbol with
      | None -> Hashtbl.replace x symbol @@ Vector.return item
      | Some h -> Vector.push h item)
    latest

let print_to_file_direct bars filename =
  let bars_json = yojson_of_t bars in
  let str = Yojson.Safe.to_string bars_json in
  let oc = open_out filename in
  output_string oc str;
  close_out oc

let print_to_file ?(filename : string option) bars prefix =
  let tail =
    match filename with
    | Some n -> n
    | None -> Lots_of_words.select () ^ "_" ^ Lots_of_words.select ()
  in
  let filename = Format.sprintf "data/%s_%s.json" prefix tail in
  print_to_file_direct bars filename

let map f (x : t) : t = Hashtbl.to_seq x |> Seq.map f |> Hashtbl.of_seq

let length_check (x : t) =
  let ( let* ) = Result.( let* ) in
  let res =
    fold x (Result.return @@ -1) @@ fun _ history acc ->
    let* acc = acc in
    let l = Vector.length history in
    match acc with
    | -1 -> Result.return l
    | x ->
      let* ok =
        match acc = x with
        | true -> Result.return acc
        | false -> Error.fatal "Mismatched bars vector lengths"
      in
      Result.return ok
  in
  res

module Infill = struct
  (* Alpaca market data api can have missing data. *)
  (* Try to fill it in with the most recent bar indicating no change if it happens. *)
  (* FIXME:  This is too big for a single function. *)
  (* FIXME: This function abuses Instrument.security to get a string hashtable rather than instrument hashtable *)

  module Timetbl = Hashtbl_make (Time)

  let top (original_bars : t) =
    Eio.traceln "Infill.top";
    let _, most_used_vector =
      let fold f =
        Seq.fold f (Instrument.Security "Bars.Infill.init", Vector.create ())
        @@ Hashtbl.to_seq original_bars
      in
      fold @@ fun (best_symbol, best_vector) (current_symbol, current_vector) ->
      if Vector.length current_vector >= Vector.length best_vector then
        (current_symbol, current_vector)
      else (best_symbol, best_vector)
    in
    Eio.traceln "Creating time tables";
    let () =
      let fold =
       fun symbol ->
        (* Eio.traceln "Iterating over %s" symbol; *)
        let vec = Hashtbl.find original_bars symbol in
        let tbl = Timetbl.create @@ Vector.length vec in
        Vector.iter
          (fun item ->
            let timestamp = Item.timestamp item in
            Timetbl.replace tbl timestamp item)
          vec;
        (* At this point, we have a hashtable of times to items *)
        (* We need to iterate over the longest vector.  If its time is present in the table, *)
        (* great, else we will look BACKWARDS in the longest vector for a time where a value is *)
        (* present.  The first found value will fill in the missing one. *)
        Vector.iteri
          (fun i item ->
            let current_time = Item.timestamp item in
            match Timetbl.find_opt tbl current_time with
            | Some _ -> ()
            | None ->
              let previous_time =
                if i > 0 then
                  Vector.get most_used_vector (i - 1) |> Item.timestamp
                else (
                  Eio.traceln "Lacking initial value, using first value.";
                  let found = Hashtbl.find original_bars symbol in
                  assert (not @@ Vector.is_empty found);
                  Vector.get found 0 |> Item.timestamp)
              in
              (* Eio.traceln "Creating value for %d: %s" i current_time; *)
              let previous_value =
                Timetbl.find_opt tbl previous_time
                |> Option.get_exn_or "Expected to find previous time"
              in
              Timetbl.replace tbl current_time @@ previous_value)
          most_used_vector;
        (* Now, we should have good tables whose lengths are appropriate. *)
        (* We need to convert the current table back to a vector. *)
        let new_vector = Timetbl.to_seq_values tbl |> Vector.of_seq in
        Vector.sort' Item.compare new_vector;
        (* Replace the old, sparse, vector with the new sorted and infilled one. *)
        Hashtbl.replace original_bars symbol new_vector;
        ()
      in
      Seq.iter fold @@ Hashtbl.to_seq_keys original_bars
    in
    ()
end

module V2 = struct
  type t = Price_history.V2.t Hashtbl.t

  let copy x =
    let copied = Hashtbl.copy x in
    Hashtbl.iter
      (fun s ph -> Hashtbl.replace copied s @@ Price_history.V2.copy ph)
      x;
    copied

  let t_of_yojson (json : Yojson.Safe.t) : (t, Error.t) result =
    let ( let* ) = Result.( let* ) in
    let bars = Yojson.Safe.Util.member "bars" json in
    let assoc = Yojson.Safe.Util.to_assoc bars in
    let* mapped =
      Result.map_l
        (fun (symbol, json) ->
          let* instrument = Instrument.of_string_res symbol in
          let* ph = Price_history.V2.t_of_yojson json in
          Result.return @@ (instrument, ph))
        assoc
    in
    let seq = Seq.of_list mapped in
    Result.return @@ Hashtbl.of_seq seq

  let yojson_of_t (x : t) : Yojson.Safe.t =
    Eio.traceln "Bars.V2.yojson_of_t NYI";
    `Null

  let empty () = Hashtbl.create 100

  let get (x : t) instrument =
    Hashtbl.find_opt x instrument |> function
    | Some x -> Result.return x
    | None -> Error.missing_data "Missing data for instrument in V2 bars"

  let length (x : t) =
    let ( let* ) = Result.( let* ) in
    fold x (Ok 0) @@ fun _ ph acc ->
    let* acc = acc in
    let len = Price_history.V2.length ph in
    match acc with
    | 0 -> Ok len
    | n when len = n -> Ok acc
    | _ -> Error.fatal "Mismated price history v2 matrices in Bars v2"

  let of_file file =
    Yojson.Safe.from_file file |> t_of_yojson |> function
    | Ok x -> x
    | Error e -> invalid_arg @@ Error.show e

  let append (latest : Latest.t) (x : t) =
    let ( let* ) = Result.( let* ) in
    fold latest (Ok ()) @@ fun symbol item acc ->
    let* acc = acc in
    let* ph = get x symbol in
    let* () = Price_history.V2.add_item ph item in
    Result.return acc

  let pp : t Format.printer =
   fun fmt _ -> Format.fprintf fmt "@[<Bars.V2.pp opaque>@]@."
end
