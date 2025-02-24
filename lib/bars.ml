module Hashtbl = Hashtbl.Make (String)

module Latest = struct
  type t = Item.t Hashtbl.t

  let get_opt = Hashtbl.find_opt
  let empty () : t = Hashtbl.create 0

  let pp : t Format.printer =
   fun fmt x ->
    let seq = Hashtbl.to_seq x in
    let pp = Seq.pp @@ Pair.pp String.pp Item.pp in
    Format.fprintf fmt "@[%d@]@." (Seq.length seq);
    Format.fprintf fmt "@[%a@]@." pp seq

  let get x symbol : Item.t =
    match Hashtbl.find_opt x symbol with
    | Some x -> x
    | None ->
        Eio.traceln "%a" pp x;
        invalid_arg "Unable to find price of symbol (Bars.Latest)"

  let timestamp (x : t) =
    ( (fun f -> Hashtbl.fold f x (Ok None)) @@ fun _ item prev ->
      match prev with
      | Ok None -> Result.return @@ Some (Item.timestamp item)
      | Ok (Some prev_time) -> (
          let current_timestamp = Item.timestamp item in
          match Ptime.compare prev_time current_timestamp with
          | 1 | 0 -> prev
          | -1 ->
              (* Eio.traceln *)
              (*   "@[@[Time mismatch in latest bars:@]@.@[%a@]@.@[%a@]@.@]@." *)
              (*   Time.pp prev_time Time.pp (Item.timestamp item); *)
              Ok (Some current_timestamp)
          | _ -> invalid_arg "Impossible return value from Ptime.compare")
      | Error _ -> prev )
    |> function
    | Ok None ->
        Eio.traceln "Current latest: %a" pp x;
        Result.fail @@ `MissingData "No values in Bars.Latest.t"
    | Ok (Some res) ->
        (* Eio.traceln "Ok Bars.Latest.t"; *)
        Ok res
    | Error e ->
        Eio.traceln "%a" pp x;
        Result.fail @@ `MissingData e
end

type symbol_history = Item.t Vector.vector

let pp_symbol_history : symbol_history Vector.printer = Vector.pp Item.pp

type t = symbol_history Hashtbl.t

let pp : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp String.pp pp_symbol_history in
  Format.fprintf fmt "@[%a@]@." pp seq

let pp_stats : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Pair.pp String.pp Int.pp in
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
              "error: bars.ml: Length mistmatch at symbol %s: previous length: \
               %d current length: %d"
              symbol length new_length;
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

let get_err (x : t) symbol =
  match get x symbol with
  | None ->
      Result.fail
      @@ `MissingData
           (Format.asprintf "[error: Bars.get_err] Symbol history for %s" symbol)
  | Some x -> Result.return x

let sort cmp (x : t) = Hashtbl.iter (fun _ vector -> Vector.sort' cmp vector) x
let empty () : t = Hashtbl.create 100

let add_order (order : Order.t) (data : t) =
  let ( let* ) = Result.( let* ) in
  let symbol = order.symbol in
  let* symbol_history = get_err data symbol in
  let* top =
    Vector.pop symbol_history |> function
    | Some x -> Result.return x
    | None ->
        Result.fail
        @@ `MissingData
             (Format.asprintf "[error: Bars.add_order] No data for %s in bars"
                symbol)
  in
  let* _ =
    match Ptime.equal order.timestamp @@ Item.timestamp top with
    | true -> Ok ()
    | false -> Result.fail @@ `FatalError "[error: bars.ml] Timestamp mismatch"
  in
  let* order_added = Item.add_order order top in
  Vector.push symbol_history order_added;
  Ok ()
(* assert (Ptime.equal order.timestamp @@ ) *)

let t_of_yojson (json : Yojson.Safe.t) : t =
  let bars = Yojson.Safe.Util.member "bars" json in
  let assoc = Yojson.Safe.Util.to_assoc bars in
  let res =
    List.Assoc.map_values
      (function
        | `List datapoints ->
            (* Eio.traceln "@[%a@]@." (List.pp Yojson.Safe.pp) datapoints; *)
            List.map Item.t_of_yojson datapoints |> Vector.of_list
        | _ -> invalid_arg "Expected a list of datapoints")
      assoc
    |> Seq.of_list |> Hashtbl.of_seq
  in
  res

let yojson_of_t (x : t) : Yojson.Safe.t =
  `Assoc
    [
      ( "bars",
        `Assoc
          (Hashtbl.to_seq x |> Seq.to_list
          |> List.Assoc.map_values (fun data ->
                 Vector.to_list data |> List.map Item.yojson_of_t |> fun l ->
                 `List l)) );
    ]

let keys (x : t) = Hashtbl.to_seq_keys x |> Seq.to_list

(* FIXME: This function does a lot of work to ensure that things are in the correct order *)
let combine (l : t list) : t =
  let keys = List.flat_map keys l |> List.uniq ~eq:String.equal in
  let get_data key : symbol_history =
    let data =
      Vector.flat_map
        (fun (x : t) ->
          match Hashtbl.find_opt x key with
          | Some found -> found
          | None -> Vector.create ())
        (Vector.of_list l)
    in
    Vector.sort' Item.compare data;
    data
  in
  let new_table = Hashtbl.create @@ List.length keys in
  List.iter (fun key -> Hashtbl.replace new_table key (get_data key)) keys;
  new_table

let append (latest : Latest.t) (x : t) =
  Hashtbl.to_seq latest
  |> Seq.iter @@ fun (symbol, item) ->
     match get x symbol with
     | None -> Hashtbl.replace x symbol @@ Vector.return item
     | Some h -> Vector.push h item

let print_to_file ?(filename : string option) bars prefix =
  let bars_json = yojson_of_t bars in
  let str = Yojson.Safe.to_string bars_json in
  let tail =
    match filename with
    | Some n -> n
    | None -> Lots_of_words.select () ^ "_" ^ Lots_of_words.select ()
  in
  let filename = Format.sprintf "data/%s_%s.json" prefix tail in
  let oc = open_out filename in
  output_string oc str;
  close_out oc

let print_to_file_direct bars filename =
  let bars_json = yojson_of_t bars in
  let str = Yojson.Safe.to_string bars_json in
  let oc = open_out filename in
  output_string oc str;
  close_out oc

module Infill = struct
  (* Alpaca market data api can have missing data. *)
  (* Try to fill it in with the most recent bar indicating no change if it happens. *)
  (* FIXME:  This is too big for a single function. *)

  let top (original_bars : t) =
    Eio.traceln "Infill.top";
    let _, most_used_vector =
      let fold f =
        Seq.fold f ("Bars.Infill.init", Vector.create ())
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
        let tbl = Hashtbl.create @@ Vector.length vec in
        Vector.iter
          (fun item ->
            let timestamp = Item.timestamp item |> Time.to_string in
            Hashtbl.replace tbl timestamp item)
          vec;
        (* At this point, we have a hashtable of times to items *)
        (* We need to iterate over the longest vector.  If its time is present in the table, *)
        (* great, else we will look BACKWARDS in the longest vector for a time where a value is *)
        (* present.  The first found value will fill in the missing one. *)
        Vector.iteri
          (fun i item ->
            let current_time = Item.timestamp item |> Time.to_string in
            match Hashtbl.find_opt tbl current_time with
            | Some _ -> ()
            | None ->
                let previous_time =
                  if i > 0 then
                    Vector.get most_used_vector (i - 1)
                    |> Item.timestamp |> Time.to_string
                  else (
                    Eio.traceln "Lacking initial value, using first value.";
                    let found = Hashtbl.find original_bars symbol in
                    assert (not @@ Vector.is_empty found);
                    Vector.get found 0 |> Item.timestamp |> Time.to_string)
                in
                (* Eio.traceln "Creating value for %d: %s" i current_time; *)
                let previous_value =
                  Hashtbl.find_opt tbl previous_time
                  |> Option.get_exn_or "Expected to find previous time"
                in
                Hashtbl.replace tbl current_time @@ previous_value)
          most_used_vector;
        (* Now, we should have good tables whose lengths are appropriate. *)
        (* We need to convert the current table back to a vector. *)
        let new_vector = Hashtbl.to_seq_values tbl |> Vector.of_seq in
        Vector.sort' Item.compare new_vector;
        (* Replace the old, sparse, vector with the new sorted and infilled one. *)
        Hashtbl.replace original_bars symbol new_vector;
        ()
      in
      Seq.iter fold @@ Hashtbl.to_seq_keys original_bars
    in
    ()
end
