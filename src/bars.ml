module Order = Trading_types.Order
module Hashtbl = Hashtbl.Make (String)

(* module Received = struct *)
(*   type t = { bars : (string * Item.t list) list } [@@deriving show, yojson] *)

(*   let unbox x = x.bars *)
(* end *)

module Latest = struct
  type t = Item.t Hashtbl.t

  let get x symbol =
    match Hashtbl.find_opt x symbol with
    | Some x -> x
    | None -> invalid_arg "Unable to find price of symbol (Bars.Latest)"

  let empty () : t = Hashtbl.create 0

  let pp : t Format.printer =
   fun fmt x ->
    let seq = Hashtbl.to_seq x in
    let pp = Seq.pp @@ Pair.pp String.pp Item.pp in
    Format.fprintf fmt "@[%a@]@." pp seq

  let timestamp (x : t) =
    let ( let* ) = Result.( let* ) in
    (fun f -> Hashtbl.fold f x (Error "No values in Bars.Latest.t hash table"))
    @@ fun _ item prev ->
    let* prev_time = prev in
    match Ptime.equal prev_time @@ Item.timestamp item with
    | true -> prev
    | false -> Error "Different timestamps within same Bars.Latest.t!"
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

let get (x : t) symbol = Hashtbl.find_opt x symbol

let sort cmp (x : t) =
  Hashtbl.to_seq_values x |> Seq.iter @@ fun vector -> Vector.sort' cmp vector

let empty () : t = Hashtbl.create 100
(* let original_received_of_yojson = Received.t_of_yojson *)

let add_order (order : Order.t) (data : t) =
  let symbol_history =
    get data order.symbol
    |> Option.get_exn_or "Expected to find symbol history in Bars.add_order"
  in
  let found = ref false in
  let time = Order.timestamp order in
  let res =
    Vector.map_in_place
      (fun bar_item ->
        if Ptime.equal (Item.timestamp bar_item) time then (
          found := true;
          Item.add_order order bar_item)
        else bar_item)
      symbol_history
  in
  if not @@ !found then
    Eio.traceln "@[[ERROR] Could not place order in data! %a@]@.@[%a@]@."
      Time.pp time Order.pp order;
  res

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

(* FIXME: This function does a lot of work to ensure that things are in the correct order *)
let combine (l : t list) : t =
  let keys =
    List.flat_map (fun x -> Hashtbl.to_seq_keys x |> Seq.to_list) l
    |> List.uniq ~eq:String.equal
  in
  let get_data key =
    let data =
      Vector.flat_map
        (fun (x : t) ->
          match Hashtbl.find_opt x key with
          | Some found -> found
          | None -> Vector.of_array [||])
        (Vector.of_list l)
    in
    Vector.sort' Item.compare data;
    data
  in
  let new_table = Hashtbl.create @@ List.length keys in
  List.iter (fun key -> Hashtbl.replace new_table key (get_data key)) keys;
  new_table

let append (latest : Latest.t) (x : t) =
  (* Eio.traceln "Bars.append: %a" Latest.pp latest; *)
  (* Eio.traceln "Bars.append: There are %d bindings" (Hashtbl.length x); *)
  Hashtbl.to_seq latest
  |> Seq.iter @@ fun (symbol, item) ->
     match get x symbol with
     | None ->
         (* Eio.traceln "Creating symbol_history for %s" symbol; *)
         Hashtbl.replace x symbol @@ Vector.create ()
     | Some h -> Vector.push h item

let price bars ticker =
  match List.Assoc.get ~eq:String.equal ticker bars with
  | Some vec when Vector.length vec = 1 -> Vector.get vec 0
  | Some _ -> invalid_arg "Multiple bar items on latest bar?"
  | None ->
      invalid_arg
      @@ Format.asprintf "Unable to get price info for ticker %s" ticker

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

  let top (x : t) =
    Eio.traceln "Infill.top";
    let _, most_used_vector =
      let fold f =
        Seq.fold f ("Bars.Infill.init", Vector.create ()) @@ Hashtbl.to_seq x
      in
      fold @@ fun (best_symbol, best_vector) (current_symbol, current_vector) ->
      if Vector.length current_vector >= Vector.length best_vector then
        (current_symbol, current_vector)
      else (best_symbol, best_vector)
    in
    Eio.traceln "Creating time tables";
    let () =
      Seq.iter (fun symbol ->
          (* Eio.traceln "Iterating over %s" symbol; *)
          let vec = Hashtbl.find x symbol in
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
                      Vector.get (Hashtbl.find x symbol) 0
                      |> Item.timestamp |> Time.to_string)
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
          Hashtbl.replace x symbol new_vector;
          ())
      @@ Hashtbl.to_seq_keys x
    in
    ()
end
