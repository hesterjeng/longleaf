module Order = Trading_types.Order
module Hashtbl = Hashtbl.Make (String)

module Item : sig
  type t [@@deriving show, yojson]

  val make :
    timestamp:Time.t ->
    open_:float ->
    high:float ->
    low:float ->
    close:float ->
    last:float ->
    volume:int ->
    ?order:Trading_types.Order.t option ->
    unit ->
    t

  val compare : t Ord.t
  val timestamp : t -> Time.t
  val open_ : t -> float
  val last : t -> float
  val high : t -> float
  val low : t -> float
  val close : t -> float
  val volume : t -> int
  val order : t -> Order.t option
  val add_order : Order.t -> t -> t
end = struct
  type t = {
    timestamp : Time.t; [@key "t"]
    open_ : float; [@key "o"]
    high : float; [@key "h"]
    low : float; [@key "l"]
    close : float; [@key "c"] (* We are using this as the latest price... *)
    last : float; [@yojson.default Float.max_finite_value]
    volume : int; [@key "v"]
    (* trade_count : int; [@key "n"] *)
    (* volume_weighted : float; [@key "vw"] *)
    order : Trading_types.Order.t option; [@default None]
  }
  [@@deriving show { with_path = false }, yojson, make]
  [@@yojson.allow_extra_fields]

  (* let t_of_yojson x = *)
  (*   try *)
  (*     t_of_yojson x |> fun (x : t) -> *)
  (*     if Float.equal x.last Float.max_finite_value then *)
  (*       { x with last = x.close } *)
  (*     else x *)
  (*   with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (e, j) -> *)
  (*     let exc = Printexc.to_string e in *)
  (*     Eio.traceln "@[bar_item:@]@.@[%s@]@.@[%s@]@." exc *)
  (*       (Yojson.Safe.to_string j); *)
  (*     exit 1 *)

  let open_ x = x.open_
  let order x = x.order

  let add_order (order : Order.t) (x : t) =
    match x.order with
    | None -> { x with order = Some order }
    | Some _ ->
        (* Eio.traceln "@[Warning: trying to replace: %a with %a@]@." Order.pp *)
        (* prev_order Order.pp order; *)
        x

  let timestamp (x : t) = x.timestamp
  let close x = x.close
  let high x = x.high
  let low x = x.low
  let last (x : t) = x.last
  let volume x = x.volume
  let compare x y = Ptime.compare x.timestamp y.timestamp
end

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

  let empty : t = Hashtbl.create 0
end

type symbol_history = Item.t Vector.vector

let pp_symbol_history : symbol_history Vector.printer = Vector.pp Item.pp

type t = symbol_history Hashtbl.t

let pp : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp String.pp pp_symbol_history in
  Format.fprintf fmt "@[%a@]@." pp seq

let get (x : t) symbol =
  Hashtbl.find_opt x symbol |> function
  | Some res -> res
  | None ->
      invalid_arg
      @@ Format.asprintf "Unable to find item for symbol %s (bars.ml)" symbol

let get_opt (x : t) symbol = Hashtbl.find_opt x symbol

let sort (x : t) =
  Hashtbl.to_seq_values x
  |> Seq.iter @@ fun vector -> Vector.sort' Item.compare vector

let empty : t = Hashtbl.create 100
(* let original_received_of_yojson = Received.t_of_yojson *)

let add_order (order : Order.t) (data : t) =
  let symbol_history = get data order.symbol in
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
  Hashtbl.to_seq latest
  |> Seq.iter @@ fun (symbol, item) -> Vector.push (get x symbol) item

let price bars ticker =
  match List.Assoc.get ~eq:String.equal ticker bars with
  | Some vec when Vector.length vec = 1 -> Vector.get vec 0
  | Some _ -> invalid_arg "Multiple bar items on latest bar?"
  | None ->
      invalid_arg
      @@ Format.asprintf "Unable to get price info for ticker %s" ticker
