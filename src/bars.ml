module Order = Trading_types.Order

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

  let t_of_yojson x =
    try
      t_of_yojson x |> fun (x : t) ->
      if Float.equal x.last Float.max_finite_value then
        { x with last = x.close }
      else x
    with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (e, j) ->
      let exc = Printexc.to_string e in
      Eio.traceln "@[bar_item:@]@.@[%s@]@.@[%s@]@." exc
        (Yojson.Safe.to_string j);
      exit 1

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

type received = (string * Item.t list) list [@@deriving show, yojson]
type symbol_history = Item.t Vector.vector

let pp_symbol_history : symbol_history Vector.printer = Vector.pp Item.pp

module Hashtbl = Hashtbl.Make (String)

type t = symbol_history Hashtbl.t

let get (x : t) symbol =
  Hashtbl.find_opt x symbol |> function
  | Some res -> res
  | None -> invalid_arg "Unable to find item for symbol (bars.ml)"

let sort = List.iter (fun ((_ : string), v) -> Vector.sort' Item.compare v)
let empty : t = Hashtbl.create 100
let original_received_of_yojson = received_of_yojson

let received_of_yojson (x : Yojson.Safe.t) : received =
  try
    match x with
    | `Assoc s ->
        List.map
          (fun ((ticker, data) : string * Yojson.Safe.t) ->
            ( ticker,
              match data with
              | `List l -> List.map Item.t_of_yojson l
              | `Assoc _ -> [ Item.t_of_yojson data ]
              | a ->
                  Util.Util_log.err (fun k -> k "%a" Yojson.Safe.pp a);
                  invalid_arg "The data must be stored as a list" ))
          s
    | _ -> invalid_arg "Bars must be a toplevel Assoc"
  with _ ->
    Eio.traceln "@[Trying Data.original_t_of_yojson.@]@.";
    let res = original_received_of_yojson x in
    Eio.traceln "@[Data.original_t_of_yojson succeeded.@]@.";
    res

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

let t_of_yojson json : t =
  received_of_yojson json
  |> List.map (fun (symbol, history) -> (symbol, Vector.of_list history))
  |> List.to_seq |> Hashtbl.of_seq

let t_of_yojson x =
  try t_of_yojson x
  with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (e, j) ->
    let exc = Printexc.to_string e in
    Eio.traceln "data: %s: %s" exc (Yojson.Safe.to_string j);
    exit 1

let yojson_of_t (x : t) =
  let listified : received =
    Hashtbl.to_seq x
    |> Seq.map (fun (symbol, history) -> (symbol, Vector.to_list history))
    |> Seq.to_list
  in
  yojson_of_received listified

(* FIXME: This function does a lot of work to ensure that things are in the correct order *)
let combine (l : t list) : t =
  let keys =
    List.flat_map (fun x -> Hashtbl.to_seq_keys x |> Seq.to_list) l |> List.uniq ~eq:String.equal
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

let price bars ticker =
  match List.Assoc.get ~eq:String.equal ticker bars with
  | Some vec when Vector.length vec = 1 -> Vector.get vec 0
  | Some _ -> invalid_arg "Multiple bar items on latest bar?"
  | None ->
      invalid_arg
      @@ Format.asprintf "Unable to get price info for ticker %s" ticker
