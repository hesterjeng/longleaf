module Order = Trading_types.Order

module Bar_item : sig
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

module Data = struct
  type received = (string * Bar_item.t list) list [@@deriving show, yojson]
  type symbol_history = Bar_item.t Vector.vector

  let pp_symbol_history : symbol_history Vector.printer = Vector.pp Bar_item.pp

  type t = (string * symbol_history) list [@@deriving show]

  let empty : t = []
  let original_received_of_yojson = received_of_yojson

  let received_of_yojson (x : Yojson.Safe.t) : received =
    try
      match x with
      | `Assoc s ->
          List.map
            (fun ((ticker, data) : string * Yojson.Safe.t) ->
              ( ticker,
                match data with
                | `List l -> List.map Bar_item.t_of_yojson l
                | `Assoc _ -> [ Bar_item.t_of_yojson data ]
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
    let symbol_history : symbol_history =
      List.Assoc.get ~eq:String.equal order.symbol data |> function
      | Some x -> x
      | None ->
          invalid_arg
          @@ Format.asprintf "Unable to find symbol in order history: %s"
               order.symbol
    in
    let found = ref false in
    let time = Order.timestamp order in
    let res =
      Vector.map_in_place
        (fun bar_item ->
          if Ptime.equal (Bar_item.timestamp bar_item) time then (
            found := true;
            Bar_item.add_order order bar_item)
          else bar_item)
        symbol_history
    in
    if not @@ !found then
      Eio.traceln "@[[ERROR] Unplaceable order! %a@]@.@[%a@]@." Time.pp time
        Order.pp order;
    res

  let t_of_yojson json : t =
    let received = received_of_yojson json in
    List.map
      (fun (symbol, history) -> (symbol, Vector.of_list history))
      received

  let t_of_yojson x =
    try t_of_yojson x
    with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (e, j) ->
      let exc = Printexc.to_string e in
      Eio.traceln "data: %s: %s" exc (Yojson.Safe.to_string j);
      exit 1

  let yojson_of_t (x : t) =
    let listified : received =
      List.map (fun (symbol, history) -> (symbol, Vector.to_list history)) x
    in
    yojson_of_received listified
end

type t = {
  data : Data.t; [@key "bars"]
  next_page_token : string option; [@default None]
  currency : string option; [@default None]
}
[@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

type bars = t [@@deriving show { with_path = false }, yojson]

let empty : t = { data = Data.empty; next_page_token = None; currency = None }
let tickers (x : t) = List.map fst x.data
let add_order (order : Order.t) (x : t) = Data.add_order order x.data

(* FIXME: This function does a lot of work to ensure that things are in the correct order *)
let combine (l : t list) : t =
  let keys =
    List.flat_map (fun x -> List.Assoc.keys x.data) l
    |> List.uniq ~eq:String.equal
  in
  let get_data key =
    let data =
      Vector.flat_map
        (fun (x : t) ->
          match List.Assoc.get ~eq:String.equal key x.data with
          | Some found -> found
          | None -> Vector.of_array [||]
          (* Vector.make 0 *))
        (Vector.of_list l)
    in
    Vector.sort' Bar_item.compare data;
    data
  in
  let data = List.map (fun key -> (key, get_data key)) keys in
  { data; next_page_token = None; currency = None }

let get (bars : t) ticker = List.Assoc.get ~eq:String.equal ticker bars.data

let price x ticker =
  let bars = x.data in
  match List.Assoc.get ~eq:String.equal ticker bars with
  | Some vec when Vector.length vec = 1 -> Vector.get vec 0
  | Some _ -> invalid_arg "Multiple bar items on latest bar?"
  | None ->
      invalid_arg
      @@ Format.asprintf "Unable to get price info for ticker %s" ticker

module Plotly = struct
  open Bar_item
  open Option.Infix

  let of_bars (x : bars) (symbol : string) : Yojson.Safe.t option =
    let+ data_vec = get x symbol in
    let data = Vector.to_list data_vec in
    let x_axis =
      let mk_plotly_x x =
        let time = timestamp x in
        let res = Ptime.to_rfc3339 time in
        `String res
      in
      List.map mk_plotly_x data
    in
    let buy_axis =
      List.map
        (fun (x : Bar_item.t) ->
          match order x with
          | None -> `Null
          | Some order -> (
              match order.side with Buy -> `String "buy" | Sell -> `Null))
        data
    in
    let sell_axis =
      List.map
        (fun (x : Bar_item.t) ->
          match order x with
          | None -> `Null
          | Some order -> (
              match order.side with Buy -> `Null | Sell -> `String "sell"))
        data
    in
    let y_axis = List.map (fun x -> `Float (last x)) data in
    `Assoc
      [
        ( "data",
          `List
            [
              `Assoc
                [
                  ("x", `List x_axis);
                  ("y", `List y_axis);
                  ("buy", `List buy_axis);
                  ("sell", `List sell_axis);
                  ("mode", `String "lines+markers");
                  ("name", `String symbol);
                ];
            ] );
        ( "layout",
          `Assoc
            [
              ("title", `String "Sample Plotly Graph");
              ( "xaxis",
                `Assoc [ ("title", `String "X Axis"); ("type", `String "date") ]
              );
              ("yaxis", `Assoc [ ("title", `String "Y Axis") ]);
            ] );
      ]
end
