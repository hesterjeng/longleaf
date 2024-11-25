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
    ?action_taken:Trading_types.Order.t option ->
    unit ->
    t

  val compare : t Ord.t
  val timestamp : t -> Time.t
  val last : t -> float
  val high : t -> float
  val low : t -> float
  val close : t -> float
  val volume : t -> int
end = struct
  [@@@warning "-11"]

  type t = {
    timestamp : Time.t; [@key "t"]
    open_ : float; [@key "o"]
    high : float; [@key "h"]
    low : float; [@key "l"]
    close : float; [@key "c"] (* We are using this as the latest price... *)
    last : float; [@key "c"]
    volume : int; [@key "v"]
    (* trade_count : int; [@key "n"] *)
    (* volume_weighted : float; [@key "vw"] *)
    action_taken : Trading_types.Order.t option; [@default None]
  }
  [@@deriving show { with_path = false }, yojson, make]
  [@@yojson.allow_extra_fields]

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

  let t_of_yojson json : t =
    let received = received_of_yojson json in
    List.map
      (fun (symbol, history) -> (symbol, Vector.of_list history))
      received

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

  let of_bars (x : bars) (symbol : string) : Yojson.Safe.t =
    let data =
      match get x symbol with
      | None ->
          invalid_arg
            "Cannot create plotly diagram, unable to find datapoints for ticker"
      | Some data -> data
    in
    let x_axis =
      (* FIXME:  This doesn't seem to be working! Maybe it's in the JS? *)
      let mk_plotly_x x =
        let time = timestamp x in
        let res = Ptime.to_rfc3339 time in
        `String res
        (* let res = Ptime.to_float_s time |> Int.of_float in *)
        (* `Int res *)
      in
      Vector.to_list @@ Vector.map mk_plotly_x data
    in
    let y_axis = List.map (fun x -> `Float (last x)) @@ Vector.to_list data in
    `Assoc
      [
        ( "data",
          `List
            [
              `Assoc
                [
                  ("x", `List x_axis);
                  ("y", `List y_axis);
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
