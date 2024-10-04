module TimeInForce = struct
  type t =
    | Day
    | GoodTillCanceled
    | Opening
    | Close
    | ImmediateOrCancel
    | FillOrKill

  let to_string = function
    | Day -> "day"
    | GoodTillCanceled -> "gtc"
    | Opening -> "opg"
    | Close -> "cls"
    | ImmediateOrCancel -> "ioc"
    | FillOrKill -> "fok"
end

module Side = struct
  type t = Buy | Sell

  let to_string = function Buy -> "buy" | Sell -> "sell"
end

module OrderType = struct
  type t = Market | Limit | Stop | StopLimit | TrailingStop

  let to_string = function
    | Market -> "market"
    | Limit -> "limit"
    | Stop -> "stop"
    | StopLimit -> "stop_limit"
    | TrailingStop -> "trailing_stop"
end

module OrderId : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let of_string x = x
  let to_string x = x
end

module Timeframe : sig
  type t

  val to_string : t -> string
  val min : int -> t
  val hour : int -> t
  val day : t
  val week : t
  val month : int -> t
end = struct
  type t = Min of int | Hour of int | Day | Week | Month of int

  let to_string = function
    | Min i -> Format.asprintf "%dMin" i
    | Hour i -> Format.asprintf "%dHour" i
    | Day -> "1Day"
    | Week -> "1Week"
    | Month i -> Format.asprintf "%dMonth" i

  let min i =
    assert (1 <= i && i <= 59);
    Min i

  let hour i =
    assert (1 <= i && i <= 23);
    Hour i

  let day = Day
  let week = Week

  let month i =
    assert (i = 1 || i = 2 || i = 3 || i = 4 || i = 6 || i = 12);
    Month i
end

module Bars = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  module Bar_item = struct
    type t = {
      timestamp : string; [@key "t"]
      opening_price : float; [@key "o"]
      high_price : float; [@key "h"]
      low_price : float; [@key "l"]
      closing_price : float; [@key "c"]
      volume : int; [@key "v"]
      trade_count : int; [@key "n"]
      volume_weighted : float; [@key "vw"]
    }
    [@@deriving show { with_path = false }, yojson]

    let compare x y =
      Ptime.compare (Time.of_string x.timestamp) (Time.of_string y.timestamp)
  end

  module Bars2 = struct
    type t = (string * Bar_item.t list) list [@@deriving show]

    let t_of_yojson (x : Yojson.Safe.t) =
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

    let yojson_of_t _ = invalid_arg "NYI Bars.yojson_of_t"
  end

  type t = {
    bars : Bars2.t;
    next_page_token : string option; [@default None]
    currency : string option; [@default None]
  }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

  let combine (l : t list) : t =
    let keys =
      List.flat_map (fun x -> List.Assoc.keys x.bars) l
      |> List.uniq ~eq:String.equal
    in
    let get_data key =
      let data =
        List.flat_map
          (fun (x : t) ->
            match List.Assoc.get ~eq:String.equal key x.bars with
            | Some found -> found
            | None -> [])
          l
      in
      List.sort Bar_item.compare data
    in
    let bars = List.map (fun key -> (key, get_data key)) keys in
    { bars; next_page_token = None; currency = None }

  let t_of_yojson x =
    try t_of_yojson x
    with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, _j) ->
      let err = Printexc.to_string e in
      invalid_arg @@ Format.asprintf "%s" err
end
