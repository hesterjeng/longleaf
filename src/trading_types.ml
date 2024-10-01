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
    [@@deriving show, yojson]
  end

  module Bar = struct
    type t = { ticker : string; data : Bar_item.t list } [@@deriving show]

    let t_of_yojson x = invalid_arg "NYI t_of_yojson"
    let yojson_of_t _ = invalid_arg "NYI Bar.yojson_of_t"
  end

  type t = {
    bars : Bar.t list;
    next_page_token : string option; [@default None]
    currency : string;
  }
  [@@deriving show, yojson]

  (* let t_of_yojson x = *)
  (*   Format.printf "%a" Yojson.Safe.pp x; *)
  (*   try t_of_yojson x *)
  (*   with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, _j) -> *)
  (*     let err = Printexc.to_string e in *)
  (*     invalid_arg @@ Format.asprintf "%s" err *)
end
