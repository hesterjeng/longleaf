module TimeInForce = struct
  type t =
    | Day
    | GoodTillCanceled
    | Opening
    | Close
    | ImmediateOrCancel
    | FillOrKill
  [@@deriving show, yojson]

  let to_string = function
    | Day -> "day"
    | GoodTillCanceled -> "gtc"
    | Opening -> "opg"
    | Close -> "cls"
    | ImmediateOrCancel -> "ioc"
    | FillOrKill -> "fok"
end

module Side = struct
  type t = Buy | Sell [@@deriving show, yojson]

  let to_string = function Buy -> "buy" | Sell -> "sell"
end

module OrderType = struct
  type t = Market | Limit | Stop | StopLimit | TrailingStop
  [@@deriving show, yojson]

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

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val pp : t Format.printer
  val to_string : t -> string
  val min : int -> t
  val hour : int -> t
  val day : t
  val week : t
  val month : int -> t
end = struct
  type t = Min of int | Hour of int | Day | Week | Month of int
  [@@deriving show, yojson]

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

module Order = struct
  module Status = struct
    type t =
      | New_
      | Partially_filled
      | Filled
      | Done_for_day
      | Canceled
      | Expired
      | Replaced
      | Pending_cancel
      | Pending_replace
      | Accepted
      | Pending_new
      | Accepted_for_bidding
      | Stopped
      | Rejected
      | Suspended
      | Calculated
    [@@deriving show, yojson]
    (* | new_ *)
    (* | partially_filled *)
    (* | filled *)
    (* | done_for_day *)
    (* | canceled *)
    (* | expired *)
    (* | replaced *)
    (* | pending_cancel *)
    (* | pending_replace *)
    (* | accepted *)
    (* | pending_new *)
    (* | accepted_for_bidding *)
    (* | stopped *)
    (* | rejected *)
    (* | suspended *)
    (* | calculated *)
  end

  type t = {
    symbol : string;
    side : Side.t;
    tif : TimeInForce.t;
    order_type : OrderType.t;
    qty : int;
    price : float;
  }
  [@@deriving show, yojson]

  module Response = struct
    type t = { id : string; status : string }

    let filled x = x.status
  end
end
