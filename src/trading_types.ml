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

module Status = struct
  type t =
    | New [@name "new"]
    | Partially_filled [@name "partially_filled"]
    | Filled [@name "filled"]
    | Done_for_day [@name "done_for_day"]
    | Canceled [@name "canceled"]
    | Expired [@name "expired"]
    | Replaced [@name "replaced"]
    | Pending_cancel [@name "pending_cancel"]
    | Pending_replace [@name "pending_replace"]
    | Accepted [@name "accepted"]
    | Pending_new [@name "pending_new"]
    | Accepted_for_bidding [@name "accepted_for_bidding"]
    | Stopped [@name "stopped"]
    | Rejected [@name "rejected"]
    | Suspended [@name "suspended"]
    | Calculated [@name "calculated"]
  [@@deriving show, yojson]
end

module Order : sig
  type t = private {
    symbol : string;
    side : Side.t;
    tif : TimeInForce.t;
    order_type : OrderType.t;
    qty : int;
    price : float;
    timestamp : Time.t;
    id : string Pmutex.t;
    status : Status.t Pmutex.t;
  }
  [@@deriving show, yojson]

  val make :
    symbol:string ->
    side:Side.t ->
    tif:TimeInForce.t ->
    order_type:OrderType.t ->
    qty:int ->
    price:float ->
    timestamp:Time.t ->
    t
end = struct
  type t = {
    symbol : string;
    side : Side.t;
    tif : TimeInForce.t;
    order_type : OrderType.t;
    qty : int;
    price : float;
    timestamp : Time.t;
    id : string Pmutex.t;
    status : Status.t Pmutex.t;
  }
  [@@deriving show, yojson]

  let make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp =
    {
      symbol;
      side;
      tif;
      order_type;
      qty;
      price;
      timestamp;
      id = Pmutex.make "id_not_set";
      status = Pmutex.make Status.New;
    }
end
