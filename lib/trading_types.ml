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
  type t = Buy | Sell [@@deriving show, yojson, eq]

  let to_color = function Buy -> "green" | Sell -> "red"
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
  type t = Min of int | Hour of int | Day | Week | Month of int
  type conv = int option -> t

  val conv : conv Cmdliner.Arg.conv
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val pp : t Format.printer
  val to_string : t -> string
  val to_string_tiingo : t -> string
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

  let to_string_tiingo = function
    | Min i -> Format.asprintf "%dmin" i
    | Hour i -> Format.asprintf "%dhour" i
    | x ->
        invalid_arg @@ Format.asprintf "Tiingo intraday doesn't support %a" pp x

  type conv = int option -> t [@@deriving show]

  let get_interval (x : int option) =
    match x with
    | Some x -> x
    | None -> invalid_arg "Expected interval argument"

  let conv_of_string : string -> (conv, _) result = function
    | "minute" | "min" | "Minute" | "Min" ->
        Result.return @@ fun i -> Min (get_interval i)
    | "hour" | "Hour" -> Result.return @@ fun i -> Hour (get_interval i)
    | "day" | "Day" -> Result.return @@ fun _ -> Day
    | "week" | "Week" -> Result.return @@ fun _ -> Week
    | "month" | "Month" -> Result.return @@ fun i -> Month (get_interval i)
    | _ -> Result.fail @@ `Msg "Invalid timeframe selection"

  let conv = Cmdliner.Arg.conv (conv_of_string, pp_conv)

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
  [@@deriving show]

  let yojson_of_t (x : t) : Yojson.Safe.t =
    match x with
    | New -> `String "new"
    | Partially_filled -> `String "partially_filled"
    | Filled -> `String "filled"
    | Done_for_day -> `String "done_for_day"
    | Canceled -> `String "canceled"
    | Expired -> `String "expired"
    | Replaced -> `String "replaced"
    | Pending_cancel -> `String "pending_cancel"
    | Pending_replace -> `String "pending_replace"
    | Accepted -> `String "accepted"
    | Pending_new -> `String "pending_new"
    | Accepted_for_bidding -> `String "accepted_for_bidding"
    | Stopped -> `String "stopped"
    | Rejected -> `String "rejected"
    | Suspended -> `String "suspended"
    | Calculated -> `String "calculated"

  let t_of_yojson (x : Yojson.Safe.t) =
    match x with
    | `String "new" -> New
    | `String "partially_filled" -> Partially_filled
    | `String "filled" -> Filled
    | `String "done_for_day" -> Done_for_day
    | `String "canceled" -> Canceled
    | `String "expired" -> Expired
    | `String "replaced" -> Replaced
    | `String "pending_cancel" -> Pending_cancel
    | `String "pending_replace" -> Pending_replace
    | `String "accepted" -> Accepted
    | `String "pending_new" -> Pending_new
    | `String "accepted_for_bidding" -> Accepted_for_bidding
    | `String "stopped" -> Stopped
    | `String "rejected" -> Rejected
    | `String "suspended" -> Suspended
    | `String "calculated" -> Calculated
    | `String s ->
        invalid_arg @@ Format.asprintf "Unknown Status.t constructor %s" s
    | _ -> invalid_arg "Expected a string inside json for Status.t"
end
