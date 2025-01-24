open Trading_types

type t = private {
  symbol : string;
  side : Side.t;
  tif : TimeInForce.t;
  order_type : OrderType.t;
  qty : int;
  price : float;
  timestamp : Time.t;
  reason : string list;
  profit : float option;
  id : string Pmutex.t;
  status : Status.t Pmutex.t;
}
[@@deriving show, yojson]

val equal : t -> t -> bool

val make :
  symbol:string ->
  side:Side.t ->
  tif:TimeInForce.t ->
  order_type:OrderType.t ->
  qty:int ->
  price:float ->
  timestamp:Time.t ->
  reason:string list ->
  profit:float option ->
  t

val timestamp : t -> Time.t
val cmp_profit : t Ord.t

val default_buy :
  ?profit:float ->
  current_cash:float ->
  price:float ->
  reason:string list ->
  timestamp:Time.t ->
  string ->
  t option

val default_sell :
  ?profit:float ->
  current_cash:float ->
  price:float ->
  reason:string list ->
  timestamp:Time.t ->
  string ->
  t option
