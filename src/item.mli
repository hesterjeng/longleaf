module Order = Trading_types.Order

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
