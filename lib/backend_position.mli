type t

val make : unit -> t
val execute_order : t -> Order.t -> (t, Error.t) Result.t
val liquidate : t -> Bars.Latest.t -> (t, Error.t) Result.t
val get_cash : t -> float
val set_cash : t -> float -> t
val symbols : t -> Instrument.t list
val qty : t -> Instrument.t -> int
val value : t -> Bars.Latest.t -> (float, Error.t) Result.t
val is_empty : t -> bool
val pp : t Format.printer
val update : t -> Bars.t -> int -> (t, Error.t) Result.t
