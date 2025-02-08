type t

val make : unit -> t
val execute_order : t -> Order.t -> (t, Error.t) Result.t
val liquidate : t -> Bars.Latest.t -> (t, Error.t) Result.t
val get_cash : t -> float
val set_cash : t -> float -> t
val symbols : t -> string list
val qty : t -> string -> int
val value : t -> Bars.Latest.t -> float
