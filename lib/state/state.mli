module Mode = Mode
module Config = Config
module Stats = Stats
module Bars = Longleaf_bars

type t
type 'a res = ('a, Error.t) result

(* Create a new state with index, data, and content *)
val make : int -> Bars.t -> Indicators_config.t -> float -> t res

(* Print basic info about the state *)
val pp : t Format.printer

(* Export the state to a simple string *)
val show : t -> string

(* Get the amount of cash available.  This value is updated only when orders are executed.*)
val cash : t -> float

(* Get the time of the state machine.  Uses Bars.timestamp *)
val time : t -> Time.t res
val config : t -> Config.t

(* Cost basis to enter the corresponding position *)
val cost_basis : t -> Instrument.t -> float

(* Get list of active symbols *)
val held_symbols : t -> Instrument.t list

(* Get qty of instrument in current position *)
val qty : t -> Instrument.t -> int

(* Get the data of the state for this symbol using Bars.get. *)
val data : t -> Instrument.t -> (Bars.Data.t, Error.t) result

(* Get the bars *)
val bars : t -> Bars.t
val value_history : t -> (Time.t * float) list
val cash_history : t -> (Time.t * float) list

(* Get the value of the state's portfolio, plus the cash on hand *)
val value : t -> float res
val orders_placed : t -> int

(* Place an order.  Assume that orders are filled completely and instantly at the current price. *)
val place_order : t -> Order.t -> t res

(* Current tick of the state machine (corresponds to the Bars.t index) *)
val tick : t -> int
val increment_tick : t -> (t, Error.t) result
val set_tick : t -> int -> t
val grow : t -> t res

(* Return the options record *)
val options : t -> Config.t
val empty : Runtype.t -> Tacaml.t list -> t
val stats : t -> Stats.t
val positions : t -> Positions.t

module Conv : sig
  val to_tearsheet_json : t -> Yojson.Safe.t
end
