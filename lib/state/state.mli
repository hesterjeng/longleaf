module Mode = Mode
module Config = Config
module Stats = Stats

type 'a t
type 'a res = ('a, Error.t) result

(* The current "state of the state" *)
val current : 'a t -> Mode.t

(* Create a new state with index, data, and content *)
val make : int -> Bars.t -> 'a -> 'a t res

(* Print basic info about the state *)
val pp : 'a t Format.printer

(* Export the state to a simple string *)
val show : 'a t -> string

(* Get the amount of cash available.  This value is updated only when orders are executed.*)
val cash : 'a t -> float

(* Get the time of the state machine.  Uses Bars.timestamp *)
val time : 'a t -> Time.t res

(* Get list of active orders *)
val history : 'a t -> Order.t list Vector.ro_vector

(* Cost basis to enter the corresponding position *)
val cost_basis : 'a t -> Instrument.t -> float

(* Get list of active symbols *)
val held_symbols : 'a t -> Instrument.t list

(* Get qty of instrument in current position *)
val qty : 'a t -> Instrument.t -> int

(* Get the data of the state for this symbol using Bars.get. *)
val data : 'a t -> Instrument.t -> (Bars.Data.t, Error.t) result

(* Get the bars *)
val bars : 'a t -> Bars.t

(* Get the value of the state's portfolio, plus the cash on hand *)
val value : 'a t -> float res

(* The same state but with with x.state set to Listening *)
val listen : 'a t -> 'a t

(* The same state but with with x.state set to Listening *)
val liquidate : 'a t -> 'a t

(* Place an order.  Assume that orders are filled completely and instantly at the current price. *)
val place_order : 'a t -> Order.t -> 'a t res

(* Current tick of the state machine (corresponds to the Bars.t index) *)
val tick : 'a t -> int
val increment_tick : 'a t -> 'a t
val set : 'a t -> Mode.t -> 'a t

(* Return the options record *)
val options : 'a t -> Config.t
val empty : unit -> unit t
val stats : 'a t -> Stats.t
