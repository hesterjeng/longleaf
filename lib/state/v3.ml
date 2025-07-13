type state =
  | Initialize
  | Listening
  | Ordering
  | Liquidate
  | LiquidateContinue
  | Continue
  | BeginShutdown
  | Finished of string
[@@deriving show { with_path = false }]

module State_config = struct
  type t = { placeholder : bool }
end

module type S = sig
  type 'a t
  type 'a res = ('a, Error.t) result

  module type Internal = sig
    val record_order : 'a t -> int -> 'a t
    val fill_order : 'a t -> Order.t -> 'a t
  end

  (* The current "state of the state" *)
  val current : 'a t -> state

  (* Create a new state with index, data, and content *)
  val make : int -> Bars.t -> 'a -> 'a t res

  (* Print basic info about the state *)
  val pp : 'a t Format.printer

  (* Export the state to a simple string *)
  val show : 'a t -> string

  (* Get the amount of cash available.  This value is updated only when orders are executed.*)
  val cash : 'a t -> float res

  (* Get the time of the state machine.  Uses Bars.timestamp *)
  val time : 'a t -> Time.t res

  (* Get list of active orders *)
  val orders : 'a t -> Order.t Vector.ro_vector

  (* Get the data of the state for this symbol using Bars.get. *)
  val data : 'a t -> Instrument.t -> (Bars.Data.t, Error.t) result

  (* Get the value of the state's portfolio, plus the cash on hand *)
  val value : 'a t -> float res

  (* The same state but with with x.state set to Listening *)
  val listen : 'a t -> 'a t

  (* Place an order.  Assume that orders are filled completely and instantly at the current price. *)
  val place_order : 'a t -> Order.t -> 'a t res

  (* Current tick of the state machine (corresponds to the Bars.t index) *)
  val tick : 'a t -> int

  (* Return the options record *)
  val options : 'a t -> State_config.t
end
