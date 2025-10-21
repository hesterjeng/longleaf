module Options = Longleaf_core.Options
module Bars = Longleaf_bars
module State = Longleaf_state

module type BACKEND_INPUT = sig
  val options : Options.t

  (* val bars : Bars.t *)
  (* (\** Historical information, ordered with in time order *\) *)

  val target : Bars.t option
  (** The data to be iterated over *)

  val mutices : Longleaf_state.Mutex.t
end

module type S = sig
  (* module Backend_position : Backend_position.S *)
  module Input : BACKEND_INPUT

  (* Is this backend a backtest? *)
  val is_backtest : bool

  (* TODO: Do something with this? *)
  (* val overnight : bool *)

  (* Save data that is received in a live/paper run *)
  (* val save_received : bool *)
  val received_data : Bars.t
  val get_trading_client : unit -> (Cohttp_eio.Client.t, Error.t) result
  val get_data_client : unit -> (Cohttp_eio.Client.t, Error.t) result
  val init_state : unit -> (State.t, Error.t) result
  val symbols : Instrument.t list
  val shutdown : unit -> unit

  (* Return the next open time if the market is closed *)
  val next_market_open : unit -> (Time.t option, Error.t) result
  val next_market_close : unit -> (Time.t, Error.t) result
  val place_order : State.t -> Order.t -> (State.t, Error.t) result
  val update_bars : State.t -> (State.t, Error.t) result
  val last_data_bar : (Bars.Latest.t, Error.t) result
  val liquidate : State.t -> (State.t, Error.t) Result.t
end
