module Run_context = struct
  type 'a t = {
    strategy : 'a;
    runtype : Options.Runtype.t;
    eio_env : Eio_unix.Stdenv.base; [@opaque]
    longleaf_env : Environment.t; [@opaque]
    switch : Eio.Switch.t; [@opaque]
    preload : Options.Preload.t;
    target : string option;
    save_received : bool;
    nowait_market_open : bool;
    mutices : Longleaf_mutex.t;
    save_to_file : bool;
  }
  [@@deriving show]
end

module Run_options = struct
  type t = {
    symbols : string list;
    tick : float;
    overnight : bool;
    resume_after_liquidate : bool;
    indicators_config : Indicators.Config.t;
    dropout : bool;
    randomized_backtest_length : int;
  }
end

module type BACKEND_INPUT = sig
  val switch : Eio.Switch.t
  val longleaf_env : Environment.t
  val eio_env : Eio_unix.Stdenv.base

  val bars : Bars.t
  (** Historical information, ordered with in time order *)

  val symbols : string list
  (** The symbols that will be traded on *)

  val tick : float
  (** The interval of time that the strategy operates on. i/e we will wait ten
      minutes then do something, etc. *)

  val overnight : bool
  (** Allow holding positions overnight *)

  val save_received : bool
  (** Save the received data *)

  val resume_after_liquidate : bool
  (** Allow the strategy to resume after liquidating the position *)

  val target : Bars.t option
  (** The target is the bars that will be iterated over in a backtest Ordered in
      reverse time order, so that we can pop off next values easily *)

  val mutices : Longleaf_mutex.t
  (** Mutices for delivering information to GUI *)

  val runtype : Options.Runtype.t
  (** Record of options *)

  val indicators_config : Indicators.Config.t
  (** Indicators options *)

  val dropout : bool
  (** Are we going to randomly drop orders for testing? *)

  val save_to_file : bool
  (** Save info to files *)
end

module type S = sig
  module Backend_position : Backend_position.S
  module Input : BACKEND_INPUT

  (* Is this backend a backtest? *)
  val is_backtest : bool

  (* TODO: Do something with this? *)
  val overnight : bool

  (* Save data that is received in a live/paper run *)
  (* val save_received : bool *)
  val received_data : Bars.t
  val get_trading_client : unit -> (Piaf.Client.t, string) result
  val get_data_client : unit -> (Piaf.Client.t, string) result
  val env : Eio_unix.Stdenv.base
  val init_state : 'a -> 'a State.t
  val get_cash : unit -> float
  val symbols : string list
  val shutdown : unit -> unit

  (* Return the next open time if the market is closed *)
  val next_market_open : unit -> Time.t option
  val next_market_close : unit -> Time.t
  val place_order : _ State.t -> Order.t -> (unit, string) result
  val latest_bars : string list -> (Bars.Latest.t, string) result
  val last_data_bar : (Bars.Latest.t, string) result
  val liquidate : _ State.t -> (unit, string) Result.t
end
