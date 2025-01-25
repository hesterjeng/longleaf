module Run_options = struct
  type t = {
    symbols : string list;
    tick : float;
    overnight : bool;
    resume_after_liquidate : bool;
    runtype : Options.Runtype.t;
    indicators_config : Indicators.Config.t;
    dropout : bool;
    randomized_backtest_length : int;
  }
end

module Run_context = struct
  type t = {
    eio_env : Eio_unix.Stdenv.base; [@opaque]
    longleaf_env : Environment.t; [@opaque]
    switch : Eio.Switch.t; [@opaque]
    preload : Options.Preload.t;
    target : string option;
    save_received : bool;
    mutices : Longleaf_mutex.t;
    save_to_file : bool;
  }
  [@@deriving show]
end

module type BACKEND_INPUT = sig
  val switch : Eio.Switch.t
  val longleaf_env : Environment.t
  val eio_env : Eio_unix.Stdenv.base

  (* Historical information, ordered with in time order *)
  val bars : Bars.t

  (* The symbols that will be traded on *)
  val symbols : string list

  (* The interval of time that the strategy operates on. *)
  (* i/e we will wait ten minutes then do something, etc. *)
  val tick : float

  (* Allow holding positions overnight *)
  val overnight : bool

  (* Save the received data *)
  val save_received : bool

  (* Allow the strategy to resume after liquidating the position *)
  val resume_after_liquidate : bool

  (* The target is the bars that will be iterated over in a backtest *)
  (* Ordered in reverse time order, so that we can pop off next values easily *)
  val target : Bars.t option

  (* Mutices for delivering information to GUI *)
  val mutices : Longleaf_mutex.t

  (* Record of options *)
  val runtype : Options.Runtype.t

  (* Indicators options *)
  val indicators_config : Indicators.Config.t

  (* Are we going to randomly drop orders for testing? *)
  val dropout : bool

  (* Save info to files *)
  val save_to_file : bool
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
