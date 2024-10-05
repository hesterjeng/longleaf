module State = struct
  type state =
    | Initialize
    | Listening
    | Ordering
    | Finished of Cohttp.Code.status_code

  type 'a t = { env : Environment.t; current : state; content : 'a }
  type ('a, 'b) status = Running of 'a t | Shutdown of 'b

  let continue x = Running x
  let shutdown x = Shutdown x
  let init env = { env; current = Initialize; content = 42 }
end

module type TICKER = sig
  val tick : unit -> unit Lwt.t
end

module FiveMinuteTicker : TICKER = struct
  let tick () = Lwt_unix.sleep 300.0
end

module ThirtyMinuteTicker : TICKER = struct
  let tick () = Lwt_unix.sleep 1800.0
end

module InstantTicker : TICKER = struct
  let tick () = Lwt.return_unit
end

module type BACKEND = sig
  module Ticker : TICKER

  val create_order :
    Environment.t -> Trading_types.Order.t -> Yojson.Safe.t Lwt.t

  val get_account : Environment.t -> Trading_api.Accounts.t Lwt.t
  val latest_bars : Environment.t -> string list -> Trading_types.Bars.t Lwt.t
  val latest_price : Environment.t -> string -> float Lwt.t
  val get_clock : Environment.t -> Trading_api.Clock.t Lwt.t
end

module Alpaca_backend : BACKEND = struct
  open Lwt.Syntax
  open Trading_types

  (* module Ticker = FiveMinuteTicker *)
  module Ticker = ThirtyMinuteTicker

  let create_order = Trading_api.Orders.create_market_order
  let get_account = Trading_api.Accounts.get_account
  let latest_bars = Market_data_api.Stock.latest_bars
  let get_clock = Trading_api.Clock.get

  let latest_price env ticker =
    let* response = latest_bars env [ ticker ] in
    let bars = response.bars in
    match List.Assoc.get ~eq:String.equal ticker bars with
    | Some [ info ] -> Lwt.return info.closing_price
    | Some _ -> invalid_arg "Multiple bar items on latest bar?"
    | None -> invalid_arg "Unable to get price info for ticker"
end

module Make (Backend : BACKEND) = struct
  module SimpleStateMachine = struct
    let () = Random.self_init ()

    open Trading_types
    open State
    open Lwt.Syntax
    module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))

    let step (state : 'a State.t) : ('a, 'b) State.status Lwt.t =
      let env = state.env in
      match state.current with
      | Initialize ->
          Log.app (fun k -> k "Initialize");
          let* _account = Backend.get_account env in
          Lwt.return @@ continue { state with current = Listening }
      | Listening ->
          Log.app (fun k -> k "Listening");
          let* clock = Backend.get_clock env in
          let* () = Backend.Ticker.tick () in
          if not @@ clock.is_open then Lwt.return @@ continue state
          else Lwt.return @@ continue { state with current = Ordering }
      | Ordering ->
          Log.app (fun k -> k "Ordering");
          let* latest_bars = Backend.latest_bars env [ "MSFT"; "NVDA" ] in
          let msft = Bars.price latest_bars "MSFT" in
          let nvda = Bars.price latest_bars "NVDA" in
          let* () =
            if msft <. nvda then Lwt.return_unit
            else (
              Log.app (fun k -> k "Shorting NVDA");
              let order : Order.t =
                {
                  symbol = "NVDA";
                  side = Side.Sell;
                  tif = TimeInForce.Day;
                  order_type = OrderType.Market;
                  qty = 1;
                }
              in
              let* _json_resp = Backend.create_order env order in
              Lwt.return_unit)
          in
          Lwt.return @@ continue { state with current = Listening }
      | Finished code -> Lwt.return @@ shutdown code

    let run env =
      let init = init env in
      let rec go prev =
        let* stepped = step prev in
        match stepped with
        | Running now -> go now
        | Shutdown code -> Lwt.return code
      in
      go init
  end
end
