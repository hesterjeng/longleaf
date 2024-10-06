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

  val backtesting : bool
  val get_cash : unit -> float
  val get_position : unit -> (string, int) Hashtbl.t

  val create_order :
    Environment.t -> Trading_types.Order.t -> Yojson.Safe.t Lwt.t

  (* val get_account : Environment.t -> Trading_api.Accounts.t Lwt.t *)
  val latest_bars : Environment.t -> string list -> Trading_types.Bars.t Lwt.t
  (* val latest_price : Environment.t -> string -> float Lwt.t *)
  (* val get_clock : Environment.t -> Trading_api.Clock.t Lwt.t *)
end

module type BACKEND_INPUT = sig
  val bars : Trading_types.Bars.t
end

(* Backtesting *)
module Backtesting_backend (Data : BACKEND_INPUT) : BACKEND = struct
  open Trading_types
  module Ticker = InstantTicker

  let backtesting = true
  let position : (string, int) Hashtbl.t = Hashtbl.create 0
  let cash = ref 100000.0
  let get_cash () = !cash
  let get_position () = position

  let create_order _ (x : Order.t) : Yojson.Safe.t Lwt.t =
    let symbol = x.symbol in
    let current_amt = Hashtbl.get position symbol |> Option.get_or ~default:0 in
    let qty = x.qty in
    match (x.side, x.order_type) with
    | Buy, Market ->
        Hashtbl.replace position symbol (current_amt + qty);
        cash := !cash -. (x.price *. Float.of_int qty);
        Lwt.return `Null
    | Sell, Market ->
        Hashtbl.replace position symbol (current_amt - qty);
        cash := !cash +. (x.price *. Float.of_int qty);
        Lwt.return `Null
    | _, _ -> invalid_arg "Backtesting can't handle this yet."

  (* TODO: Massage historical bars to give them in a format we can use *)
  (* for price data *)
  let latest_bars _ _ =
    let bars = Data.bars in
    Format.printf "%a" Bars.pp bars;
    invalid_arg "NYI"
end

(* Live trading *)
module Alpaca_backend : BACKEND = struct
  open Lwt.Syntax
  open Trading_types
  module Ticker = ThirtyMinuteTicker

  module Backtesting_backend = Backtesting_backend (struct
    let bars = Bars.empty
  end)

  let backtesting = false
  let get_account = Trading_api.Accounts.get_account
  let latest_bars = Market_data_api.Stock.latest_bars
  let get_clock = Trading_api.Clock.get
  let get_cash = Backtesting_backend.get_cash
  let get_position = Backtesting_backend.get_position

  (* let latest_price env ticker = *)
  (*   let* response = latest_bars env [ ticker ] in *)
  (*   let bars = response.bars in *)
  (*   match List.Assoc.get ~eq:String.equal ticker bars with *)
  (*   | Some [ info ] -> Lwt.return info.closing_price *)
  (*   | Some _ -> invalid_arg "Multiple bar items on latest bar?" *)
  (*   | None -> invalid_arg "Unable to get price info for ticker" *)

  let create_order env order =
    let* res = Trading_api.Orders.create_market_order env order in
    let* _ = Backtesting_backend.create_order env order in
    Lwt.return res
end

module type STRAT = sig
  val run : Environment.t -> Cohttp.Code.status_code Lwt.t
end

module SimpleStateMachine (Backend : BACKEND) : STRAT = struct
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
        Lwt.return @@ continue { state with current = Listening }
    | Listening ->
        let* () = Backend.Ticker.tick () in
        Lwt.return @@ continue { state with current = Ordering }
    | Ordering ->
        Log.app (fun k -> k "Ordering");
        let* latest_bars = Backend.latest_bars env [ "MSFT"; "NVDA" ] in
        let msft = Bars.price latest_bars "MSFT" in
        let nvda = Bars.price latest_bars "NVDA" in
        let cash_available = Backend.get_cash () in
        (* Buy as many shares as possible with 10% of cash *)
        let qty =
          match cash_available >=. 0.0 with
          | true ->
              let tenp = cash_available *. 0.1 in
              let max_amt = tenp /. nvda in
              if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
          | false -> 0
        in
        let* () =
          if msft <. nvda then Lwt.return_unit
          else
            let order : Order.t =
              {
                symbol = "NVDA";
                side = Side.Buy;
                tif = TimeInForce.Day;
                order_type = OrderType.Market;
                qty;
                price = nvda;
              }
            in
            let* _json_resp = Backend.create_order env order in
            Lwt.return_unit
        in
        Lwt.return @@ continue { state with current = Listening }
    | Finished code ->
        Log.app (fun k -> k "cash: %f" (Backend.get_cash ()));
        Lwt.return @@ shutdown code

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
