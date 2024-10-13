module State = struct
  type state =
    [ `Initialize | `Listening | `Ordering | `Liquidate | `Finished of string ]

  type 'a t = { env : Environment.t; current : state; content : 'a }
  type ('a, 'b) status = Running of 'a t | Shutdown of 'b

  let continue x = Running x
  let shutdown x = Shutdown x

  let init env =
    { env; current = `Initialize; content = Trading_types.Bars.empty }
end

module type TICKER = sig
  val tick : unit -> (unit, _) Lwt_result.t
end

module FiveMinuteTicker : TICKER = struct
  let tick () = Lwt_unix.sleep 300.0 |> Lwt_result.ok
end

module ThirtyMinuteTicker : TICKER = struct
  let tick () = Lwt_unix.sleep 1800.0 |> Lwt_result.ok
end

module InstantTicker : TICKER = struct
  let tick () = Lwt.return_unit |> Lwt_result.ok
end

module type BACKEND = sig
  module Ticker : TICKER

  val backtesting : bool
  val get_cash : unit -> float
  val get_position : unit -> (string, int) Hashtbl.t

  val create_order :
    Environment.t ->
    Trading_types.Order.t ->
    (Yojson.Safe.t, string) Lwt_result.t

  val latest_bars :
    Environment.t -> string list -> (Trading_types.Bars.t, string) Lwt_result.t

  val last_data_bar : Trading_types.Bars.t option
  val liquidate : Environment.t -> (unit, string) Lwt_result.t
end

module type BACKEND_INPUT = sig
  val bars : Trading_types.Bars.t
end

(* Backtesting *)
module Backtesting_backend (Data : BACKEND_INPUT) : BACKEND = struct
  open Trading_types
  open Lwt_result.Syntax
  module Ticker = InstantTicker

  let backtesting = true
  let position : (string, int) Hashtbl.t = Hashtbl.create 0
  let cash = ref 100000.0
  let get_cash () = !cash
  let get_position () = position

  let create_order _ (x : Order.t) : (Yojson.Safe.t, string) Lwt_result.t =
    let symbol = x.symbol in
    let current_amt = Hashtbl.get position symbol |> Option.get_or ~default:0 in
    let qty = x.qty in
    match (x.side, x.order_type) with
    | Buy, Market ->
        Hashtbl.replace position symbol (current_amt + qty);
        cash := !cash -. (x.price *. Float.of_int qty);
        Lwt_result.return `Null
    | Sell, Market ->
        Hashtbl.replace position symbol (current_amt - qty);
        cash := !cash +. (x.price *. Float.of_int qty);
        Lwt_result.return `Null
    | _, _ -> invalid_arg "Backtesting can't handle this yet."

  let data_remaining = ref Data.bars.bars

  let latest_bars _ _ : (Bars.t, string) Lwt_result.t =
    let bars = !data_remaining in
    let latest =
      if List.exists (fun (_, l) -> List.is_empty l) bars then None
      else
        Some
          (List.Assoc.map_values
             (fun bar_item_list ->
               match bar_item_list with
               | [] -> invalid_arg "latest_bars"
               | x :: _ -> [ x ])
             bars)
    in
    let rest =
      List.Assoc.map_values
        (fun bar_item_list ->
          match bar_item_list with [] -> [] | _ :: xs -> xs)
        bars
    in
    data_remaining := rest;
    match latest with
    | Some x ->
        Lwt_result.return
        @@ Bars.{ bars = x; next_page_token = None; currency = None }
    | None ->
        Lwt_result.fail
          "No latest bars in backend, empty data or backtest finished"

  let last_data_bar =
    Some
      {
        Trading_types.Bars.bars =
          List.Assoc.map_values
            (fun bar_item_list ->
              match List.last_opt bar_item_list with
              | Some z -> [ z ]
              | None -> invalid_arg "Empty dataset")
            Data.bars.bars;
        currency = None;
        next_page_token = None;
      }

  let liquidate env : (unit, string) Lwt_result.t =
    let position = get_position () in
    if List.is_empty @@ Hashtbl.keys_list position then Lwt_result.return ()
    else
      let final_bar =
        match last_data_bar with
        | Some b -> b
        | None ->
            invalid_arg "Expected to have last data bar in backtesting backend"
      in
      let _ =
        Hashtbl.map_list
          (fun symbol qty ->
            if qty = 0 then Lwt_result.return ()
            else
              let order : Order.t =
                {
                  symbol;
                  side = (if qty >= 0 then Side.Sell else Side.Buy);
                  tif = TimeInForce.GoodTillCanceled;
                  order_type = OrderType.Market;
                  qty;
                  price = Bars.price final_bar symbol;
                }
              in
              let* _json_resp = create_order env order in
              Lwt_result.return ())
          position
      in
      Lwt_result.return ()
end

(* Live trading *)
module Alpaca_backend : BACKEND = struct
  open Lwt_result.Syntax
  open Trading_types
  module Ticker = ThirtyMinuteTicker

  module Backtesting_backend = Backtesting_backend (struct
    let bars = Bars.empty
  end)

  let backtesting = false
  let get_account = Trading_api.Accounts.get_account
  let last_data_bar = None

  let latest_bars x y =
    let* res = Market_data_api.Stock.latest_bars x y in
    Lwt_result.return res

  let get_clock = Trading_api.Clock.get
  let get_cash = Backtesting_backend.get_cash
  let get_position = Backtesting_backend.get_position

  let create_order env order =
    let* res = Trading_api.Orders.create_market_order env order in
    let* _ = Backtesting_backend.create_order env order in
    Lwt_result.return res

  let liquidate env =
    let position = get_position () in
    if List.is_empty @@ Hashtbl.keys_list position then Lwt_result.return ()
    else
      let symbols = Hashtbl.keys_list position in
      let* last_data_bar = latest_bars env symbols in
      let _ =
        Hashtbl.map_list
          (fun symbol qty ->
            if qty = 0 then Lwt_result.return ()
            else
              let order : Order.t =
                {
                  symbol;
                  side = (if qty >= 0 then Side.Sell else Side.Buy);
                  tif = TimeInForce.GoodTillCanceled;
                  order_type = OrderType.Market;
                  qty;
                  price = Bars.price last_data_bar symbol;
                }
              in
              let* _json_resp = create_order env order in
              Lwt_result.return ())
          position
      in
      Lwt_result.return ()
end

module type STRAT = sig
  val run : Environment.t -> string Lwt.t
end
