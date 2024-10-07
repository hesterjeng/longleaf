module State = struct
  type state =
    | Initialize
    | Listening
    | Ordering
    | Liquidate
    | Finished of string

  type 'a t = { env : Environment.t; current : state; content : 'a }
  type ('a, 'b) status = Running of 'a t | Shutdown of 'b

  let continue x = Running x
  let shutdown x = Shutdown x

  let init env =
    { env; current = Initialize; content = Trading_types.Bars.empty }
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

module SimpleStateMachine (Backend : BACKEND) : STRAT = struct
  let () = Random.self_init ()

  open Trading_types
  open State
  open Lwt_result.Syntax
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))

  let ok_code = Cohttp.Code.status_of_code 200

  (* TODO: Handle market closed with live backend rather than requesting all through the night *)
  let step (state : 'a State.t) : (('a, 'b) State.status, string) Lwt_result.t =
    let env = state.env in
    match state.current with
    | Initialize ->
        Log.app (fun k -> k "Running");
        Lwt_result.return @@ continue { state with current = Listening }
    | Listening ->
        let open CalendarLib in
        let calendar = Calendar.now () in
        let time =
          Calendar.Time.lmake ~hour:(Calendar.hour calendar)
            ~minute:(Calendar.minute calendar) ()
        in
        let open_time = Calendar.Time.lmake ~hour:8 ~minute:30 () in
        let close_time = Calendar.Time.lmake ~hour:16 () in
        let* () =
          let ( < ) x y = Calendar.Time.compare x y in
          if
            Calendar.Time.compare open_time time = 1
            && Calendar.Time.compare time close_time = -1
          then Lwt_result.return ()
          else
            Lwt_result.ok
              (Log.app (fun k -> k "Waiting because market is closed");
               Lwt_unix.sleep 300.0)
        in
        let* () = Backend.Ticker.tick () in
        Lwt_result.return @@ continue { state with current = Ordering }
    | Liquidate ->
        Log.app (fun k -> k "Liquidate");
        let* _ = Backend.liquidate env in
        Lwt_result.return
        @@ continue { state with current = Finished "Successfully liquidated" }
    | Finished code ->
        let json =
          Trading_types.Bars.yojson_of_t state.content |> Yojson.Safe.to_string
        in
        let filename = Format.sprintf "data/live_%s" (Util.rfc339 ()) in
        let oc = open_out filename in
        output_string oc json;
        close_out oc;
        Log.app (fun k -> k "cash: %f" (Backend.get_cash ()));
        Lwt_result.return @@ shutdown code
    | Ordering ->
        let* latest_bars = Backend.latest_bars env [ "MSFT"; "NVDA" ] in
        let msft = Bars.price latest_bars "MSFT" in
        let nvda = Bars.price latest_bars "NVDA" in
        let cash_available = Backend.get_cash () in
        let qty =
          match cash_available >=. 0.0 with
          | true ->
              let tenp = cash_available *. 0.5 in
              let max_amt = tenp /. nvda in
              if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
          | false -> 0
        in
        (* Actually do the trade *)
        let* () =
          if msft <. nvda then Lwt_result.return ()
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
            Lwt_result.return ()
        in
        let new_bars = Bars.combine [ latest_bars; state.content ] in
        let* () = Lwt_result.ok @@ Lwt_unix.sleep 0.01 in
        Lwt_result.return
        @@ continue { state with current = Listening; content = new_bars }

  let run env =
    let init = init env in
    let open Lwt.Syntax in
    let rec go prev =
      let* stepped = step prev in
      match stepped with
      | Ok x -> (
          match x with
          | Running now -> go now
          | Shutdown code -> Lwt.return code)
      | Error s ->
          let liquidate = { prev with current = Liquidate } in
          let* liquidated = go liquidate in
          Log.app (fun k -> k "%s" liquidated);
          Lwt.return s
    in
    go init
end
