module type S = sig
  module Ticker : Ticker.S

  val is_backtest : bool
  val get_cash : unit -> float
  val get_position : unit -> (string, int) Hashtbl.t
  val symbols : string list
  val shutdown : unit -> unit
  val create_order : Environment.t -> Trading_types.Order.t -> Yojson.Safe.t

  val latest_bars :
    Environment.t -> string list -> (Trading_types.Bars.t, string) result

  val last_data_bar : Trading_types.Bars.t option
  val liquidate : Environment.t -> unit
end

module type BACKEND_INPUT = sig
  val bars : Trading_types.Bars.t
  val symbols : string list
end

(* Backtesting *)
module Backtesting (Input : BACKEND_INPUT) : S = struct
  open Trading_types
  module Ticker = Ticker.InstantLwt

  let symbols = Input.symbols
  let is_backtest = true
  let position : (string, int) Hashtbl.t = Hashtbl.create 0
  let cash = ref 100000.0
  let get_cash () = !cash
  let get_position () = position
  let shutdown () = ()

  let create_order _ (x : Order.t) : Yojson.Safe.t =
    let symbol = x.symbol in
    let current_amt = Hashtbl.get position symbol |> Option.get_or ~default:0 in
    let qty = x.qty in
    match (x.side, x.order_type) with
    | Buy, Market ->
        Hashtbl.replace position symbol (current_amt + qty);
        cash := !cash -. (x.price *. Float.of_int qty);
        `Null
    | Sell, Market ->
        Hashtbl.replace position symbol (current_amt - qty);
        cash := !cash +. (x.price *. Float.of_int qty);
        `Null
    | _, _ -> invalid_arg "Backtesting can't handle this yet."

  let data_remaining = ref Input.bars.bars

  let latest_bars _ _ =
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
        Result.return Bars.{ bars = x; next_page_token = None; currency = None }
    | None -> Error "Unable to find latest bars in backtest"

  let last_data_bar =
    Some
      {
        Trading_types.Bars.bars =
          List.Assoc.map_values
            (fun bar_item_list ->
              match List.last_opt bar_item_list with
              | Some z -> [ z ]
              | None -> invalid_arg "Empty dataset")
            Input.bars.bars;
        currency = None;
        next_page_token = None;
      }

  let liquidate env =
    let position = get_position () in
    if List.is_empty @@ Hashtbl.keys_list position then ()
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
            if qty = 0 then ()
            else
              let order : Order.t =
                {
                  symbol;
                  side = (if qty >= 0 then Side.Sell else Side.Buy);
                  tif = TimeInForce.GoodTillCanceled;
                  order_type = OrderType.Market;
                  qty;
                  price = (Bars.price final_bar symbol).close;
                }
              in
              let _json_resp = create_order env order in
              ())
          position
      in
      ()
end

(* Live trading *)
module Alpaca (Input : BACKEND_INPUT) : S = struct
  open Trading_types
  module Ticker = Ticker.FiveMinuteLwt
  module Backtesting = Backtesting (Input)

  (* let shutdown = *)
  let shutdown () = ()
  let symbols = Input.symbols
  let is_backtest = false
  let get_account = Trading_api.Accounts.get_account
  let last_data_bar = None

  let latest_bars x y =
    let res = Market_data_api.Stock.latest_bars x y in
    Ok res

  let get_clock = Trading_api.Clock.get
  let get_cash = Backtesting.get_cash
  let get_position = Backtesting.get_position

  let create_order env order =
    let res = Trading_api.Orders.create_market_order env order in
    let _ = Backtesting.create_order env order in
    res

  let liquidate env =
    let position = get_position () in
    if List.is_empty @@ Hashtbl.keys_list position then ()
    else
      let symbols = Hashtbl.keys_list position in
      let last_data_bar =
        match latest_bars env symbols with
        | Ok x -> x
        | Error _ ->
            invalid_arg
              "Unable to get price information for symbol while liquidating"
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
                  price = (Bars.price last_data_bar symbol).close;
                }
              in
              let _json_resp = create_order env order in
              Lwt_result.return ())
          position
      in
      ()
end
