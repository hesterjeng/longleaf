module Order = Trading_types.Order

module type LONGLEAF_MUTEX = sig
  val shutdown_mutex : bool Pmutex.t
  val data_mutex : Bars.t Pmutex.t
  val orders_mutex : Order_history.t Pmutex.t
end

module LongleafMutex () : LONGLEAF_MUTEX = struct
  let shutdown_mutex = Pmutex.make false
  let data_mutex = Pmutex.make Bars.empty
  let orders_mutex = Pmutex.make @@ Hashtbl.create 0
end

module type S = sig
  module Ticker : Ticker.S
  module LongleafMutex : LONGLEAF_MUTEX
  module Backend_position : Backend_position.S

  val get_trading_client : unit -> Piaf.Client.t
  val get_data_client : unit -> Piaf.Client.t
  val env : Eio_unix.Stdenv.base
  val is_backtest : bool

  (* val get_position : unit -> Backend_position.t *)
  val get_cash : unit -> float
  val symbols : string list
  val shutdown : unit -> unit

  (* Return the next open time if the market is closed *)
  val next_market_open : unit -> Time.t option
  val place_order : _ State.t -> Time.t -> Order.t -> unit
  val latest_bars : string list -> (Bars.t, string) result
  val last_data_bar : Bars.t option
  val liquidate : _ State.t -> unit
end

module type BACKEND_INPUT = sig
  val switch : Eio.Switch.t
  val longleaf_env : Environment.t
  val eio_env : Eio_unix.Stdenv.base
  val bars : Bars.t
  val symbols : string list
end

(* Backtesting *)
module Backtesting (Input : BACKEND_INPUT) (LongleafMutex : LONGLEAF_MUTEX) :
  S = struct
  open Trading_types
  module Ticker = Ticker.Instant
  module LongleafMutex = LongleafMutex
  module Backend_position = Backend_position.Generative ()

  let get_trading_client _ =
    invalid_arg "Backtesting does not have a trading client"

  let get_data_client _ =
    invalid_arg "Backtesting does not have a trading client"

  let next_market_open _ = None
  let env = Input.eio_env
  let symbols = Input.symbols
  let is_backtest = true
  let shutdown () = ()
  let get_cash = Backend_position.get_cash

  (* module OrderQueue = struct *)
  (*   let queue : Order.t list ref = ref [] *)
  (*   let add x = queue := [ x ] *)

  (*   let work latest_bars = *)
  (*     Eio.traceln "@[Remaining in queue: %d@]@.@[%a@]@." (List.length !queue) *)
  (*       (List.pp Order.pp) !queue; *)
  (*     let res = *)
  (*       List.filter_map *)
  (*         (fun (order : Order.t) -> *)
  (*           let symbol = order.symbol in *)
  (*           let price = order.price in *)
  (*           let current_price = Bars.price latest_bars symbol in *)
  (*           match (order.side, order.order_type) with *)
  (*           | Buy, StopLimit -> *)
  (*               Eio.traceln *)
  (* "@[StopLimit orders execution is probably really buggy!! \ *)
     (*                  Warning!!!@]@."; *)
  (*               let current_amt = *)
  (*                 Hashtbl.get position symbol |> Option.get_or ~default:0 *)
  (*               in *)
  (*               if current_price.close >. price then ( *)
  (*                 Eio.traceln *)
  (* "Executing a Buy StopLimit order because it has been \ *)
     (*                    triggered"; *)
  (*                 Hashtbl.replace position symbol (current_amt + order.qty); *)
  (*                 cash := *)
  (*                   !cash -. (current_price.close *. Float.of_int order.qty); *)
  (*                 None) *)
  (*               else Some order *)
  (*           | Sell, StopLimit -> *)
  (*               invalid_arg "Can't do sell stoplimit orders yet" *)
  (*           | _, _ -> invalid_arg "Don't know how to handle this order in queue") *)
  (*         !queue *)
  (*     in *)
  (*     Eio.traceln "@[Reminaing after work: %d@]@." (List.length res); *)
  (*     queue := res *)
  (* end *)

  let place_order state time (order : Order.t) =
    Backend_position.execute_order state time order

  let data_remaining = ref Input.bars.data

  let latest_bars _ =
    let bars = !data_remaining in
    let latest =
      if List.exists (fun (_, l) -> Vector.is_empty l) bars then None
      else
        Some
          (List.Assoc.map_values
             (fun bar_items ->
               assert (not @@ Vector.is_empty bar_items);
               Vector.make 1 @@ Vector.get bar_items 0)
             bars)
    in
    let rest =
      List.Assoc.map_values
        (fun bar_items ->
          if Vector.is_empty bar_items then bar_items
          else (
            Vector.remove_and_shift bar_items 0;
            bar_items))
        (* match bar_item_list with *)
        (*   [] -> [] *)
        (* | _ :: xs -> xs) *)
        bars
    in
    data_remaining := rest;
    match latest with
    | Some data ->
        let res = Bars.{ data; next_page_token = None; currency = None } in
        (* OrderQueue.work res; *)
        Result.return res
    | None -> Error "Backtest complete.  There is no data remaining."

  let last_data_bar =
    Some
      {
        Bars.data =
          (let res =
             List.Assoc.map_values
               (fun bar_items ->
                 match Vector.pop bar_items with
                 | Some z -> Vector.make 1 z
                 | None -> invalid_arg "Empty dataset")
               Input.bars.data
           in
           res);
        currency = None;
        next_page_token = None;
      }

  let liquidate state =
    let final_bar =
      match last_data_bar with
      | Some b -> b
      | None ->
          invalid_arg "Expected to have last data bar in backtesting backend"
    in
    Backend_position.liquidate state final_bar;
    (* Eio.traceln "@[Position:@]@.@[%a@]@." *)
    (*   (Hashtbl.pp String.pp Int.pp) *)
    (*   position.position; *)
    ()
end

(* Live trading *)
module Alpaca
    (Input : BACKEND_INPUT)
    (Ticker : Ticker.S)
    (LongleafMutex : LONGLEAF_MUTEX) : S = struct
  open Trading_types
  module Ticker = Ticker
  module Backtesting = Backtesting (Input) (LongleafMutex)
  module LongleafMutex = Backtesting.LongleafMutex
  module Backend_position = Backtesting.Backend_position

  let get_cash = Backend_position.get_cash
  let env = Input.eio_env

  let trading_client =
    let res =
      Piaf.Client.create ~sw:Input.switch Input.eio_env
        Input.longleaf_env.apca_api_base_url
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create trading client"

  let tiingo_client = Tiingo_api.tiingo_client Input.eio_env Input.switch

  module Tiingo_client : Util.CLIENT = struct
    let longleaf_env = Input.longleaf_env
    let client = tiingo_client
  end

  module Tiingo = Tiingo_api.Make (Tiingo_client)

  let data_client =
    let res =
      Piaf.Client.create ~sw:Input.switch Input.eio_env
        Input.longleaf_env.apca_api_data_url
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create data client"

  let get_trading_client _ = trading_client
  let get_data_client _ = data_client

  module Trading_api = Trading_api.Make (struct
    let client = trading_client
    let longleaf_env = Input.longleaf_env
  end)

  module Market_data_api = Market_data_api.Make (struct
    let client = data_client
    let longleaf_env = Input.longleaf_env
  end)

  let next_market_open () =
    let clock = Trading_api.Clock.get () in
    if clock.is_open then None else Some clock.next_open

  (* let shutdown = *)
  let shutdown () =
    Eio.traceln "Alpaca backend shutdown";
    Piaf.Client.shutdown trading_client;
    Piaf.Client.shutdown data_client;
    ()

  let symbols = Input.symbols
  let is_backtest = false
  let get_account = Trading_api.Accounts.get_account
  let last_data_bar = None

  let latest_bars symbols =
    match symbols with
    | [] -> Ok Bars.empty
    | _ ->
        let _ = Backtesting.latest_bars symbols in
        (* let res = Market_data_api.Stock.latest_bars symbols in *)
        let res = Tiingo.latest symbols in
        Ok res

  let get_clock = Trading_api.Clock.get

  let place_order state time order =
    Backtesting.place_order state time order;
    Trading_api.Orders.create_market_order order

  let liquidate state =
    let symbols = Backend_position.symbols () in
    let last_data_bar =
      match latest_bars symbols with
      | Ok x -> x
      | Error _ ->
          invalid_arg
            "Unable to get price information for symbol while liquidating"
    in
    let _ =
      (* Hashtbl.map_list *)
      List.iter
        (fun symbol ->
          let qty = Backend_position.qty symbol in
          if qty = 0 then ()
          else
            let latest_info = Bars.price last_data_bar symbol in
            let timestamp = Bars.Bar_item.timestamp latest_info in
            let order : Order.t =
              let side = if qty >= 0 then Side.Sell else Side.Buy in
              let tif = TimeInForce.GoodTillCanceled in
              let order_type = OrderType.Market in
              let qty = Int.abs qty in
              let price = Bars.Bar_item.last latest_info in
              Order.make ~symbol ~side ~tif ~order_type ~qty ~price
            in
            Eio.traceln "%a" Order.pp order;
            let _json_resp = place_order state timestamp order in
            ())
        symbols
    in
    ()
end
