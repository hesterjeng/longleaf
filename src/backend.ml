module type MUTEX_VAL = sig
  val set_mutex : unit -> unit
  val get_mutex : unit -> bool
end

module type MUTEX = sig
  module Shutdown : MUTEX_VAL
  module RunInfo : MUTEX_VAL
end

module Mutex = struct
  module Make (INPUT : sig
    type t

    val default : t
  end) =
  struct
    type t = { mutable data : INPUT.t; mutex : Eio.Mutex.t }

    let t = { data = INPUT.default; mutex = Eio.Mutex.create () }

    let set_mutex value =
      Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () -> t.data <- value

    let get_mutex () = Eio.Mutex.use_ro t.mutex @@ fun () -> t.data
  end

  module Shutdown = Make (struct
    type t = bool

    let default = false
  end)

  module RunInfo = Make (struct
    type t = Bars.t

    let default = Bars.empty
  end)
end

module type S = sig
  module Ticker : Ticker.S
  module Mutex : MUTEX

  val get_trading_client : unit -> Piaf.Client.t
  val get_data_client : unit -> Piaf.Client.t
  val env : Eio_unix.Stdenv.base
  val is_backtest : bool
  val get_cash : unit -> float
  val get_position : unit -> (string, int) Hashtbl.t
  val symbols : string list
  val shutdown : unit -> unit
  val create_order : Trading_types.Order.t -> Yojson.Safe.t
  val latest_bars : string list -> (Bars.t, string) result
  val last_data_bar : Bars.t option
  val liquidate : unit -> unit
end

module type BACKEND_INPUT = sig
  val switch : Eio.Switch.t
  val longleaf_env : Environment.t
  val eio_env : Eio_unix.Stdenv.base
  val bars : Bars.t
  val symbols : string list

  module Mutex : MUTEX
end

(* Backtesting *)
module Backtesting (Input : BACKEND_INPUT) : S = struct
  open Trading_types
  module Ticker = Ticker.Instant
  module Mutex = Input.Mutex

  let get_trading_client _ =
    invalid_arg "Backtesting does not have a trading client"

  let get_data_client _ =
    invalid_arg "Backtesting does not have a trading client"

  let env = Input.eio_env
  let symbols = Input.symbols
  let is_backtest = true
  let position : (string, int) Hashtbl.t = Hashtbl.create 0
  let cash = ref 100000.0
  let get_cash () = !cash
  let get_position () = position
  let shutdown () = ()

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

  let create_order (x : Order.t) : Yojson.Safe.t =
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
    (* | Buy, StopLimit -> *)
    (*     `Null *)
    | side, order_type ->
        invalid_arg
        @@ Format.asprintf "@[Backtesting can't handle this yet. %a %a@]@."
             Side.pp side OrderType.pp order_type

  let data_remaining = ref Input.bars.data

  let latest_bars _ =
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
    | Some data ->
        let res = Bars.{ data; next_page_token = None; currency = None } in
        (* OrderQueue.work res; *)
        Result.return res
    | None -> Error "Backtest complete.  There is no data remaining."

  let last_data_bar =
    Some
      {
        Bars.data =
          List.Assoc.map_values
            (fun bar_item_list ->
              match List.last_opt bar_item_list with
              | Some z -> [ z ]
              | None -> invalid_arg "Empty dataset")
            Input.bars.data;
        currency = None;
        next_page_token = None;
      }

  let liquidate () =
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
              let side = if qty >= 0 then Side.Sell else Side.Buy in
              let order : Order.t =
                {
                  symbol;
                  side;
                  tif = TimeInForce.GoodTillCanceled;
                  order_type = OrderType.Market;
                  qty = Int.abs qty;
                  price = (Bars.price final_bar symbol).close;
                }
              in
              Eio.traceln "@[%a@]@." Order.pp order;
              let _json_resp = create_order order in
              ())
          position
      in
      Eio.traceln "@[Position:@]@.@[%a@]@."
        (Hashtbl.pp String.pp Int.pp)
        position;
      ()
end

(* Live trading *)
module Alpaca (Input : BACKEND_INPUT) (Ticker : Ticker.S) : S = struct
  open Trading_types
  module Ticker = Ticker
  module Backtesting = Backtesting (Input)
  module Mutex = Backtesting.Mutex

  let env = Input.eio_env

  let trading_client =
    let res =
      Piaf.Client.create ~sw:Input.switch Input.eio_env
        Input.longleaf_env.apca_api_base_url
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create trading client"

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
    let _ = Backtesting.latest_bars symbols in
    let res = Market_data_api.Stock.latest_bars symbols in
    Ok res

  let get_clock = Trading_api.Clock.get
  let get_cash = Backtesting.get_cash
  let get_position = Backtesting.get_position

  let create_order order =
    let res = Trading_api.Orders.create_market_order order in
    let _ = Backtesting.create_order order in
    res

  let liquidate () =
    let position = get_position () in
    if List.is_empty @@ Hashtbl.keys_list position then ()
    else
      let symbols = Hashtbl.keys_list position in
      let last_data_bar =
        match latest_bars symbols with
        | Ok x -> x
        | Error _ ->
            invalid_arg
              "Unable to get price information for symbol while liquidating"
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
                  qty = Int.abs qty;
                  price = (Bars.price last_data_bar symbol).close;
                }
              in
              Eio.traceln "%a" Order.pp order;
              let _json_resp = create_order order in
              ())
          position
      in
      ()
end
