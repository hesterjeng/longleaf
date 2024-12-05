module Order = Trading_types.Order

module type LONGLEAF_MUTEX = sig
  val shutdown_mutex : bool Pmutex.t
  val data_mutex : Bars.t Pmutex.t
  val orders_mutex : Order_history.t Pmutex.t
  val symbols_mutex : string option Pmutex.t
end

module LongleafMutex () : LONGLEAF_MUTEX = struct
  let shutdown_mutex = Pmutex.make false
  let data_mutex = Pmutex.make Bars.empty
  let orders_mutex = Pmutex.make @@ Vector.create ()
  let symbols_mutex = Pmutex.make None
end

module type S = sig
  module Ticker : Ticker.S
  module LongleafMutex : LONGLEAF_MUTEX
  module Backend_position : Backend_position.S

  val get_trading_client : unit -> Piaf.Client.t
  val get_data_client : unit -> Piaf.Client.t
  val env : Eio_unix.Stdenv.base
  val is_backtest : bool
  (* val loaded_bars : Bars.t *)

  (* val get_position : unit -> Backend_position.t *)
  val init_state : 'a -> 'a State.t
  val get_cash : unit -> float
  val symbols : string list
  val shutdown : unit -> unit
  val overnight : bool

  (* Return the next open time if the market is closed *)
  val next_market_open : unit -> Time.t option
  val next_market_close : unit -> Time.t
  val place_order : _ State.t -> Order.t -> unit
  val latest_bars : string list -> (Bars.Latest.t, string) result
  val last_data_bar : Bars.Latest.t option
  val liquidate : _ State.t -> unit
end

module type BACKEND_INPUT = sig
  val switch : Eio.Switch.t
  val longleaf_env : Environment.t
  val eio_env : Eio_unix.Stdenv.base
  val bars : Bars.t
  val symbols : string list
  val tick : float
  val overnight : bool

  (* The target is the bars that will be iterated over in a backtest *)
  val target : Bars.t option
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

  let init_state content =
    {
      State.current = `Initialize;
      bars = Bars.empty;
      latest = Bars.Latest.empty;
      content;
      order_history = Vector.create ();
    }

  let next_market_open _ = None
  let next_market_close _ = Ptime.max
  let env = Input.eio_env
  let symbols = Input.symbols
  let is_backtest = true
  let shutdown () = ()
  let get_cash = Backend_position.get_cash
  let overnight = Input.overnight

  let place_order state (order : Order.t) =
    Backend_position.execute_order state order

  let data_remaining =
    match Input.target with
    | Some b -> b
    | None -> invalid_arg "Must have a target specified for backtest"

  let latest_bars _ =
    let module Hashtbl = Bars.Hashtbl in
    let latest : Bars.Latest.t =
      Hashtbl.create (Hashtbl.length data_remaining)
    in
    let found =
      Hashtbl.to_seq data_remaining
      |> Seq.find_map @@ fun (symbol, vector) ->
         Vector.top vector |> function
         | None -> Some "Empty vector when trying to collect data"
         | Some _ ->
             (* Eio.traceln "There are %d members remaining in bar %s." *)
             (*   (Vector.length vector) symbol; *)
             Hashtbl.replace latest symbol @@ Vector.get vector 0;
             Vector.remove_and_shift vector 0;
             None
    in
    match found with Some err -> Error err | None -> Ok latest

  let last_data_bar =
    let module Hashtbl = Bars.Hashtbl in
    let last : Bars.Latest.t = Hashtbl.create 20 in
    (Hashtbl.to_seq Input.bars
    |> Seq.iter @@ fun (symbol, vector) ->
       match Vector.pop vector with
       | None -> invalid_arg "Empty dataset"
       | Some item -> Hashtbl.replace last symbol item);
    Some last

  let liquidate state =
    match last_data_bar with
    | None -> invalid_arg "Expected last data bar in backtest"
    | Some db -> Backend_position.liquidate state db
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
  let overnight = Input.overnight

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

  let init_state content =
    let account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    let account_cash = account_status.cash in
    Backend_position.set_cash account_cash;
    {
      State.current = `Initialize;
      bars = Bars.empty;
      latest = Bars.Latest.empty;
      content;
      order_history = Vector.create ();
    }

  let next_market_open () =
    let clock = Trading_api.Clock.get () in
    if clock.is_open then None else Some clock.next_open

  let next_market_close () =
    let clock = Trading_api.Clock.get () in
    clock.next_close

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
    | [] ->
        invalid_arg
          "Backend.latest_bars: Cannot get latest bars for no symbols."
    | _ ->
        let _ = Backtesting.latest_bars symbols in
        (* let res = Market_data_api.Stock.latest_bars symbols in *)
        let res = Tiingo.latest symbols in
        Ok res

  let get_clock = Trading_api.Clock.get

  let place_order state order =
    Backtesting.place_order state order;
    Trading_api.Orders.create_market_order order

  let liquidate state =
    let symbols = Backend_position.symbols () in
    let last_data_bar =
      match latest_bars symbols with
      | Ok x -> x
      | Error _ ->
          invalid_arg
            "[Error] Unable to get price information for symbol while \
             liquidating."
    in
    let _ =
      List.iter
        (fun symbol ->
          let qty = Backend_position.qty symbol in
          if qty = 0 then ()
          else
            let latest_info = Bars.Latest.get last_data_bar symbol in
            let order : Order.t =
              let side = if qty >= 0 then Side.Sell else Side.Buy in
              let tif = TimeInForce.GoodTillCanceled in
              let order_type = OrderType.Market in
              let qty = Int.abs qty in
              let price = Bars.Item.last latest_info in
              let timestamp = Bars.Item.timestamp latest_info in
              Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp
                ~profit:None ~reason:"Liquidate"
            in
            Eio.traceln "%a" Order.pp order;
            let _json_resp = place_order state order in
            ())
        symbols
    in
    let account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    ()
end
