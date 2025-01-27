open Backend_intf

module Make (Input : BACKEND_INPUT) : S = struct
  open Trading_types
  module Backtesting = Backtesting_backend.Make (Input)
  module Backend_position = Backtesting.Backend_position
  module Input = Input

  let get_cash = Backend_position.get_cash
  let env = Input.eio_env
  let overnight = Input.overnight
  let save_received = Input.save_received
  let received_data = Bars.empty ()

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

  let get_trading_client _ = Ok trading_client
  let get_data_client _ = Ok data_client

  module Trading_api = Trading_api.Make (struct
    let client = trading_client
    let longleaf_env = Input.longleaf_env
  end)

  module Market_data_api = Market_data_api.Make (struct
    let client = data_client
    let longleaf_env = Input.longleaf_env
  end)

  let init_state content =
    let account_status =
      Trading_api.Accounts.get_account () |> function
      | Ok x -> x
      | Error e ->
          Eio.traceln "alpaca_backend: error getting account status";
          raise e
    in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    let account_cash = account_status.cash in
    Backend_position.set_cash account_cash;
    {
      State.current = `Initialize;
      bars = Input.bars;
      tick = 0;
      latest = Bars.Latest.empty ();
      content;
      stats = Stats.empty;
      order_history = Vector.create ();
      indicators = Indicators.empty ();
      active_orders = [];
    }

  let next_market_open () =
    let clock =
      Trading_api.Clock.get () |> function
      | Ok x -> x
      | Error e ->
          Eio.traceln "alpaca_backend: error getting clock";
          raise e
    in
    if clock.is_open then None else Some clock.next_open

  let next_market_close () =
    let clock =
      Trading_api.Clock.get () |> function
      | Ok x -> x
      | Error e ->
          Eio.traceln "alpaca_backend: error getting clock";
          raise e
    in
    clock.next_close

  let shutdown () =
    Eio.traceln "Alpaca backend shutdown";
    Piaf.Client.shutdown trading_client;
    Piaf.Client.shutdown data_client;
    Piaf.Client.shutdown tiingo_client;
    ()

  let symbols = Input.symbols
  let is_backtest = false
  let get_account = Trading_api.Accounts.get_account
  let last_data_bar = Error "No last data bar in Alpaca backend"

  let latest_bars symbols =
    match symbols with
    | [] ->
        Eio.traceln "No symbols in latest bars request.";
        Result.return @@ Bars.Latest.empty ()
    | _ ->
        let _ = Backtesting.latest_bars symbols in
        (* let res = Market_data_api.Stock.latest_bars symbols in *)
        let res =
          Tiingo.latest symbols |> function
          | Ok x -> x
          | Error e ->
              Eio.traceln "alpaca backend: error getting latest tiingo data";
              raise e
        in
        if save_received then Bars.append res received_data;
        Ok res

  let get_clock = Trading_api.Clock.get

  let place_order state order =
    let ( let* ) = Result.( let* ) in
    assert (not @@ Input.dropout);
    let* () = Backtesting.place_order state order in
    Trading_api.Orders.create_market_order order

  let liquidate state =
    let ( let* ) = Result.( let* ) in
    let symbols = Backend_position.symbols () in
    let* last_data_bar = latest_bars symbols in
    let _ =
      List.iter
        (fun symbol ->
          let qty = Backend_position.qty symbol in
          assert (qty <> 0);
          let latest_info = Bars.Latest.get last_data_bar symbol in
          let order : Order.t =
            let side = if qty >= 0 then Side.Sell else Side.Buy in
            let tif = TimeInForce.GoodTillCanceled in
            let order_type = OrderType.Market in
            let qty = Int.abs qty in
            let price = Item.last latest_info in
            let timestamp = Item.timestamp latest_info in
            Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp
              ~profit:None ~reason:[ "Liquidate" ]
          in
          (* Eio.traceln "%a" Order.pp order; *)
          let _json_resp = place_order state order in
          ())
        symbols
    in
    let account_status =
      Trading_api.Accounts.get_account () |> function
      | Ok x -> x
      | Error e ->
          Eio.traceln
            "alpaca backend: error getting account status while liquidating";
          raise e
    in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    Ok ()
end
