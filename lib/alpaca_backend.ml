open Backend_intf

type runtype = Live | Paper

let apca_api_base_url ty =
  match ty with
  | Live -> Uri.of_string "https://api.alpaca.markets/v2"
  | Paper -> Uri.of_string "https://paper-api.alpaca.markets/v2"

let apca_api_data_url = Uri.of_string "https://data.alpaca.markets/v2"

module Make (Input : BACKEND_INPUT) : S = struct
  open Trading_types
  module Backtesting = Backtesting_backend.Make (Input)
  module Input = Input

  let context = Input.options.context
  let env = context.eio_env
  let runtype = context.runtype
  let overnight = Input.options.overnight
  let save_received = context.save_received
  let received_data = Bars.empty ()

  let trading_client =
    let res =
      let ty =
        match Input.options.context.runtype with Live -> Live | _ -> Paper
      in
      Piaf.Client.create ~sw:context.switch context.eio_env
      @@ apca_api_base_url ty
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create trading client"

  let tiingo_client = Tiingo_api.tiingo_client context.eio_env context.switch

  module Tiingo_client : Util.CLIENT = struct
    let longleaf_env = context.longleaf_env
    let client = tiingo_client
  end

  module Tiingo = Tiingo_api.Make (Tiingo_client)

  let data_client =
    let res =
      Piaf.Client.create ~sw:context.switch context.eio_env apca_api_data_url
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create data client"

  let get_trading_client _ = Ok trading_client
  let get_data_client _ = Ok data_client

  module Trading_api = Trading_api.Make (struct
    let client = trading_client
    let longleaf_env = context.longleaf_env
  end)

  module Market_data_api = Market_data_api.Make (struct
    let client = data_client
    let longleaf_env = context.longleaf_env
  end)

  let init_state content =
    let ( let* ) = Result.( let* ) in
    let* account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    let account_cash = account_status.cash in
    let positions =
      Backend_position.make () |> fun p ->
      Backend_position.set_cash p account_cash
    in
    Result.return
    @@ {
         State.current = Initialize;
         bars = Input.bars;
         tick = 0;
         tick_length = Input.options.tick;
         positions;
         latest = Bars.Latest.empty ();
         content;
         stats = Stats.empty;
         order_history = Order.History.empty;
         indicators = Indicators.empty ();
       }

  let next_market_open () =
    let ( let* ) = Result.( let* ) in
    let* clock = Trading_api.Clock.get () in
    if clock.is_open || context.nowait_market_open then Result.return None
    else Result.return @@ Some clock.next_open

  let next_market_close () =
    let ( let* ) = Result.( let* ) in
    let* clock = Trading_api.Clock.get () in
    Result.return @@ clock.next_close

  let shutdown () =
    Eio.traceln "Alpaca backend shutdown";
    Piaf.Client.shutdown trading_client;
    Piaf.Client.shutdown data_client;
    Piaf.Client.shutdown tiingo_client;
    ()

  let symbols = Input.options.symbols
  let is_backtest = false
  let get_account = Trading_api.Accounts.get_account

  let last_data_bar =
    Result.fail @@ `MissingData "No last data bar in Alpaca backend"

  let latest_bars symbols =
    let ( let* ) = Result.( let* ) in
    (* let* account = Trading_api.Accounts.get_account () in *)
    (* let backend_cash = Backend_position.get_cash in *)
    (* if not @@ Float.equal backend_cash account.cash then *)
    (*   Eio.traceln "[alpaca_backend] Backend cash: %f Alpaca cash: %f" *)
    (*     (Backend_position.get_cash ()) *)
    (*     account.cash; *)
    (* Backend_position.set_cash account.cash; *)
    match symbols with
    | [] ->
        Eio.traceln "No symbols in latest bars request.";
        Result.return @@ Bars.Latest.empty ()
    | _ ->
        let _ = Backtesting.latest_bars symbols in
        (* let res = Market_data_api.Stock.latest_bars symbols in *)
        let* res =
          match Tiingo.latest symbols with
          | Ok x -> Result.return x
          | Error s ->
              Eio.traceln
                "Error %a from Tiingo.latest, trying again after 5 seconds."
                Error.pp s;
              Ticker.tick ~runtype env 5.0;
              Tiingo.latest symbols
        in
        if save_received then Bars.append res received_data;
        Ok res

  let get_clock = Trading_api.Clock.get

  let place_order state order =
    let ( let* ) = Result.( let* ) in
    assert (not @@ Input.options.dropout);
    let* () = Trading_api.Orders.create_market_order order in
    let* state = Backtesting.place_order state order in
    Result.return state

  let liquidate (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    let symbols = Backend_position.symbols state.positions in
    let* last_data_bar = latest_bars symbols in
    let* liquidated_state =
      List.fold_left
        (fun prev symbol ->
          let* prev = prev in
          let qty = Backend_position.qty state.positions symbol in
          assert (qty <> 0);
          let latest_info = Bars.Latest.get last_data_bar symbol in
          let order : Order.t =
            let side = if qty >= 0 then Side.Sell else Side.Buy in
            let tif = TimeInForce.GoodTillCanceled in
            let order_type = OrderType.Market in
            let qty = Int.abs qty in
            let price = Item.last latest_info in
            let timestamp = Item.timestamp latest_info in
            Order.make ~symbol ~tick:state.tick ~side ~tif ~order_type ~qty
              ~price ~timestamp ~profit:None ~reason:[ "Liquidate" ]
          in
          place_order prev order)
        (Ok state) symbols
    in
    let* account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    Ok liquidated_state
end
