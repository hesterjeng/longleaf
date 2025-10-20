open Backend_intf
module Util = Longleaf_util
module Tiingo_api = Longleaf_apis.Tiingo_api
module Trading_api = Longleaf_apis.Trading_api
module Market_data_api = Longleaf_apis.Market_data_api

(* type runtype = Live | Paper *)

module Make (Input : BACKEND_INPUT) : S = struct
  open Trading_types

  (* module Backtesting = Backtesting_backend.Make (Input) *)
  module Input = Input

  let switch = Input.options.switch
  let env = Input.options.eio_env
  let opts = Input.options
  let save_received = opts.flags.save_received
  let received_data = Bars.empty ()

  open Cohttp_eio

  let trading_client =
    Client.make ~https:None (Eio.Stdenv.net env)

  let tiingo_client = Tiingo_api.tiingo_client env switch

  let data_client =
    Client.make ~https:None (Eio.Stdenv.net env)

  let get_trading_client _ = Ok trading_client
  let get_data_client _ = Ok data_client

  module Trading_api = Trading_api.Make (struct
    let client = trading_client
    let longleaf_env = opts.longleaf_env
    let runtype = opts.flags.runtype
  end)

  module Market_data_api = Market_data_api.Make (struct
    let client = data_client
    let longleaf_env = opts.longleaf_env
  end)

  module Tiingo = Tiingo_api.Make (struct
    let client = tiingo_client
    let longleaf_env = opts.longleaf_env
  end)

  let init_state () =
    let ( let* ) = Result.( let* ) in
    let* account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    let account_cash = account_status.cash in
    let* bars =
      match Input.target with
      | None -> Error.fatal "No historical data for alpaca backend"
      | Some b -> Result.return b
    in
    let config =
      Indicators_config.make Input.options.flags.runtype
        Input.options.tacaml_indicators
    in
    State.make 0 bars config account_cash

  let next_market_open () =
    let ( let* ) = Result.( let* ) in
    let* clock = Trading_api.Clock.get () in
    if clock.is_open || opts.flags.nowait_market_open then Result.return None
    else Result.return @@ Some clock.next_open

  let next_market_close () =
    let ( let* ) = Result.( let* ) in
    let* clock = Trading_api.Clock.get () in
    Result.return @@ clock.next_close

  let shutdown () =
    Eio.traceln "Alpaca backend shutdown";
    (* cohttp-eio clients don't require explicit shutdown *)
    ()

  let symbols = List.map Instrument.security Input.options.symbols
  let is_backtest = false
  let get_account = Trading_api.Accounts.get_account

  let last_data_bar =
    Result.fail @@ `MissingData "No last data bar in Alpaca backend"

  let update_bars (state : State.t) =
    let ( let+ ) = Result.( let+ ) in
    let bars = State.bars state in
    let tick = State.tick state in
    match symbols with
    | [] ->
      Eio.traceln "No symbols in latest bars request.";
      Result.return state
    | _ ->
      let+ () =
        match Tiingo.latest bars symbols tick with
        | Ok x -> Result.return x
        | Error s ->
          Eio.traceln
            "Error %a from Tiingo.latest, trying again after 5 seconds."
            Error.pp s;
          Unix.sleep 5;
          (* Ticker.tick ~runtype:opts.flags.runtype env 5.0; *)
          Tiingo.latest bars symbols tick
      in
      state

  let get_clock = Trading_api.Clock.get

  let place_order state order =
    let ( let* ) = Result.( let* ) in
    let* () = Trading_api.Orders.create_market_order order in
    let* state = State.place_order state order in
    Result.return state

  let liquidate (state : State.t) =
    let ( let* ) = Result.( let* ) in
    let symbols = State.held_symbols state in
    Eio.traceln "@[Liquidating %d positions@]@." (List.length symbols);

    (* Create sell orders for all positions *)
    let* liquidated_state =
      List.fold_left
        (fun prev symbol ->
          let* prev = prev in
          let qty = State.qty prev symbol in
          if qty = 0 then (
            Eio.traceln "@[Skipping %a: no position to liquidate@]@."
              Instrument.pp symbol;
            Result.return prev)
          else
            let* data = State.data prev symbol in
            let* col = Bars.Data.Column.of_data data @@ State.tick state in
            let* price = Bars.Data.Column.get col Last in
            let abs_qty = Int.abs qty in
            let tick = State.tick prev in
            let* order : Order.t =
              let side = if qty > 0 then Side.Sell else Side.Buy in
              let tif = TimeInForce.GoodTillCanceled in
              let order_type = OrderType.Market in
              let* timestamp = Bars.Data.Column.timestamp col in
              (* last_column Time |> Time.of_float_res in *)
              Result.return
              @@ Order.make ~symbol ~tick ~side ~tif ~order_type ~qty:abs_qty
                   ~price ~timestamp ~profit:None
                   ~reason:[ "Liquidate position" ]
            in
            Eio.traceln "@[Liquidating %d shares of %a at %f@]@." abs_qty
              Instrument.pp symbol price;
            place_order prev order)
        (Ok state) symbols
    in

    (* Verify all positions are closed *)
    let remaining_symbols = State.held_symbols liquidated_state in
    if List.length remaining_symbols > 0 then (
      Eio.traceln "@[Warning: %d positions still remain after liquidation@]@."
        (List.length remaining_symbols);
      List.iter
        (fun sym ->
          let qty = State.qty liquidated_state sym in
          Eio.traceln "@[  %a: %d shares@]@." Instrument.pp sym qty)
        remaining_symbols)
    else Eio.traceln "@[All positions successfully liquidated@]@.";

    let* account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    Ok liquidated_state
end
