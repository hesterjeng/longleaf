open Backend_intf
module Util = Longleaf_util
module Massive_websocket = Longleaf_apis.Massive_websocket
module Trading_api = Longleaf_apis.Trading_api

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

  (* Initialize RNG for TLS - must be called before creating HTTPS clients *)
  let () = Longleaf_apis.Https.init_rng ()

  (* Create HTTPS wrapper for all clients *)
  let authenticator = Longleaf_apis.Https.authenticator ()
  let https = Longleaf_apis.Https.make_https ~authenticator

  let trading_client =
    Client.make ~https:(Some https) (Eio.Stdenv.net env)

  let get_trading_client _ = Ok trading_client
  let get_data_client _ = Ok trading_client  (* Use same client *)

  module Trading_api = Trading_api.Make (struct
    let client = trading_client
    let longleaf_env = opts.longleaf_env
    let runtype = opts.flags.runtype
  end)

  (* Extract symbols early so we can use it in WebSocket initialization *)
  let symbols = List.map Instrument.security Input.options.symbols

  (* Helper to convert WebSocket errors to string *)
  let ws_error_to_string = function
    | `ConnectionClosed -> "Connection closed"
    | `InvalidOpcode i -> "Invalid opcode: " ^ string_of_int i
    | `JsonError s -> "JSON error: " ^ s
    | `MissingData s -> "Missing data: " ^ s
    | `ParseError s -> "Parse error: " ^ s
    | `ReadError s -> "Read error: " ^ s
    | `UnexpectedFrame s -> "Unexpected frame: " ^ s
    | `DnsError s -> "DNS error: " ^ s
    | `HandshakeError s -> "Handshake error: " ^ s
    | `InvalidScheme s -> "Invalid scheme: " ^ s
    | `InvalidUrl s -> "Invalid URL: " ^ s
    | `TlsError s -> "TLS error: " ^ s
    | `WriteError s -> "Write error: " ^ s

  (* WebSocket client and background fiber state *)
  let massive_ws_client = ref None
  let massive_ws_background_started = ref false

  (* Track current tick for background fiber - set in prepare_live_trading *)
  let current_tick = ref 0

  let get_or_create_massive_ws_client bars () =
    match !massive_ws_client with
    | Some client -> Ok client
    | None ->
      Eio.traceln "Alpaca backend: Initializing Massive WebSocket client";
      let ( let* ) = Result.( let* ) in
      let* massive_key = match opts.longleaf_env.massive_key with
        | Some key -> Ok key
        | None -> Error (`MissingData "Massive API key not configured")
      in
      let* client = Massive_websocket.Client.connect ~sw:switch ~env ~massive_key () in
      (* Subscribe to all symbols *)
      let* () = Massive_websocket.Client.subscribe client symbols in
      massive_ws_client := Some client;

      (* Start background fiber for continuous updates *)
      if not !massive_ws_background_started then begin
        Massive_websocket.Client.start_background_updates
          ~sw:switch ~env client bars (fun () -> !current_tick);
        massive_ws_background_started := true;
        Eio.traceln "Alpaca backend: Background Massive WebSocket update fiber started"
      end;

      Eio.traceln "Alpaca backend: Massive WebSocket client initialized and subscribed";
      Ok client

  let init_state () =
    let ( let* ) = Result.( let* ) in
    Eio.traceln "Alpaca backend: Fetching account information...";
    let* account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "Alpaca backend: Account cash: $%.2f" account_status.cash;
    let account_cash = account_status.cash in
    Eio.traceln "Alpaca backend: Loading historical data...";
    let* bars =
      match Input.target with
      | None -> Error.fatal "No historical data for alpaca backend"
      | Some b -> Result.return b
    in
    Eio.traceln "Alpaca backend: Creating indicator configuration...";
    let config =
      Indicators_config.make_with_print_tick Input.options.flags.runtype
        Input.options.tacaml_indicators opts.flags.print_tick_arg
    in
    Eio.traceln "Alpaca backend: Initializing state...";
    State.make 0 bars config account_cash opts.flags.print_tick_arg

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

  let reset_websocket () =
    Eio.traceln "Alpaca backend: Resetting websocket connection state";
    (* Close existing connection if any *)
    (match !massive_ws_client with
     | Some client ->
       (try Massive_websocket.Client.close client
        with _ -> Eio.traceln "Alpaca backend: Error closing old websocket (ignoring)");
     | None -> ());
    (* Reset refs so next get_or_create_massive_ws_client creates fresh connection *)
    massive_ws_client := None;
    massive_ws_background_started := false;
    Eio.traceln "Alpaca backend: Websocket state reset complete"

  let is_backtest = false
  let get_account = Trading_api.Accounts.get_account

  let last_data_bar () =
    Result.fail @@ `MissingData "No last data bar in Alpaca backend"

  let prepare_live_trading (state : State.t) =
    let bars = State.bars state in
    let tick = State.tick state in
    Eio.traceln "Alpaca backend: Preparing live trading (MassiveWebSocket, starting tick=%d)" tick;
    match get_or_create_massive_ws_client bars () with
    | Ok _client ->
      current_tick := tick;
      Eio.traceln "Alpaca backend: WebSocket ready, current_tick=%d" tick;
      Ok ()
    | Error e ->
      Error (`MissingData ("WebSocket initialization failed: " ^ ws_error_to_string e))

  let update_bars (state : State.t) =
    let tick = State.tick state in
    (* WebSocket is already running (initialized in prepare_live_trading) *)
    (* Advance current_tick for next iteration so websocket writes to next slot *)
    current_tick := tick + 1;
    Ok state

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
