open Backend_intf
module Util = Longleaf_util
module Tiingo_api = Longleaf_apis.Tiingo_api
module Tiingo_websocket = Longleaf_apis.Tiingo_websocket
module Massive_websocket = Longleaf_apis.Massive_websocket
module Trading_api = Longleaf_apis.Trading_api
module Market_data_api = Longleaf_apis.Market_data_api

(* type runtype = Live | Paper *)

(* Data source configuration *)
type data_source =
  | AlpacaRest  (* Use Alpaca's REST API for latest bars *)
  | TiingoWebSocket  (* Use Tiingo's WebSocket for real-time data *)
  | MassiveWebSocket  (* Use Massive's WebSocket for real-time data *)
  | TiingoRest  (* Use Tiingo's REST API (legacy, has latency issues) *)

module Make (Input : BACKEND_INPUT) : S = struct
  open Trading_types

  (* module Backtesting = Backtesting_backend.Make (Input) *)
  module Input = Input

  let switch = Input.options.switch
  let env = Input.options.eio_env
  let opts = Input.options
  let save_received = opts.flags.save_received
  let received_data = Bars.empty ()

  (* Configure data source - Use Massive WebSocket if key configured, otherwise Tiingo *)
  let data_source = match opts.flags.runtype with
    | Live | Paper ->
      (match opts.longleaf_env.massive_key with
      | Some _ -> MassiveWebSocket  (* Use Massive WebSocket if key available *)
      | None -> TiingoWebSocket)    (* Fall back to Tiingo WebSocket *)
    | _ -> AlpacaRest  (* Use REST for other modes *)

  open Cohttp_eio

  (* Initialize RNG for TLS - must be called before creating HTTPS clients *)
  let () = Longleaf_apis.Https.init_rng ()

  (* Create HTTPS wrapper for all clients *)
  let authenticator = Longleaf_apis.Https.authenticator ()
  let https = Longleaf_apis.Https.make_https ~authenticator

  let trading_client =
    Client.make ~https:(Some https) (Eio.Stdenv.net env)

  let tiingo_client =
    Client.make ~https:(Some https) (Eio.Stdenv.net env)

  let data_client =
    Client.make ~https:(Some https) (Eio.Stdenv.net env)

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
  let tiingo_ws_client = ref None
  let tiingo_ws_background_started = ref false
  let tiingo_ws_initial_fetch_done = ref false

  let massive_ws_client = ref None
  let massive_ws_background_started = ref false
  let massive_ws_initial_fetch_done = ref false

  (* Track current tick for background fiber *)
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
      (* Use delayed endpoint if available, otherwise real-time *)
      let use_delayed = false in  (* Set to true for delayed data *)
      let* client = Massive_websocket.Client.connect ~sw:switch ~env ~massive_key ~use_delayed () in
      (* Subscribe to all symbols *)
      let* () = Massive_websocket.Client.subscribe client symbols in
      massive_ws_client := Some client;

      (* Start background fiber for continuous updates *)
      if not !massive_ws_background_started then begin
        Massive_websocket.Client.start_background_updates
          ~sw:switch ~env ~use_delayed client bars (fun () -> !current_tick);
        massive_ws_background_started := true;
        Eio.traceln "Alpaca backend: Background Massive WebSocket update fiber started"
      end;

      Eio.traceln "Alpaca backend: Massive WebSocket client initialized and subscribed";
      Ok client

  let get_or_create_ws_client bars () =
    match !tiingo_ws_client with
    | Some client -> Ok client
    | None ->
      Eio.traceln "Alpaca backend: Initializing Tiingo WebSocket client";
      let ( let* ) = Result.( let* ) in
      let* tiingo_key = match opts.longleaf_env.tiingo_key with
        | Some key -> Ok key
        | None -> Error (`MissingData "Tiingo API key not configured")
      in
      let* client = Tiingo_websocket.Client.connect ~sw:switch ~env ~tiingo_key () in
      (* Subscribe to all symbols *)
      let* () = Tiingo_websocket.Client.subscribe client symbols in
      tiingo_ws_client := Some client;

      (* Start background fiber for continuous updates *)
      (* TODO: Update Tiingo websocket to use queue like Massive *)
      if not !tiingo_ws_background_started then begin
        (* Tiingo_websocket.Client.start_background_updates
          ~sw:switch ~env client bars (fun () -> !current_tick); *)
        tiingo_ws_background_started := true;
        Eio.traceln "Alpaca backend: Tiingo WebSocket not yet updated for queue-based design"
      end;

      Eio.traceln "Alpaca backend: Tiingo WebSocket client initialized and subscribed";
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

  let is_backtest = false
  let get_account = Trading_api.Accounts.get_account

  let last_data_bar () =
    Result.fail @@ `MissingData "No last data bar in Alpaca backend"

  let update_bars (state : State.t) =
    let ( let+ ) = Result.( let+ ) in
    let bars = State.bars state in
    let tick = State.tick state in
    let symbols_list = symbols in  (* Capture symbols in local scope *)

    match symbols_list with
    | [] ->
      Eio.traceln "No symbols in latest bars request.";
      Result.return state
    | _ ->
      let+ () =
        match data_source with
        | MassiveWebSocket ->
          (* On first call, initialize WebSocket client *)
          (* After that, WebSocket updates continuously in background *)
          (match get_or_create_massive_ws_client bars () with
           | Ok _client ->
             if not !massive_ws_initial_fetch_done then (
               (* First time: set current_tick to THIS tick so websocket populates it *)
               current_tick := tick;
               Eio.traceln "Alpaca backend: Massive WebSocket initialized, current_tick=%d, waiting for data..." tick;
               (* Sleep to let websocket populate the current tick *)
               Eio.Time.sleep env#clock 2.0;
               (* Now switch to pipeline mode: set current_tick to next tick *)
               current_tick := tick + 1;
               Eio.traceln "Alpaca backend: Initial data received, switching to pipeline mode (current_tick=%d)" (tick + 1);
               massive_ws_initial_fetch_done := true;
               Ok ()
             ) else (
               (* Subsequent calls: set current_tick to NEXT tick (pipeline mode) *)
               (* During the sleep after this iteration, websocket will populate tick+1 *)
               current_tick := tick + 1;
               Ok ()
             )
           | Error e ->
             Eio.traceln "Alpaca backend: Failed to initialize Massive WebSocket: %s" (ws_error_to_string e);
             Error (`MissingData ("Massive WebSocket initialization failed: " ^ ws_error_to_string e)))
        | TiingoWebSocket ->
          (* On first call, populate the next tick with REST to have initial data *)
          (* After that, WebSocket updates continuously in background *)
          (match get_or_create_ws_client bars () with
           | Ok _client ->
             if not !tiingo_ws_initial_fetch_done then (
               (* First time - fetch current tick via REST to seed the data *)
               (* This matches WebSocket behavior which writes to current_tick *)
               Eio.traceln "Alpaca backend: First update_bars call - fetching tick %d via REST" tick;
               tiingo_ws_initial_fetch_done := true;
               match Tiingo.latest bars symbols_list tick with
               | Ok () ->
                 Eio.traceln "Alpaca backend: Initial tick %d populated, WebSocket will update from here" tick;
                 Ok ()
               | Error e ->
                 Eio.traceln "Alpaca backend: Warning - failed to populate initial tick: %a" Error.pp e;
                 (* Continue anyway - WebSocket will fill in data eventually *)
                 Ok ()
             ) else (
               (* WebSocket is running and updating continuously *)
               Ok ()
             )
           | Error e ->
             Eio.traceln "Alpaca backend: Failed to initialize WebSocket: %s" (ws_error_to_string e);
             Error (`MissingData ("WebSocket initialization failed: " ^ ws_error_to_string e)))
        | AlpacaRest ->
          (* Use Alpaca REST API *)
          Eio.traceln "Alpaca backend: Fetching data via Alpaca REST API (tick %d)" tick;
          (match Market_data_api.Stock.latest bars symbols_list tick with
           | Ok x -> Result.return x
           | Error s ->
             Eio.traceln "Error %a from Alpaca latest bars, trying again after 5 seconds." Error.pp s;
             Unix.sleep 5;
             (match Market_data_api.Stock.latest bars symbols_list tick with
              | Ok () -> Ok ()
              | Error e -> Error e))
        | TiingoRest ->
          (* Legacy Tiingo REST API (not recommended due to latency) *)
          Eio.traceln "Alpaca backend: Fetching data via Tiingo REST API (tick %d)" tick;
          (match Tiingo.latest bars symbols_list tick with
           | Ok () -> Ok ()
           | Error e -> Error e)
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
