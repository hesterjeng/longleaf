module State = Longleaf_state
module Pmutex = Longleaf_util.Pmutex
module Options = Longleaf_core.Options
module Mode = State.Mode
module Bars = Longleaf_bars
module Indicators = Longleaf_indicators.Indicators

let ( let* ) = Result.( let* )

module Make (Backend : Backend.S) : sig
  val go :
    (State.t -> (State.t, Error.t) result) ->
    State.t ->
    (State.t, Error.t) result
end = struct
  module Input = Backend.Input

  let options = Input.options
  let eio_env = Input.options.eio_env
  let mutices : State.Mutex.t = Input.mutices
  let runtype = options.flags.runtype

  module LiveIndicators = struct
    let top (state : State.t) =
      let* () =
        match Input.options.flags.runtype with
        | Live
        | Paper ->
          let indicator_config = (State.config state).indicator_config in
          let pool = Input.options.executor_pool in
          let tick = State.tick state in
          Eio.traceln "[DIAG] Computing indicators for tick %d" tick;
          let* () = Indicators.Calc.compute_single ~pool tick eio_env
            indicator_config
          @@ State.bars state in
          Eio.traceln "[DIAG] Indicators computed for tick %d" tick;
          Result.return ()
        | _ -> Result.return ()
      in
      Result.return state
  end

  module Initialize : sig
    val top : State.t -> (State.t, Error.t) result
  end = struct
    let top (state : State.t) =
      Eio.traceln "=== Initialization started ===";
      Eio.traceln "[1/4] Setting up symbols...";
      let symbols_str =
        List.map Instrument.symbol Backend.symbols |> String.concat ","
      in
      Pmutex.set mutices.symbols_mutex (Some symbols_str);
      Eio.traceln "[2/4] Computing indicators for %d symbols..." (List.length Backend.symbols);
      let* () =
        let indicator_config = (State.config state).indicator_config in
        let pool = Input.options.executor_pool in
        Indicators.Calc.compute_all ~pool eio_env indicator_config @@ State.bars state
      in
      Eio.traceln "[3/4] Indicators computed successfully";
      let* state =
        (* If we are in live or paper, we need to grow the bars to have somewhere \ *)
        (* to put the received data. *)
        match Input.options.flags.runtype with
        | Live
        | Paper ->
          Eio.traceln "[4/4] Preparing bars for live data reception...";
          let* bars_length = Bars.length @@ State.bars state in
          (* Start at first empty slot (bars_length), not last historical slot (bars_length - 1) *)
          let* state = State.set_tick state bars_length |> State.grow in
          Eio.traceln "State prepared: tick=%d (first empty slot after %d historical bars)"
            (State.tick state) bars_length;
          Result.return state
        | _ ->
          Eio.traceln "[4/4] Backtest mode - no bar growth needed";
          Result.return state
      in
      (* Initialize websocket for live/paper trading before main loop starts *)
      let* () = Backend.prepare_live_trading state in
      Eio.traceln "=== Initialization complete (tick %d) ===" (State.tick state);
      Result.return state
  end

  exception FinalState of State.t

  (* Track when market opened to skip opening volatility window *)
  let market_open_time : float option ref = ref None

  module Listen = struct
    (* Check if we're in the opening volatility window *)
    let in_opening_window env =
      match !market_open_time with
      | None -> false  (* Market hasn't opened yet *)
      | Some open_ts ->
        let now = Eio.Time.now env#clock in
        let elapsed_minutes = (now -. open_ts) /. 60.0 in
        elapsed_minutes <. Float.of_int options.flags.opening_wait_minutes

    let listen_tick state env : (unit, Error.t) result =
      match runtype with
      | Live
      | Paper ->
        Eio.traceln "Listen: Starting listen_tick for tick %d" (State.tick state);
        Eio.Fiber.any
        @@ [
             (fun () ->
               let* nmo = Backend.next_market_open () in
               match nmo with
               | None ->
                 (* Market is open - check if we need to track opening time *)
                 if Option.is_none !market_open_time then (
                   let now = Eio.Time.now env#clock in
                   market_open_time := Some now;
                   if options.flags.opening_wait_minutes > 0 then
                     Eio.traceln "@[Market is open. Waiting %d minutes to avoid opening volatility.@]@."
                       options.flags.opening_wait_minutes
                 );

                 (* Check if we're still in opening window *)
                 if in_opening_window env then (
                   let now = Eio.Time.now env#clock in
                   let elapsed = match !market_open_time with
                     | Some open_ts -> (now -. open_ts) /. 60.0
                     | None -> 0.0
                   in
                   let remaining = Float.of_int options.flags.opening_wait_minutes -. elapsed in
                   Eio.traceln "@[Still in opening window: %.1f minutes remaining before trading@]@." remaining;
                   Ticker.tick ~runtype env Input.options.tick;
                   Result.return ()
                 ) else (
                   Eio.traceln "Listen: Market open, sleeping %.1fs (tick %d)" Input.options.tick (State.tick state);
                   Ticker.tick ~runtype env Input.options.tick;
                   Eio.traceln "Listen: Woke up (tick %d)" (State.tick state);
                   Result.return ()
                 )
               | Some open_time ->
                 Eio.traceln "@[Current time: %a@]@." (Option.pp Time.pp)
                   (Eio.Time.now env#clock |> Ptime.of_float_s);
                 Eio.traceln "@[Open time: %a@]@." Time.pp open_time;
                 let open_time_ts = Ptime.to_float_s open_time in
                 Eio.traceln "@[Waiting until market open...@]@.";
                 Eio.Time.sleep_until env#clock open_time_ts;
                 Eio.traceln "@[Market is open, resuming.@]@.";
                 (* Reset websocket after overnight sleep to ensure fresh connection *)
                 Backend.reset_websocket ();
                 (* Record market open time *)
                 market_open_time := Some open_time_ts;
                 if options.flags.opening_wait_minutes > 0 then
                   Eio.traceln "@[Will wait %d minutes before starting to trade.@]@."
                     options.flags.opening_wait_minutes;
                 Ticker.tick ~runtype env Input.options.tick;
                 Result.return ());
             (fun () ->
               while
                 let shutdown = Pmutex.get mutices.shutdown_mutex in
                 not shutdown
               do
                 Eio.Fiber.yield ();
                 Ticker.tick ~runtype env 1.0
               done;
               raise (FinalState state));
           ]
      | _ -> Result.return ()

    (* Returns Ok state only when we're ready to trade (past opening window or not applicable) *)
    (* Loops internally during the opening window, only returning when ready to trade *)
    let rec top (state : State.t) : (State.t, Error.t) result =
      if not options.flags.no_gui then Pmutex.set mutices.state_mutex state;
      (* Print tick at start of strategy iteration to show loop is progressing *)
      if options.flags.print_tick_arg && not (in_opening_window eio_env) then
        Eio.traceln "=== Starting strategy iteration for tick %d ===" (State.tick state);
      let* () = listen_tick state eio_env in
      (* If we're in opening window, wait and try again *)
      if in_opening_window eio_env then (
        (* Silent recursion - details already logged in listen_tick *)
        top state  (* Recursively call until we're past the opening window *)
      ) else (
        if Option.is_some !market_open_time && options.flags.opening_wait_minutes > 0 then
          Eio.traceln "=== Opening volatility window passed, starting to trade ===";
        Ok state
      )
  end

  module ForwardFill = struct
    (* Forward-fill: copy current tick to next tick for all symbols *)
    (* Only for live/paper - backtesting data is already complete *)
    (* This ensures continuity if a symbol has no trades during a tick *)
    let top (state : State.t) : (State.t, Error.t) result =
      (match runtype with
       | Live | Paper ->
         let tick = State.tick state in
         let bars = State.bars state in
         List.iter (fun instrument ->
           match Bars.get bars instrument with
           | Ok data -> Bars.Data.forward_fill_next_tick data ~tick
           | Error _ -> ()
         ) Backend.symbols
       | _ -> ());
      Ok state
  end

  module Increment = struct
    let top state =
      let* length = State.bars state |> Bars.length in
      let current_tick = State.tick state in

      (* Check if we're approaching the end - grow if within 1000 ticks of limit *)
      (* This allows indefinite live/paper trading sessions *)
      let* state =
        if current_tick >= length - 1000 then (
          match options.flags.runtype with
          | Live | Paper ->
            Eio.traceln "Approaching bar limit (tick %d of %d), growing bars..." current_tick length;
            let* grown_state = State.grow state in
            let* new_length = State.bars grown_state |> Bars.length in
            Eio.traceln "Bars grown from %d to %d slots" length new_length;
            Result.return grown_state
          | _ -> Result.return state
        ) else Result.return state
      in

      (* Now check if we've truly hit the end (shouldn't happen with auto-growth) *)
      let* final_length = State.bars state |> Bars.length in
      if current_tick >= final_length - 1 then
        raise (FinalState state)
      else (
        let* new_state = State.increment_tick state in
        if options.flags.print_tick_arg then
          Eio.traceln "=== Strategy loop completed tick %d, moving to tick %d ==="
            current_tick (State.tick new_state);
        Result.return new_state
      )
  end

  module Liquidate = struct
    let top = Backend.liquidate

    let top_continue state =
      let* state = Backend.liquidate state in
      Eio.traceln "liquidate continue 1 minute wait...";
      Ticker.tick ~runtype eio_env 60.0;
      Result.return state
  end

  module Finish = struct
    let get_filename () = Longleaf_util.random_filename ()

    let output_data (state : State.t) filename =
      match options.flags.save_to_file with
      | true ->
        let prefix =
          match Backend.is_backtest with
          | true -> "backtest"
          | false -> "live"
        in
        Eio.traceln "Saving all bars...";
        let bars = State.bars state in
        let* () = Bars.print_to_file ~filename bars prefix in
        let* () =
          Bars.print_to_file ~filename Backend.received_data
            (prefix ^ "_received")
        in
        Result.return ()
      | false -> Result.return ()

    let output_order_history (_state : State.t) filename =
      if options.flags.save_to_file then (
        Eio.traceln "@[Outputting order history to filename %s@]@." filename;
        let json_str =
          `List [] |> Yojson.Safe.to_string
          (* TODO: get order history from trading_state *)
        in
        let filename = Format.sprintf "data/order_history_%s.json" filename in
        let oc = open_out filename in
        output_string oc json_str;
        close_out oc)
      else ()

    let top state =
      let bars = State.bars state in
      let tick = State.tick state in
      if not options.flags.no_gui then Pmutex.set mutices.state_mutex state;
      Eio.traceln "@[Reached finished state %d. with %f after %d orders.@]@."
        tick (State.cash state)
        (State.orders_placed state);

      (* Compute and display trade statistics *)
      let all_orders = State.order_history state in
      (match Longleaf_state.Stats.TradeStats.compute all_orders with
      | None -> Eio.traceln "No completed trades to analyze"
      | Some trade_stats ->
        Eio.traceln "";
        Eio.traceln "=== TRADE STATISTICS ===";
        Eio.traceln "%s" (Longleaf_state.Stats.TradeStats.to_string trade_stats);
        Eio.traceln "");

      (* TODO: get order history length from trading_state *)
      Eio.traceln "state.bars at finish: %a" Bars.pp bars;
      Eio.traceln "Done...";
      let filename = get_filename () in
      let* () = output_data state filename in
      output_order_history state filename;
      Ok state
  end

  module GetData = struct
    let top = Backend.update_bars
  end

  (* Just like Result.map, but if we have an error we will first try to liquidate inputs *)
  let ( >>= ) x y =
    let liquidate_finish s =
      Eio.traceln
        "Recovered previous state, attempting to liquidate before failing";
      let liq = Result.(Liquidate.top s >>= Finish.top) in
      Eio.traceln "Emergency liquidation successful: %b" (Result.is_ok liq);
      ()
    in
    let res = Result.(x >>= y) in
    match res with
    | Ok _ -> res
    | Error err ->
      Eio.traceln "ERROR: %a" Error.pp err;
      let* prev = x in
      liquidate_finish prev;
      res

  let go order (x : State.t) =
    let* init = Initialize.top x in
    Eio.traceln "=== Starting main strategy loop ===";
    let rec loop state =
      (* Main strategy loop:
         1. Listen - wait for market open, sleep for tick duration
         2. ForwardFill - copy current tick to next tick (no-op for backtest)
         3. GetData - advance websocket write pointer (no-op for backtest)
         4. LiveIndicators - compute indicators for current tick
         5. order - execute strategy logic
         6. Increment - advance to next tick *)
      Listen.top state
      >>= ForwardFill.top
      >>= GetData.top
      >>= LiveIndicators.top
      >>= order
      >>= Increment.top
      >>= loop
    in
    let res =
      try loop init with
      | FinalState s ->
        Eio.traceln "=== Strategy loop ended, finalizing ===";
        Liquidate.top s >>= Finish.top
    in
    res
end
