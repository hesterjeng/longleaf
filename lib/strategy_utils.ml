module Make (Backend : Backend_intf.S) = struct
  let mutices : Longleaf_mutex.t = Backend.Input.mutices
  let runtype = Backend.Input.runtype

  let listen_tick () : State.nonlogical_state =
    Eio.Fiber.any
    @@ [
         (fun () ->
           match Backend.next_market_open () with
           | None ->
               Ticker.tick ~runtype Backend.env Backend.Input.tick;
               `Continue
           | Some open_time -> (
               let open_time = Ptime.to_float_s open_time in
               Eio.traceln "@[Waiting until market open...@]@.";
               Eio.Time.sleep_until Backend.env#clock open_time;
               Eio.traceln "@[Market is open, resuming.@]@.";
               Eio.Time.now Backend.env#clock |> Ptime.of_float_s |> function
               | Some t ->
                   Eio.traceln "@[Current time: %a@]@." Time.pp t;
                   Ticker.tick ~runtype Backend.env 5.0;
                   Eio.traceln "@[Waited five seconds.@]@.";
                   `Continue
               | None ->
                   Eio.traceln "@[Detected an illegal time!  Shutting down.@]@.";
                   `BeginShutdown));
         (fun () ->
           while
             let shutdown = Pmutex.get mutices.shutdown_mutex in
             not shutdown
           do
             Eio.Fiber.yield ();
             Ticker.tick ~runtype Backend.env 1.0
           done;
           Eio.traceln "@[Shutdown command received by shutdown mutex.@]@.";
           `BeginShutdown);
         (fun () ->
           let close_time = Backend.next_market_close () in
           let now =
             Eio.Time.now Backend.env#clock |> Ptime.of_float_s |> function
             | Some t -> t
             | None ->
                 invalid_arg
                   "Unable to get clock time in listen_tick for market close"
           in
           let time_until_close =
             Ptime.diff close_time now |> Ptime.Span.to_float_s
           in
           while Backend.overnight || time_until_close >=. 600.0 do
             Eio.Fiber.yield ()
           done;
           Eio.traceln
             "@[Liquidating because we are within 10 minutes to market \
              close.@]@.";
           match Backend.Input.resume_after_liquidate with
           | false -> `BeginShutdown
           | true ->
               Eio.traceln
                 "Liquidating and then continuing because we are approaching \
                  market close.";
               `LiquidateContinue);
       ]

  let counter = ref 0

  let run ~init_state step : float =
    let rec go prev =
      let stepped = step prev in
      match stepped with
      | Ok x -> go x
      | Error s -> (
          let try_liquidating () =
            Eio.traceln
              "@[Trying to liquidate because of a signal or error: %s@]@." s;
            let liquidate = { prev with State.current = `Liquidate } in
            go liquidate
          in
          match prev.current with
          | `Liquidate | `Finished _ ->
              Eio.traceln "@[Exiting run.@]@.";
              Backend.get_cash ()
          | _ -> try_liquidating ())
    in
    go init_state

  let get_filename () = Lots_of_words.select () ^ "_" ^ Lots_of_words.select ()

  let output_data (state : _ State.t) filename =
    if Backend.Input.save_to_file then (
      let prefix =
        match Backend.is_backtest with true -> "backtest" | false -> "live"
      in
      Eio.traceln "Saving all bars...";
      Bars.print_to_file ~filename state.bars prefix;
      Bars.print_to_file ~filename Backend.received_data (prefix ^ "_received"))
    else ()

  let output_order_history (state : _ State.t) filename =
    if Backend.Input.save_to_file then (
      let json_str =
        Order_history.yojson_of_t state.order_history |> Yojson.Safe.to_string
      in
      let filename = Format.sprintf "data/order_history_%s.json" filename in
      let oc = open_out filename in
      output_string oc json_str;
      close_out oc)
    else ()

  let handle_nonlogical_state (current : State.nonlogical_state)
      (state : _ State.t) =
    let ( let* ) = Result.( let* ) in
    (* Eio.traceln "There are %d bindings in state.bars" *)
    (*   (Bars.Hashtbl.length state.bars); *)
    match current with
    | `Initialize ->
        Eio.traceln "Initialize state...";
        let symbols_str = String.concat "," Backend.symbols in
        Pmutex.set mutices.symbols_mutex (Some symbols_str);
        Result.return @@ { state with current = `Listening }
    | `Listening -> (
        Pmutex.set mutices.data_mutex state.bars;
        Pmutex.set mutices.orders_mutex state.order_history;
        Pmutex.set mutices.stats_mutex state.stats;
        Pmutex.set mutices.indicators_mutex state.indicators;
        (* Eio.traceln "tick"; *)
        match listen_tick () with
        | `Continue ->
            let open Result.Infix in
            let* latest = Backend.latest_bars Backend.symbols in
            let* time = Bars.Latest.timestamp latest in
            (* Eio.traceln "@[strategies: %a@]@." Item.pp *)
            (*   (Bars.Latest.get latest "NVDA"); *)
            Indicators.add_latest Backend.Input.indicators_config time
              state.bars latest state.indicators;
            let value = Backend.Backend_position.value latest in
            let risk_free_value =
              Stats.risk_free_value state.stats Backend.Input.tick
            in
            Bars.append latest state.bars;
            Result.return
            @@ {
                 state with
                 current = `Ordering;
                 latest;
                 stats =
                   Stats.append
                     { time; value; orders = []; risk_free_value }
                     state.stats;
               }
        | `BeginShutdown ->
            Eio.traceln "Attempting to liquidate positions before shutting down";
            Result.return { state with current = `Liquidate }
        | `LiquidateContinue ->
            Eio.traceln "Strategies.listen_tick resturned LiquidateContinue";
            Result.return { state with current = `LiquidateContinue }
        | _ ->
            invalid_arg
              "Strategies.handle_nonlogical_state: unhandled return value from \
               listen_tick")
    | `Liquidate ->
        let* () = Backend.liquidate state in
        Result.return
        @@ { state with current = `Finished "Liquidation finished" }
    | `LiquidateContinue ->
        let* () = Backend.liquidate state in
        Ticker.tick ~runtype Backend.env 600.0;
        Result.return { state with current = `Listening }
    | `Finished code ->
        Eio.traceln "@[Reached finished state.@]@.";
        Vector.iter (fun order -> Bars.add_order order state.bars)
        @@ Pmutex.get mutices.orders_mutex;
        let stats_with_orders =
          Stats.add_orders state.order_history state.stats
        in
        Pmutex.set mutices.data_mutex state.bars;
        Pmutex.set mutices.stats_mutex stats_with_orders;
        Pmutex.set mutices.indicators_mutex state.indicators;
        let filename = get_filename () in
        output_data state filename;
        output_order_history state filename;
        let tearsheet = Tearsheet.make state in
        Eio.traceln "%a" Tearsheet.pp tearsheet;
        Result.fail code
    | _ ->
        invalid_arg
          "Strategies.handle_nonlogical_state: unhandled nonlogical state"
end
