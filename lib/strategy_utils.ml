module Make (Backend : Backend_intf.S) = struct
  module Input = Backend.Input

  let ( let* ) = Result.( let* )
  let options = Input.options
  let mutices : Longleaf_mutex.t = options.mutices
  let runtype = options.flags.runtype

  let listen_tick () : (State.state, Error.t) result =
    Eio.Fiber.any
    @@ [
         (fun () ->
           let* nmo = Backend.next_market_open () in
           match nmo with
           | None ->
             Ticker.tick ~runtype Backend.env Input.options.tick;
             Result.return State.Continue
           | Some open_time -> (
             Eio.traceln "@[Current time: %a@]@." (Option.pp Time.pp)
               (Eio.Time.now Backend.env#clock |> Ptime.of_float_s);
             Eio.traceln "@[Open time: %a@]@." Time.pp open_time;
             let open_time = Ptime.to_float_s open_time in
             Eio.traceln "@[Waiting until market open...@]@.";
             Eio.Time.sleep_until Backend.env#clock open_time;
             Eio.traceln "@[Market is open, resuming.@]@.";
             Eio.Time.now Backend.env#clock |> Ptime.of_float_s |> function
             | Some t ->
               Eio.traceln "@[Current time: %a@]@." Time.pp t;
               Ticker.tick ~runtype Backend.env 5.0;
               Eio.traceln "@[Waited five seconds.@]@.";
               Result.return @@ State.Continue
             | None ->
               Eio.traceln "@[Detected an illegal time!  Shutting down.@]@.";
               Result.return @@ State.BeginShutdown));
         (fun () ->
           while
             let shutdown = Pmutex.get mutices.shutdown_mutex in
             not shutdown
           do
             Eio.Fiber.yield ();
             Ticker.tick ~runtype Backend.env 1.0
           done;
           Eio.traceln "@[Shutdown command received by shutdown mutex.@]@.";
           Result.return @@ State.BeginShutdown);
         (fun () ->
           let* close_time = Backend.next_market_close () in
           let* now =
             Eio.Time.now Backend.env#clock |> Ptime.of_float_s |> function
             | Some t -> Result.return t
             | None ->
               Error.fatal
                 "Unable to get clock time in listen_tick for market close"
           in
           let time_until_close =
             Ptime.diff close_time now |> Ptime.Span.to_float_s
           in
           while time_until_close >=. 600.0 do
             Eio.Fiber.yield ()
           done;
           Eio.traceln
             "@[Liquidating because we are within 10 minutes to market \
              close.@]@.";
           Result.return State.BeginShutdown);
         (* match context.flags.resume_after_liquidate with *)
         (* | false -> Result.return @@ State.BeginShutdown *)
         (* | true -> *)
         (*   Eio.traceln *)
         (*     "Liquidating and then continuing because we are approaching \ *)
           (*      market close."; *)
         (*   Result.return @@ State.LiquidateContinue); *)
       ]

  let run ~init_state step =
    let rec go prev =
      let stepped = step prev in
      match stepped with
      | Ok x -> go x
      | Error e -> (
        let try_liquidating () =
          Eio.traceln
            "@[Trying to liquidate because of a signal or error: %a@]@."
            Error.pp e;
          let liquidate = { prev with State.current = Liquidate } in
          go liquidate
        in
        match prev.current with
        | Liquidate
        | Finished _ ->
          Eio.traceln "@[Exiting run.@]@.";
          Backend_position.get_cash prev.positions
        | _ -> try_liquidating ())
    in
    match init_state with
    | Ok init_state -> go init_state
    | Error e ->
      Eio.traceln "[error] %a" Error.pp e;
      0.0

  let get_filename () = Lots_of_words.select () ^ "_" ^ Lots_of_words.select ()

  let output_data (state : _ State.t) filename =
    let ( let* ) = Result.( let* ) in
    match options.flags.save_to_file with
    | true ->
      let prefix =
        match Backend.is_backtest with
        | true -> "backtest"
        | false -> "live"
      in
      Eio.traceln "Saving all bars...";
      let* () = Bars.print_to_file ~filename state.bars prefix in
      let* () =
        Bars.print_to_file ~filename Backend.received_data (prefix ^ "_received")
      in
      Result.return ()
    | false -> Result.return ()

  let output_order_history (state : _ State.t) filename =
    if options.flags.save_to_file then (
      let json_str =
        Order.History.yojson_of_t state.order_history |> Yojson.Safe.to_string
      in
      let filename = Format.sprintf "data/order_history_%s.json" filename in
      let oc = open_out filename in
      output_string oc json_str;
      close_out oc)
    else ()

  let update_continue (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    (* let previous = state.latest in *)
    let* latest = Backend.latest_bars Backend.symbols state.bars state.tick in
    (* We have the index, we have state.bars, we have the newest info *)
    (* Here we can update indicators, BEFORE appending to bars *)
    let* positions =
      Backend_position.update state.positions state.bars state.tick
    in
    let* time = Bars.Latest.timestamp latest in
    assert (Ptime.compare time state.time = 1);
    (* let* () = Bars.append latest state.bars in *)
    (* let* indicators = *)
    (*   Indicators.compute_latest context.compare_preloaded *)
    (*     Input.options.indicators_config state.bars state.indicators *)
    (* in *)
    let* value = Backend_position.value positions latest in
    let risk_free_value =
      Stats.risk_free_value state.stats Input.options.tick
    in
    let stats =
      Stats.cons state.stats
      @@ {
           Stats.time;
           value;
           orders = [];
           risk_free_value;
           cash = Backend_position.get_cash state.positions;
         }
    in
    if options.flags.print_tick_arg then
      Eio.traceln "[ %a ] CASH %f" Time.pp time value;
    Result.return
    @@ { state with stats; positions; time; tick = state.tick + 1 }

  let start_time = ref 0.0

  let handle_nonlogical_state (state : _ State.t) =
    let ( let* ) = Result.( let* ) in
    (* Eio.traceln "There are %d bindings in state.bars" *)
    (*   (Bars.Hashtbl.length state.bars); *)
    match state.current with
    | Initialize ->
      Eio.traceln "Initialize state...";
      let symbols_str =
        List.map Instrument.symbol Backend.symbols |> String.concat ","
      in
      Pmutex.set mutices.symbols_mutex (Some symbols_str);
      start_time := Eio.Time.now Backend.env#clock;
      Eio.traceln "Running...";
      Result.return @@ { state with current = Listening }
    | Listening -> (
      if not options.flags.no_gui then (
        Pmutex.set mutices.data_mutex state.bars;
        Pmutex.set mutices.orders_mutex state.order_history;
        Pmutex.set mutices.stats_mutex state.stats
        (* Pmutex.set mutices.indicators_mutex state.indicators *));
      (* Eio.traceln "tick"; *)
      let* listened = listen_tick () in
      match listened with
      | Continue ->
        let* state = update_continue state in
        Result.return @@ { state with current = Ordering }
      | BeginShutdown ->
        Eio.traceln "Attempting to liquidate positions before shutting down";
        Result.return { state with current = Liquidate }
      | LiquidateContinue ->
        Eio.traceln "Strategies.listen_tick resturned LiquidateContinue";
        Result.return { state with current = LiquidateContinue }
      | _ ->
        Error.fatal
          "Strategies.handle_nonlogical_state: unhandled return value from \
           listen_tick")
    | Liquidate ->
      let* state = Backend.liquidate state in
      Result.return @@ { state with current = Finished "Liquidation finished" }
    | LiquidateContinue ->
      let* state = Backend.liquidate state in
      Ticker.tick ~runtype Backend.env 600.0;
      Result.return { state with current = Listening }
    | Finished code ->
      Eio.traceln "@[Reached finished state.@]@.";
      if not options.flags.no_gui then (
        let stats_with_orders =
          Stats.add_orders state.order_history state.stats
        in
        Pmutex.set mutices.data_mutex state.bars;
        Pmutex.set mutices.stats_mutex stats_with_orders
        (* Pmutex.set mutices.indicators_mutex state.indicators *));
      let filename = get_filename () in
      let* () = output_data state filename in
      output_order_history state filename;
      let tearsheet = Tearsheet.make state in
      Eio.traceln "%a" Tearsheet.pp tearsheet;
      assert (Backend_position.is_empty state.positions);
      Eio.traceln "Done... %fs" (Eio.Time.now Backend.env#clock -. !start_time);
      Result.fail @@ `Finished code
    | Ordering
    | Continue
    | BeginShutdown ->
      Error.fatal
        "Strategies.handle_nonlogical_state: unhandled nonlogical state"
end
