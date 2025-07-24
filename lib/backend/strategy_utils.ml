module State = Longleaf_state
module Pmutex = Longleaf_util.Pmutex
module Options = Longleaf_core.Options
module Mode = State.Mode
module Bars = Longleaf_bars
(* module Server = Longleaf_server *)

module Make (Backend : Backend.S) = struct
  module Input = Backend.Input

  let ( let* ) = Result.( let* )
  let options = Input.options
  let mutices : State.Mutex.t = Input.mutices
  let runtype = options.flags.runtype

  let listen_tick () : (State.Mode.t, Error.t) result =
    Eio.Fiber.any
    @@ [
         (fun () ->
           let* nmo = Backend.next_market_open () in
           match nmo with
           | None ->
             Ticker.tick ~runtype Backend.env Input.options.tick;
             Result.return Mode.Continue
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
               Result.return @@ Mode.Continue
             | None ->
               Eio.traceln "@[Detected an illegal time!  Shutting down.@]@.";
               Result.return @@ Mode.BeginShutdown));
         (fun () ->
           while
             let shutdown = Pmutex.get mutices.shutdown_mutex in
             not shutdown
           do
             Eio.Fiber.yield ();
             Ticker.tick ~runtype Backend.env 1.0
           done;
           Eio.traceln "@[Shutdown command received by shutdown mutex.@]@.";
           Result.return @@ Mode.BeginShutdown);
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
           Result.return Mode.BeginShutdown);
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
          go @@ State.liquidate prev
        in
        match State.current prev with
        | Liquidate
        | Finished _ ->
          Eio.traceln "@[Exiting run.@]@.";
          State.cash prev (* 0.0 *)
        | _ -> try_liquidating ())
    in
    match init_state with
    | Ok init_state -> go init_state
    | Error e ->
      Eio.traceln "[error] %a" Error.pp e;
      0.0

  let get_filename () = Longleaf_util.random_filename ()

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
      let bars = State.bars state in
      let* () = Bars.print_to_file ~filename bars prefix in
      let* () =
        Bars.print_to_file ~filename Backend.received_data (prefix ^ "_received")
      in
      Result.return ()
    | false -> Result.return ()

  let output_order_history (_state : _ State.t) filename =
    if options.flags.save_to_file then (
      let json_str =
        `List [] |> Yojson.Safe.to_string
        (* TODO: get order history from trading_state *)
      in
      let filename = Format.sprintf "data/order_history_%s.json" filename in
      let oc = open_out filename in
      output_string oc json_str;
      close_out oc)
    else ()

  let update_continue (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    let bars = State.bars state in
    let res = State.increment_tick state in
    Bars.set_current bars @@ State.tick res;
    let* () = Backend.update_bars Backend.symbols bars @@ State.tick res in
    let* _value = State.value state in
    Result.return res

  let start_time = ref 0.0

  let handle_nonlogical_state (state : _ State.t) =
    let ( let* ) = Result.( let* ) in
    (* Eio.traceln "There are %d bindings in state.bars" *)
    (*   (Bars.Hashtbl.length state.bars); *)
    let bars = State.bars state in
    let tick = State.tick state in
    match State.current state with
    | Initialize ->
      Eio.traceln "Initialize state...";
      let symbols_str =
        List.map Instrument.symbol Backend.symbols |> String.concat ","
      in
      Bars.set_current bars tick;
      Eio.traceln "Bars initialize: %a" Bars.pp bars;
      Pmutex.set mutices.symbols_mutex (Some symbols_str);
      let indicator_config = (State.config state).indicator_config in
      let* () =
        Longleaf_indicators.Indicators.compute_all
          ~eio_env:Input.options.eio_env indicator_config
        @@ State.bars state
      in
      start_time := Eio.Time.now Backend.env#clock;
      Eio.traceln "Running...";
      Result.return @@ State.set state Listening
    | Listening -> (
      if not options.flags.no_gui then Pmutex.set mutices.state_mutex state;
      (* Eio.traceln "tick"; *)
      let* listened = listen_tick () in
      let* length = Bars.length bars in
      match listened with
      | Continue when tick >= length - 1 ->
        Result.return @@ State.set state Liquidate
      | Continue ->
        let* state = update_continue state in
        Result.return @@ State.set state Ordering
      | BeginShutdown ->
        Eio.traceln "Attempting to liquidate positions before shutting down";
        Result.return @@ State.set state Liquidate
      | LiquidateContinue ->
        Eio.traceln "Strategies.listen_tick resturned LiquidateContinue";
        Result.return @@ State.set state LiquidateContinue
      | _ ->
        Error.fatal
          "Strategies.handle_nonlogical_state: unhandled return value from \
           listen_tick")
    | Liquidate ->
      let* state = Backend.liquidate state in
      Result.return @@ State.set state (Finished "Liquidation finished")
    | LiquidateContinue ->
      let* state = Backend.liquidate state in
      Ticker.tick ~runtype Backend.env 600.0;
      Result.return @@ State.set state Listening
    | Finished code ->
      let stats = State.stats state in
      Eio.traceln "@[Reached finished state %d. with %f after %d orders.@]@."
        tick (State.cash state) stats.num_orders;
      (* TODO: get order history length from trading_state *)
      Eio.traceln "state.bars at finish: %a" Bars.pp bars;
      Eio.traceln "Done... %fs" (Eio.Time.now Backend.env#clock -. !start_time);
      if not options.flags.no_gui then Pmutex.set mutices.state_mutex state;
      let filename = get_filename () in
      let* () = output_data state filename in
      output_order_history state filename;
      (* let tearsheet = Tearsheet.make state in *)
      (* Eio.traceln "%a" Tearsheet.pp tearsheet; *)
      (* assert (State.is_portfolio_empty state); *)
      Eio.traceln "Finished cleanup... %fs"
        (Eio.Time.now Backend.env#clock -. !start_time);
      Result.fail @@ `Finished code
    | Ordering
    | Continue
    | BeginShutdown ->
      Error.fatal
        "Strategies.handle_nonlogical_state: unhandled nonlogical state"
end
