module State = Longleaf_state
module Pmutex = Longleaf_util.Pmutex
module Options = Longleaf_core.Options
module Mode = State.Mode
module Bars = Longleaf_bars
module Indicators = Longleaf_indicators.Indicators
(* module Server = Longleaf_server *)

module Make (Backend : Backend.S) = struct
  module Input = Backend.Input

  let eio_env = Input.options.eio_env
  let ( let* ) = Result.( let* )
  let options = Input.options
  let mutices : State.Mutex.t = Input.mutices
  let runtype = options.flags.runtype

  let listen_tick env : (unit, Error.t) result =
    match runtype with
    | Live
    | Paper ->
      Eio.Fiber.any
      @@ [
           (fun () ->
             let* nmo = Backend.next_market_open () in
             match nmo with
             | None ->
               Ticker.tick ~runtype env Input.options.tick;
               Result.return ()
             | Some open_time ->
               Eio.traceln "@[Current time: %a@]@." (Option.pp Time.pp)
                 (Eio.Time.now env#clock |> Ptime.of_float_s);
               Eio.traceln "@[Open time: %a@]@." Time.pp open_time;
               let open_time = Ptime.to_float_s open_time in
               Eio.traceln "@[Waiting until market open...@]@.";
               Eio.Time.sleep_until env#clock open_time;
               Eio.traceln "@[Market is open, resuming.@]@.";
               Eio.Time.now env#clock |> Ptime.of_float_s |> ( function
               | Some t ->
                 Eio.traceln "@[Current time: %a@]@." Time.pp t;
                 Ticker.tick ~runtype env 5.0;
                 Eio.traceln "@[Waited five seconds.@]@.";
                 Result.return ()
               | None ->
                 Error.fatal "Detected an illegal time! strategy_util.ml" ));
           (fun () ->
             while
               let shutdown = Pmutex.get mutices.shutdown_mutex in
               not shutdown
             do
               Eio.Fiber.yield ();
               Ticker.tick ~runtype env 1.0
             done;
             Error.fatal "Shutdown command received by shutdown mutexx");
           (fun () ->
             let* close_time = Backend.next_market_close () in
             let* now =
               Eio.Time.now env#clock |> Ptime.of_float_s |> function
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
             Result.return ());
         ]
    | _ -> Result.return ()

  (* let run ~init_state step = *)
  (*   let rec go prev = *)
  (*     let stepped = step prev in *)
  (*     match stepped with *)
  (*     | Ok x -> go x *)
  (*     | Error e -> *)
  (*       let try_liquidating () = *)
  (*         Eio.traceln *)
  (*           "@[Trying to liquidate because of a signal or error: %a@]@." *)
  (*           Error.pp e; *)
  (*         go @@ State.liquidate prev *)
  (*       in *)
  (*       (match State.current prev with *)
  (*       | Liquidate *)
  (*       | Finished _ -> *)
  (*         Eio.traceln "@[Exiting run.@]@."; *)
  (*         State.cash prev (\* 0.0 *\) *)
  (*       | _ -> try_liquidating ()) *)
  (*   in *)
  (*   match init_state with *)
  (*   | Ok init_state -> go init_state *)
  (*   | Error e -> *)
  (*     Eio.traceln "[error] %a" Error.pp e; *)
  (*     0.0 *)

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
    let* res = State.increment_tick state in
    let* () = Backend.update_bars Backend.symbols bars @@ State.tick res in
    let* () =
      match Input.options.flags.runtype with
      | Live
      | Paper ->
        let indicator_config = (State.config state).indicator_config in
        Indicators.Calc.compute_single (State.tick res) eio_env indicator_config
        @@ State.bars state
      | _ -> Result.return ()
    in
    (* let* _value = State.value state in *)
    Result.return res

  module Initialize : sig
    val top :
      [ `Initialize ] State.t -> ([ `Listening ] State.t, Error.t) result
  end = struct
    let top (state : [ `Initialize ] State.t) =
      Eio.traceln "Initialize state...";
      let symbols_str =
        List.map Instrument.symbol Backend.symbols |> String.concat ","
      in
      Pmutex.set mutices.symbols_mutex (Some symbols_str);
      let* () =
        let indicator_config = (State.config state).indicator_config in
        Indicators.Calc.compute_all eio_env indicator_config @@ State.bars state
      in
      let* state : [ `Listening ] State.t =
        (* If we are in live or paper, we need to grow the bars to have somewhere \ *)
        (* to put the received data. *)
        match Input.options.flags.runtype with
        | Live
        | Paper ->
          let* bars_length = Bars.length @@ State.bars state in
          let state =
            State.set_tick state (bars_length - 1) |> State.grow |> State.listen
          in
          Eio.traceln "Initialize state: %a" State.pp state;
          Result.return state
        | _ -> State.listen state |> Result.return
      in
      Eio.traceln "Finished with initialization: %d..." (State.tick state);
      Result.return state
  end

  module Listen = struct
    let top (state : [ `Listening ] State.t) :
        ([ `Ordering ] State.t, Error.t) result =
      if not options.flags.no_gui then
        Pmutex.set mutices.state_mutex @@ State.lock state;
      let* () = listen_tick eio_env in
      let* length = State.bars state |> Bars.length in
      let tick = State.tick state in
      tick >= length - 1 |> function
      | true ->
        let* state = Backend.liquidate state in
        State.set_finished_flag state |> State.ordering |> Result.return
      | false -> State.ordering state |> Result.return
  end

  module Liquidate = struct
    let top state =
      let* state = Backend.liquidate state in
      State.set_finished_flag state |> State.finished |> Result.return

    let top_continue state =
      let* state = Backend.liquidate state in
      Ticker.tick ~runtype eio_env 600.0;
      State.listen state |> Result.return
  end

  module Finish = struct
    let top state =
      let bars = State.bars state in
      let tick = State.tick state in
      Eio.traceln "@[Reached finished state %d. with %f after %d orders.@]@."
        tick (State.cash state)
        (State.orders_placed state);
      (* TODO: get order history length from trading_state *)
      Eio.traceln "state.bars at finish: %a" Bars.pp bars;
      Eio.traceln "Done...";
      if not options.flags.no_gui then Pmutex.set mutices.state_mutex state;
      let filename = get_filename () in
      let* () = output_data state filename in
      output_order_history state filename;
      Result.fail @@ `Finished "Finished in strategy_utils.ml"
  end

  (* let handle_nonlogical_state (state : _ State.t) = *)
  (*   match State.current state with *)
  (*   | Initialize -> Initialize.top state *)
  (*   | Listening -> Listener.top state *)
  (*   | Liquidate -> Liquidate.top state *)
  (*   | LiquidateContinue -> Liquidate.top_continue state *)
  (*   | Finished code -> *)
  (*     Eio.traceln "@[Reached finished state %d. with %f after %d orders.@]@." *)
  (*       tick (State.cash state) *)
  (*       (State.orders_placed state); *)
  (*     (\* TODO: get order history length from trading_state *\) *)
  (*     Eio.traceln "state.bars at finish: %a" Bars.pp bars; *)
  (*     Eio.traceln "Done..."; *)
  (*     if not options.flags.no_gui then Pmutex.set mutices.state_mutex state; *)
  (*     let filename = get_filename () in *)
  (*     let* () = output_data state filename in *)
  (*     output_order_history state filename; *)
  (*     Result.fail @@ `Finished code *)
  (*   | Ordering *)
  (*   | Continue *)
  (*   | BeginShutdown -> *)
  (*     Error.fatal *)
  (*       "Strategies.handle_nonlogical_state: unhandled nonlogical state" *)
end
