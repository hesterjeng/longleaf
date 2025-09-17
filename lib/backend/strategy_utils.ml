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

  let eio_env = Input.options.eio_env
  let options = Input.options
  let mutices : State.Mutex.t = Input.mutices
  let runtype = options.flags.runtype

  module LiveIndicators = struct
    let top (state : State.t) =
      let* () =
        match Input.options.flags.runtype with
        | Live
        | Paper ->
          let indicator_config = (State.config state).indicator_config in
          Indicators.Calc.compute_single (State.tick state) eio_env
            indicator_config
          @@ State.bars state
        | _ -> Result.return ()
      in
      Result.return state
  end

  module Initialize : sig
    val top : State.t -> (State.t, Error.t) result
  end = struct
    let top (state : State.t) =
      Eio.traceln "Initialize state...";
      let symbols_str =
        List.map Instrument.symbol Backend.symbols |> String.concat ","
      in
      Pmutex.set mutices.symbols_mutex (Some symbols_str);
      let* () =
        let indicator_config = (State.config state).indicator_config in
        Indicators.Calc.compute_all eio_env indicator_config @@ State.bars state
      in
      let* state =
        (* If we are in live or paper, we need to grow the bars to have somewhere \ *)
        (* to put the received data. *)
        match Input.options.flags.runtype with
        | Live
        | Paper ->
          let* bars_length = Bars.length @@ State.bars state in
          let* state = State.set_tick state (bars_length - 1) |> State.grow in
          Eio.traceln "Initialize state: %a" State.pp state;
          Result.return state
        | _ -> Result.return state
      in
      Eio.traceln "Finished with initialization: %d..." (State.tick state);
      Result.return state
  end

  exception FinalState of State.t

  module Listen = struct
    let listen_tick state env : (unit, Error.t) result =
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
               raise (FinalState state));
             (* (fun () -> *)
             (*   let* close_time = Backend.next_market_close () in *)
             (*   let* now = *)
             (*     Eio.Time.now env#clock |> Ptime.of_float_s |> function *)
             (*     | Some t -> Result.return t *)
             (*     | None -> *)
             (*       Error.fatal *)
             (*         "Unable to get clock time in listen_tick for market close" *)
             (*   in *)
             (*   let time_until_close = *)
             (*     Ptime.diff close_time now |> Ptime.Span.to_float_s *)
             (*   in *)
             (*   while time_until_close >=. 600.0 do *)
             (*     Eio.Fiber.yield () *)
             (*   done; *)
             (*   Eio.traceln *)
             (*     "@[Liquidating because we are within 10 minutes to market \ *)
           (*      close.@]@."; *)
             (*   Result.return ()); *)
           ]
      | _ -> Result.return ()

    let top (state : State.t) : (State.t, Error.t) result =
      if not options.flags.no_gui then Pmutex.set mutices.state_mutex state;
      let* () = listen_tick state eio_env in
      Ok state
  end

  module Increment = struct
    let top state =
      let* length = State.bars state |> Bars.length in
      State.tick state >= length - 1 |> function
      | true -> raise (FinalState state)
      | false ->
        if options.flags.print_tick_arg then
          Eio.traceln "[ Increment.top ] %d" (State.tick state + 1);
        State.increment_tick state
  end

  module Liquidate = struct
    let top = Backend.liquidate

    let top_continue state =
      let* state = Backend.liquidate state in
      Eio.traceln "liquidate continue 10 minute wait...";
      Ticker.tick ~runtype eio_env 600.0;
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
    let rec loop state =
      GetData.top state >>= LiveIndicators.top >>= order >>= Increment.top
      >>= Listen.top >>= loop
    in
    let res =
      try loop init with
      | FinalState s -> Liquidate.top s >>= Finish.top
    in
    res
end
