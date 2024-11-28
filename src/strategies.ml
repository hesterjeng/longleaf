module type S = sig
  val run : unit -> string
  val shutdown : unit -> unit
end

module type STRAT_BUILDER = functor (_ : Backend.S) -> S

module Log = (val Logs.src_log Logs.(Src.create "strategies"))

module Strategy_utils (Backend : Backend.S) = struct
  let listen_tick orders bars =
    Eio.Fiber.any
    @@ [
         (fun () ->
           Pmutex.set Backend.LongleafMutex.data_mutex bars;
           Pmutex.set Backend.LongleafMutex.orders_mutex orders;
           match Backend.next_market_open () with
           | None ->
               Backend.Ticker.tick Backend.env;
               `Continue
           | Some open_time -> (
               let open_time = Ptime.to_float_s open_time in
               Eio.traceln "@[Waiting until market open...@]@.";
               Eio.Time.sleep_until Backend.env#clock open_time;
               Eio.traceln "@[Market is open, resuming.@]@.";
               Eio.Time.now Backend.env#clock |> Ptime.of_float_s |> function
               | Some t ->
                   Eio.traceln "@[Current time: %a@]@." Time.pp t;
                   `Continue
               | None ->
                   Eio.traceln "@[Detected an illegal time!  Shutting down.@]@.";
                   `Shutdown_signal));
         (fun () ->
           while
             let shutdown = Pmutex.get Backend.LongleafMutex.shutdown_mutex in
             not shutdown
           do
             Ticker.OneSecond.tick Backend.env
           done;
           Eio.traceln "@[Shutdown command received by shutdown mutex.@]@.";
           `Shutdown_signal);
       ]

  let run ~init_state step =
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
              s
          | _ -> try_liquidating ())
    in
    go init_state

  let get_filename () = Lots_of_words.select () ^ "_" ^ Lots_of_words.select ()

  let output_data (state : _ State.t) filename =
    Eio.traceln "cash: %f" (Backend.get_cash ());
    if Backend.is_backtest then ()
    else
      let json = Bars.yojson_of_t state.bars |> Yojson.Safe.to_string in
      (* let tail = Lots_of_words.select () ^ "_" ^ Lots_of_words.select () in *)
      let filename = Format.sprintf "data/live_%s.json" filename in
      let oc = open_out filename in
      output_string oc json;
      close_out oc

  let output_order_history (state : _ State.t) filename =
    let json_str =
      Order_history.yojson_of_t state.order_history |> Yojson.Safe.to_string
    in
    let filename = Format.sprintf "data/order_history_%s.json" filename in
    let oc = open_out filename in
    output_string oc json_str;
    close_out oc

  let handle_nonlogical_state (current : State.nonlogical_state)
      (state : _ State.t) =
    match current with
    | `Initialize -> Result.return @@ { state with current = `Listening }
    | `Listening -> (
        match listen_tick state.order_history state.bars with
        | `Continue ->
            let open Result.Infix in
            let+ latest_bars = Backend.latest_bars Backend.symbols in
            let bars = Bars.combine [ latest_bars; state.bars ] in
            { state with current = `Ordering; bars; latest_bars }
        | `Shutdown_signal ->
            Eio.traceln "Attempting to liquidate positions before shutting down";
            Result.return { state with current = `Liquidate })
    | `Liquidate ->
        Backend.liquidate state;
        Result.return
        @@ { state with current = `Finished "Liquidation finished" }
    | `Finished code ->
        Eio.traceln "@[Reached finished state.@]@.";
        let bars = state.bars in
        Vector.iter (fun order -> Bars.add_order order bars)
        @@ Pmutex.get Backend.LongleafMutex.orders_mutex;
        Pmutex.set Backend.LongleafMutex.data_mutex bars;
        let filename = get_filename () in
        output_data state filename;
        output_order_history state filename;
        Result.fail code
end
