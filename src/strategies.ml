module type S = sig
  val run : unit -> string
  val shutdown : unit -> unit
end

module type STRAT_BUILDER = functor (_ : Backend.S) -> S

module Log = (val Logs.src_log Logs.(Src.create "strategies"))

module Strategy_utils (Backend : Backend.S) = struct
  let listen_tick bars =
    Eio.Fiber.any
    @@ [
         (fun () ->
           Pmutex.set Backend.LongleafMutex.data_mutex bars;
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
           Pmutex.set Backend.LongleafMutex.data_mutex bars;
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
    let order_history : State.order_history =
      Hashtbl.to_list state.order_history
    in
    let json =
      State.yojson_of_order_history order_history |> Yojson.Safe.to_string
    in
    let filename = Format.sprintf "data/order_history_%s.json" filename in
    let oc = open_out filename in
    output_string oc json;
    close_out oc

  let handle_nonlogical_state (current : State.nonlogical_state)
      (state : _ State.t) =
    match current with
    | `Initialize -> Result.return @@ { state with current = `Listening }
    | `Listening -> (
        match listen_tick state.bars with
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
        Pmutex.set Backend.LongleafMutex.data_mutex state.bars;
        let filename = get_filename () in
        output_data state filename;
        output_order_history state filename;
        Result.fail code
end

module SimpleStateMachine (Backend : Backend.S) : S = struct
  open Trading_types
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))

  let shutdown = Backend.shutdown

  type state = unit State.t

  let init_state : state =
    {
      State.current = `Initialize;
      bars = Bars.empty;
      latest_bars = Bars.empty;
      content = ();
      order_history = Hashtbl.create 20;
    }

  module SU = Strategy_utils (Backend)

  let step (state : 'a State.t) =
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering ->
        let msft = Bars.price state.latest_bars "MSFT" in
        let nvda = Bars.price state.latest_bars "NVDA" in
        let cash_available = Backend.get_cash () in
        let qty =
          match cash_available >=. 0.0 with
          | true ->
              let tenp = cash_available *. 0.5 in
              let max_amt = tenp /. nvda.close in
              if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
          | false -> 0
        in
        (* Actually do the trade *)
        let () =
          if msft.close <. nvda.close then ()
          else
            let order : Order.t =
              let symbol = "NVDA" in
              let side = Side.Buy in
              let tif = TimeInForce.Day in
              let order_type = OrderType.Market in
              let price = nvda.close in
              Order.make ~symbol ~side ~tif ~order_type ~price ~qty
            in
            let time = msft.timestamp in
            let _json_resp = Backend.place_order state time order in
            ()
        in
        Result.return @@ { state with current = `Listening }

  let run () = SU.run ~init_state step
end
