module State = struct
  type nonlogical_state =
    [ `Initialize | `Listening | `Liquidate | `Finished of string ]
  [@@deriving show { with_path = false }]

  type logical_state = [ `Ordering ] [@@deriving show { with_path = false }]

  type state = [ nonlogical_state | logical_state ]
  [@@deriving show { with_path = false }]

  type 'a t = {
    current : state;
    bars : Bars.t;
    latest_bars : Bars.t;
    content : 'a;
  }

  let init () =
    {
      current = `Initialize;
      bars = Bars.empty;
      latest_bars = Bars.empty;
      content = ();
    }
end

module type S = sig
  val run : unit -> string
  val shutdown : unit -> unit
end

module type STRAT_BUILDER = functor (_ : Backend.S) -> S

module Log = (val Logs.src_log Logs.(Src.create "strategies"))

module Strategy_utils (Backend : Backend.S) = struct
  let market_closed backtest =
    let res =
      let open CalendarLib in
      let time = Time.now () in
      let open_time = Calendar.Time.lmake ~hour:8 ~minute:30 () in
      let close_time = Calendar.Time.lmake ~hour:16 () in
      if
        backtest
        || Calendar.Time.compare time open_time = 1
           && Calendar.Time.compare time close_time = -1
      then false
      else true
    in
    res

  let listen_tick bars =
    Eio.Fiber.any
    @@ [
         (fun () ->
           Parametric_mutex.set Backend.LongleafMutex.data_mutex bars;
           if market_closed Backend.is_backtest then (
             Eio.traceln "Waiting five minutes because the market is closed";
             Ticker.FiveMinute.tick Backend.env)
           else Backend.Ticker.tick Backend.env;
           `Continue);
         (fun () ->
           while
             let shutdown =
               Parametric_mutex.get Backend.LongleafMutex.shutdown_mutex
             in
             not shutdown
           do
             Ticker.OneSecond.tick Backend.env
           done;
           Eio.traceln "@[Shutdown command received by strategy.@]@.";
           Parametric_mutex.set Backend.LongleafMutex.data_mutex bars;
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
          | `Liquidate | `Finished _ -> s
          | _ -> try_liquidating ())
    in
    go init_state

  let output_data (state : _ State.t) =
    Eio.traceln "cash: %f" (Backend.get_cash ());
    if Backend.is_backtest then ()
    else
      let json = Bars.yojson_of_t state.bars |> Yojson.Safe.to_string in
      let tail = Lots_of_words.select () ^ "_" ^ Lots_of_words.select () in
      let filename = Format.sprintf "data/live_%s" tail in
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
        Backend.liquidate ();
        Result.return
        @@ { state with current = `Finished "Liquidation finished" }
    | `Finished code ->
        Parametric_mutex.set Backend.LongleafMutex.data_mutex state.bars;
        output_data state;
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
              {
                symbol = "NVDA";
                side = Side.Buy;
                tif = TimeInForce.Day;
                order_type = OrderType.Market;
                qty;
                price = nvda.close;
              }
            in
            let _json_resp = Backend.create_order order in
            ()
        in
        Result.return @@ { state with current = `Listening }

  let run () = SU.run ~init_state step
end
