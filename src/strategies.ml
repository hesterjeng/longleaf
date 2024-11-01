module State = struct
  type nonlogical_state =
    [ `Initialize | `Listening | `Liquidate | `Finished of string ]
  [@@deriving show { with_path = false }]

  type logical_state = [ `Ordering ] [@@deriving show { with_path = false }]

  type state = [ nonlogical_state | logical_state ]
  [@@deriving show { with_path = false }]

  type 'a t = { current : state; content : 'a }
  type ('a, 'b) status = Running of 'a t | Shutdown of 'b

  let continue x = Running x
  let shutdown x = Shutdown x
  let init () = { current = `Initialize; content = Trading_types.Bars.empty }
end

module type S = sig
  val run : unit -> string
  val shutdown : unit -> unit
end

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

  let listen_tick () =
    Eio.Fiber.any
    @@ [
         (fun () ->
           if market_closed Backend.is_backtest then (
             Eio.traceln "Waiting five minutes because the market is closed";
             Ticker.FiveMinute.tick Backend.env)
           else Backend.Ticker.tick Backend.env;
           `Continue);
         (fun () ->
           while not @@ Backend.Mutex.get_mutex () do
             Ticker.OneSecond.tick Backend.env
           done;
           `Shutdown_signal);
       ]

  let run step env =
    let init = State.init env in
    let rec go prev =
      let stepped = step prev in
      match stepped with
      | Ok x -> (
          match x with
          | State.Running now -> (go [@tailcall]) now
          | Shutdown code -> code)
      | Error s -> (
          let try_liquidating () =
            let liquidate = { prev with current = `Liquidate } in
            go liquidate
          in
          match prev.current with `Liquidate -> s | _ -> try_liquidating ())
    in
    go init

  let output_data (state : _ State.t) =
    let backtest = if Backend.is_backtest then "test" else "live" in
    let json =
      Trading_types.Bars.yojson_of_t state.content |> Yojson.Safe.to_string
    in
    let tail = Lots_of_words.select () ^ "_" ^ Lots_of_words.select () in
    let filename = Format.sprintf "data/%s_%s" backtest tail in
    let oc = open_out filename in
    output_string oc json;
    close_out oc;
    Log.app (fun k -> k "cash: %f" (Backend.get_cash ()))

  let handle_nonlogical_state (current : State.nonlogical_state)
      (state : _ State.t) =
    match current with
    | `Initialize ->
        Result.return @@ State.continue { state with current = `Listening }
    | `Listening -> (
        Result.return
        @@
        match listen_tick () with
        | `Continue -> State.continue { state with current = `Ordering }
        | `Shutdown_signal -> State.shutdown "Received shutdown signal")
    | `Liquidate ->
        Backend.liquidate ();
        Result.return
        @@ State.continue
             { state with current = `Finished "Successfully liquidated" }
    | `Finished code ->
        output_data state;
        Result.return @@ State.shutdown code
end

module SimpleStateMachine (Backend : Backend.S) : S = struct
  open Trading_types
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))

  let shutdown = Backend.shutdown

  module SU = Strategy_utils (Backend)

  let step (state : 'a State.t) =
    let open Result.Infix in
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering ->
        let* latest_bars = Backend.latest_bars [ "MSFT"; "NVDA" ] in
        let msft = Bars.price latest_bars "MSFT" in
        let nvda = Bars.price latest_bars "NVDA" in
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
        let new_bars = Bars.combine [ latest_bars; state.content ] in
        Result.return
        @@ State.continue { current = `Listening; content = new_bars }

  let run = SU.run step
end
