module SimpleStateMachine (Backend : Backend.S) : Strategy.S = struct
  open Trading_types

  let shutdown = Backend.shutdown

  let init_state =
    Result.return
    @@ {
         State.current = Initialize;
         bars = Bars.empty ();
         latest = Bars.Latest.empty ();
         content = ();
         positions = Backend_position.make ();
         tick = 0;
         tick_length = 0.0;
         stats = Stats.empty ();
         order_history = Order.History.empty;
         indicators = Indicators.empty ();
       }

  module SU = Strategy_utils.Make (Backend)

  let step (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    match state.current with
    | Ordering ->
        let* msft = Bars.Latest.get state.latest @@ Security "MSFT" in
        let* nvda = Bars.Latest.get state.latest @@ Security "NVDA" in
        let cash_available = Backend_position.get_cash state.positions in
        let qty =
          match cash_available >=. 0.0 with
          | true ->
              let last = Item.last nvda in
              let tenp = cash_available *. 0.5 in
              let max_amt = tenp /. last in
              if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
          | false -> 0
        in
        (* Actually do the trade *)
        let () =
          let msft_last, nvda_last = Pair.map_same Item.last (msft, nvda) in
          if msft_last <. nvda_last then ()
          else
            let order : Order.t =
              let symbol = Instrument.Security "NVDA" in
              let side = Side.Buy in
              let tif = TimeInForce.Day in
              let order_type = OrderType.Market in
              let price = nvda_last in
              let timestamp = Item.timestamp msft in
              Order.make ~tick:state.tick ~symbol ~side ~tif ~order_type ~price
                ~qty ~timestamp ~profit:None ~reason:[ "SimpleStateMachine" ]
            in
            let _json_resp = Backend.place_order state order in
            ()
        in
        Result.return @@ { state with current = Listening }
    | _ -> SU.handle_nonlogical_state state

  let run () = SU.run ~init_state step
end
