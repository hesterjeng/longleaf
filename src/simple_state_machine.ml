module SimpleStateMachine (Backend : Backend.S) : Strategies.S = struct
  open Trading_types

  let shutdown = Backend.shutdown

  type state = unit State.t

  let init_state : state =
    {
      State.current = `Initialize;
      bars = Bars.empty ();
      latest = Bars.Latest.empty ();
      content = ();
      order_history = Vector.create ();
    }

  module SU = Strategies.Strategy_utils (Backend)

  let step (state : 'a State.t) =
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering ->
        let msft = Bars.Latest.get state.latest "MSFT" in
        let nvda = Bars.Latest.get state.latest "NVDA" in
        let cash_available = Backend.get_cash () in
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
              let symbol = "NVDA" in
              let side = Side.Buy in
              let tif = TimeInForce.Day in
              let order_type = OrderType.Market in
              let price = nvda_last in
              let timestamp = Item.timestamp msft in
              Order.make ~symbol ~side ~tif ~order_type ~price ~qty ~timestamp
                ~profit:None ~reason:"SimpleStateMachine"
            in
            let _json_resp = Backend.place_order state order in
            ()
        in
        Result.return @@ { state with current = `Listening }

  let run () = SU.run ~init_state step
end
