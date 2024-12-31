module Make (Backend : Backend.S) : Strategies.S = struct
  module SU = Strategies.Strategy_utils (Backend)

  let qty (state : _ State.t) pct symbol =
    let cash_available = Backend.get_cash () in
    match cash_available >=. 0.0 with
    | true ->
        let tenp = cash_available *. pct in
        let current_price = Item.last @@ Bars.Latest.get state.latest symbol in
        let max_amt = tenp /. current_price in
        if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
    | false -> 0

  let init_state = Backend.init_state None

  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let step (state : _ State.t) =
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        let most_recent_price = Bars.Latest.get state.latest "SPY" in
        match state.content with
        | Some () -> Result.return @@ { state with current = `Listening }
        | None ->
            let ( let* ) = Result.( let* ) in
            let order =
              Trading_types.Order.make ~symbol:"SPY" ~side:Buy
                ~tif:GoodTillCanceled
                ~price:(Item.last most_recent_price)
                ~timestamp:(Item.timestamp most_recent_price)
                ~qty:(qty state 0.5 "SPY") ~profit:None ~order_type:Market
                ~reason:[ "Buy and hold SPY" ]
            in
            let* () = Backend.place_order state order in
            Result.return
            @@ { state with current = `Listening; content = Some () })

  let run () = SU.run ~init_state step
end
