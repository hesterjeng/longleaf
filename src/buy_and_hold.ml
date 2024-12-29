module BuyAndHold (Backend : Backend.S) : Strategies.S = struct
  module SU = Strategies.Strategy_utils (Backend)

  let init_state = Backend.init_state None

  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let step (state : _ State.t) =
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        match state.content with
        | Some () -> Result.return @@ { state with current = `Listening }
        | None ->
            Eio.traceln "@[NYI: Buying SPY for buy and hold test...@]@.";
            Result.return @@ { state with current = `Listening })

  let run () = SU.run ~init_state step
end
