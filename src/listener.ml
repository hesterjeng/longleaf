module Make (Backend : Backend.S) : Strategy.S = struct
  module SU = Strategy_utils.Make (Backend)

  let init_state = Backend.init_state ()

  let shutdown () =
    Eio.traceln "Shutting down listener.";
    ()

  let step (state : _ State.t) =
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> Result.return @@ State.listen state

  let run () = SU.run ~init_state step
end
