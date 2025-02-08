module Make (Backend : Backend.S) : Strategy.S = struct
  module SU = Strategy_utils.Make (Backend)

  let init_state = Backend.init_state ()

  let shutdown () =
    Eio.traceln "Shutting down listener.";
    ()

  let step (state : _ State.t) =
    match state.current with
    | Ordering -> Result.return @@ State.listen state
    | _ -> SU.handle_nonlogical_state state

  let run () = SU.run ~init_state step
end
