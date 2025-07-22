module Make (Backend : Longleaf_backend.S) : Strategy.S = struct
  module SU = Longleaf_backend.Utils.Make (Backend)

  let init_state = Backend.init_state ()

  let shutdown () =
    Eio.traceln "Shutting down listener.";
    ()

  let step (state : _ Longleaf_state.t) =
    match Longleaf_state.current state with
    | Ordering -> Result.return @@ Longleaf_state.listen state
    | _ -> SU.handle_nonlogical_state state

  let run () = SU.run ~init_state step
end
