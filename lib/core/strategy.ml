module Util = Longleaf_util

module type S = sig
  type state

  val run : unit -> (float, Error.t) result
  val run_state : unit -> (state, Error.t) result
  val shutdown : unit -> unit
end

let dummy f =
  let module Dummy = struct
    type state = unit

    let run () =
      let _ = f () in
      Ok 0.0

    let run_state () = Ok ()
    let shutdown () = ()
  end in
  (module Dummy : S)

(* module type BUILDER = functor (_ : Backend.S) -> S *)

(* type builder = (module BUILDER) *)
