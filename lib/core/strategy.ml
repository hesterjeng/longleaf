module Util = Longleaf_util

module type S = sig
  val run : unit -> (float, Error.t) result
  val shutdown : unit -> unit
end

let dummy f =
  let module Dummy = struct
    let run () =
      let _ = f () in
      Ok 0.0

    let shutdown () = ()
  end in
  (module Dummy : S)

(* module type BUILDER = functor (_ : Backend.S) -> S *)

(* type builder = (module BUILDER) *)
