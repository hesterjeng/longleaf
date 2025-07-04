module type S = sig
  val run : unit -> float
  val shutdown : unit -> unit
end

module type BUILDER = functor (_ : Backend.S) -> S

(** Helper function to reduce code duplication. *)
let run (module Strat : BUILDER) options =
  (* let options = run_options context in *)
  let ( let* ) = Result.( let* ) in
  let* backend = Backend.make options in
  let module Backend = (val backend) in
  let module S = Strat (Backend) in
  Eio.traceln "Applied strategy functor to backend, running.";
  let res = S.run () in
  Backend.shutdown ();
  Result.return res
