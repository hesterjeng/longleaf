(* type final = *)
(*   { *)
(*     final_position_value : float; *)
(*   } *)

(* type t = (final, string) result *)

module type S = sig
  val run : unit -> float
  val shutdown : unit -> unit
end

module type BUILDER = functor (_ : Backend_intf.S) -> S
