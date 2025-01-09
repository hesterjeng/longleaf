module type S = sig
  val run : unit -> string
  val shutdown : unit -> unit
end

module type BUILDER = functor (_ : Backend.S) -> S
