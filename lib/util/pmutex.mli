type 'a t [@@deriving show, yojson]

(* val equal : 'a t -> 'a t -> bool *)

val make : 'a -> 'a t
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a
