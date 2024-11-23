type 'a t

val make : 'a -> 'a t
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a
