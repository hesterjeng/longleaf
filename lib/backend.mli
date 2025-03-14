module type S = Backend_intf.S

val make_bars : Options.t -> (Bars.t * Bars.t option, Error.t) result

val make :
  Options.t ->
  Bars.t option ->
  Bars.t option option ->
  ((module Backend_intf.S), Error.t) result
(** Using only the options and context, create an instantiated backend. This
    backend can then be used to instantiate a strategy for running.*)
