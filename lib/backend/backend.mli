module type S = Backend_intf.S

module Bars = Longleaf_bars

val make :
  Longleaf_state.Mutex.t ->
  Bars.t ->
  Options.t ->
  ((module Backend_intf.S), Error.t) result
(** Using only the options and context, create an instantiated backend. This
    backend can then be used to instantiate a strategy for running.*)
