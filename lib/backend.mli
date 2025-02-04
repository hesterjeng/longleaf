module type S = Backend_intf.S

val make : Options.t -> (module Backend_intf.S)
(** Using only the options and context, create an instantiated backend. This
    backend can then be used to instantiate a strategy for running.*)
