open Gadt
module I = Tacaml.Indicator.Raw

let ( @@@ ) f x = App (f, x)
let uid () = Var (Uuidm.v4_gen Util.random_state ())
let atr = Fun I.atr @@@ uid ()
let stddev = Fun I.stddev @@@ uid () @@@ uid ()
