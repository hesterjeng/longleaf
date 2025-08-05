open Gadt

let uid () = Var (Uuidm.v4_gen Util.random_state ())

let atr = App (Tacaml.Indicator.Raw.atr, uid ())
