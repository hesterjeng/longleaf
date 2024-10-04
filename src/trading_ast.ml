type trigger = unit -> bool

type t =
  | Long of trigger
  | Short of trigger
