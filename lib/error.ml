type t =
  [ `MissingData of string
  | `MissingClient of string
  | `UnsupportedOrder of Order.t
  | (Piaf.Error.t[@printer Piaf.Error.pp_hum])
  | `JsonError of string
  | `FatalError of string ]
[@@deriving show { with_path = false }]
