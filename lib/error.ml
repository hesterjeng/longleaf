type client = Data | Trading [@@deriving show { with_path = false }]

type t =
  [ `MissingData of string
  | `MissingClient of client
  | `UnsupportedOrder of Order.t
  | `NetworkError of (Piaf.Error.t[@printer Piaf.Error.pp_hum])
  | `JsonError of string
  | `FatalError of string ]
[@@deriving show { with_path = false }]
