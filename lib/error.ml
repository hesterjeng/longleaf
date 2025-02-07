type client = Data | Trading [@@deriving show { with_path = false }]

type t =
  | MissingData of string
  | MissingClient of client
  | UnsupportedOrder of Order.t
  | Json of string
  | Fatal of string
[@@deriving show { with_path = false }]
