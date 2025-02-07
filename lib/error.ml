type client = Data | Trading

type t =
  | MissingData of string
  | MissingClient of client
  | UnsupportedOrder of Order.t
  | Fatal of string
