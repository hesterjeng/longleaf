type t =
  [ `Initialize
  | `Listening
  | `Ordering
  | `Liquidate
  | `Lock
  | (* | `LiquidateContinue *)
    `Continue
  | `BeginShutdown
  | `Finished of string ]
[@@deriving show { with_path = false }]
