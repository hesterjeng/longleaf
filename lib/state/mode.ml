type t =
  [ `Initialize
  | `Listening
  | `Ordering
  | `Liquidate
  | (* | `LiquidateContinue *)
    `Continue
  | `BeginShutdown
  | `Finished of string ]
[@@deriving show { with_path = false }]
