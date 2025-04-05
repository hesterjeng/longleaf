module Reason = struct
  type t = string list [@@deriving show]
end

module Side = Trading_types.Side

type t = { instrument : Instrument.t; side : Side.t; reason : string list }
[@@deriving show { with_path = false }]
