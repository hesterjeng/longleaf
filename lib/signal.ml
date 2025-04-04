module Reason = struct
  type t = string list [@@deriving show]
end

module Side = Trading_types.Side

type t = {
  instrument : Instrument.t; [@compare fun _ _ -> 0]
  side : Side.t; [@compare fun _ _ -> 0]
  reason : string list; [@compare fun _ _ -> 0]
  score : float;
}
[@@deriving show { with_path = false }]

(* type t = Pass of Instrument.t * Reason.t | Fail of Instrument.t * Reason.t *)
(* [@@deriving show { with_path = false }] *)
