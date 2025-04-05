module Reason = struct
  type t = string list [@@deriving show]
end

module Side = Trading_types.Side

let ( let$ ) x f = match x with None -> Ok None | Some x -> f x
let ( let&& ) x f = match x with true -> f () | false -> Ok None

type t = { instrument : Instrument.t; side : Side.t; reason : string list }
[@@deriving show { with_path = false }]
