module Reason = struct
  type t = string list [@@deriving show]
end

module Side = Trading_types.Side

(* let ( let&& ) x f = match x with true -> f () | false -> Ok None *)

type t = {
  instrument : Instrument.t;
  side : Side.t;
  reason : string list;
  flag : bool;
}
[@@deriving show { with_path = false }]

let make i s b = { instrument = i; side = s; reason = []; flag = b }

let ( let&& ) signal x f =
  match x with
  | true, _ -> f ()
  | false, s -> { signal with flag = false; reason = [ s ] }

let ( let|| ) signal x f =
  match x with
  | true, s -> { signal with flag = true; reason = [ s ] }
  | false, _ -> f ()

let ( let$ ) signal x f =
  match x with
  | None -> { signal with flag = false; reason = [ "missing option" ] }
  | Some x -> f x

let let_get_opt = ( let$ )
let let_or = ( let|| )
let let_and = ( let&& )
