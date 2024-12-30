module Hashtbl = Hashtbl.Make (String)

let money_flow_multiplier (x : Item.t) =
  let open Float in
  let open Item in
  let close = close x in
  let low = low x in
  let high = high x in
  (close - low - (high - close)) / (high - low)

let money_flow_volume (x : Item.t) =
  let open Float in
  let open Item in
  let volume = volume x |> Float.of_int in
  volume * money_flow_multiplier x

(* This is pretty inefficient, as it recalculates the whole thing every time *)
let accumulation_distribution_line_whole (l : Bars.symbol_history) =
  let mfv_vector = Vector.map money_flow_volume l in
  Vector.fold ( +. ) 0.0 mfv_vector

let accumulation_distribution_line previous_adl (current : Item.t) =
  money_flow_volume current +. previous_adl

let simple_moving_average n (l : Bars.symbol_history) =
  let length = Vector.length l in
  let close = Vector.map Item.close l in
  let start = Int.max (length - n) 0 in
  let window = Vector.slice_iter close start n in
  let sum = Iter.fold ( +. ) 0.0 window in
  sum /. Float.of_int n

module Point = struct
  type t = { timestamp : Time.t; adl : float; sma : float }
  [@@deriving show, yojson]
end

module Latest = struct
  type t = Point.t Hashtbl.t

  let empty () : t = Hashtbl.create 0

  let pp : t Format.printer =
   fun fmt x ->
    let seq = Hashtbl.to_seq x in
    let pp = Seq.pp @@ Pair.pp String.pp Point.pp in
    Format.fprintf fmt "@[%a@]@." pp seq

  let get x symbol =
    match Hashtbl.find_opt x symbol with
    | Some x -> x
    | None -> invalid_arg "Unable to find price of symbol (Indicators.Latest)"

  let of_latest (x : Bars.t) (latest : Bars.Latest.t) = invalid_arg "NYI"
end

type t = Point.t Vector.vector Hashtbl.t

let pp : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp String.pp (Vector.pp Point.pp) in
  Format.fprintf fmt "@[%a@]@." pp seq

let empty () = Hashtbl.create 100
