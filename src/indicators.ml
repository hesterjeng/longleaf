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

(* Accumulation distirbution line *)
let adl previous_adl (current : Item.t) =
  money_flow_volume current +. previous_adl

(* Exponential moving average *)
(* Length is the number of data points so far *)
(* Previous is the previous EMA value *)
(* Latest is the latest price *)
let ema length previous latest =
  let price = Item.last latest in
  let alpha = 2.0 /. (length +. 1.0) in
  previous +. (alpha *. (price -. previous))

module Point = struct
  type t = {
    timestamp : Time.t option;
    accumulation_distribution_line : float;
    exponential_moving_average : float;
  }
  [@@deriving show, yojson]

  let initial () : t =
    {
      timestamp = None;
      accumulation_distribution_line = 0.0;
      exponential_moving_average = 0.0;
    }

  let of_latest timestamp length (previous : t) (latest : Item.t) =
    {
      timestamp = Some timestamp;
      accumulation_distribution_line =
        adl previous.accumulation_distribution_line latest;
      exponential_moving_average =
        ema length previous.exponential_moving_average latest;
    }
end

(* let simple_moving_average n (l : Bars.symbol_history) = *)
(*   let length = Vector.length l in *)
(*   let close = Vector.map Item.close l in *)
(*   let start = Int.max (length - n) 0 in *)
(*   let window = Vector.slice_iter close start n in *)
(*   let sum = Iter.fold ( +. ) 0.0 window in *)
(*   sum /. Float.of_int n *)

type t = Point.t Vector.vector Hashtbl.t

let pp : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp String.pp (Vector.pp Point.pp) in
  Format.fprintf fmt "@[%a@]@." pp seq

let empty () = Hashtbl.create 100
let get (x : t) symbol = Hashtbl.find_opt x symbol

let add_latest timestamp (latest_bars : Bars.Latest.t) (x : t) =
  Hashtbl.to_seq latest_bars |> fun seq ->
  let iter f = Seq.iter f seq in
  iter @@ fun (symbol, latest) ->
  let indicators_vector =
    match get x symbol with
    | Some i -> i
    | None ->
        Eio.traceln "Creating initial indicators for %s." symbol;
        let new_vector = Vector.create () in
        Hashtbl.replace x symbol new_vector;
        new_vector
  in
  let length = Vector.length indicators_vector |> Float.of_int in
  let previous =
    match Vector.top indicators_vector with
    | Some p -> p
    | None -> Point.initial ()
  in
  let new_indicators = Point.of_latest timestamp length previous latest in
  Vector.push indicators_vector new_indicators
