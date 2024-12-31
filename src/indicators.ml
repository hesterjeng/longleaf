module Hashtbl = Hashtbl.Make (String)

let money_flow_multiplier (x : Item.t) =
  let open Float in
  let open Item in
  let close = close x in
  let low = low x in
  let high = high x in
  if Float.equal high low then 0.0
  else (close - low - (high - close)) / (high - low)

let money_flow_volume (x : Item.t) =
  let open Float in
  let open Item in
  let volume = volume x |> Float.of_int in
  if Float.equal volume 0.0 then 0.0 else volume * money_flow_multiplier x

(* Accumulation distirbution line *)
let adl previous_adl (current : Item.t) =
  let res = money_flow_volume current +. previous_adl in
  (* if Float.is_nan res then *)
  (*   Eio. traceln "%f %f" (money_flow_volume current) previous_adl; *)
  res

(* Exponential moving average *)
(* Length is the number of data points so far *)
(* Previous is the previous EMA value *)
(* Latest is the latest price *)
let ema length previous latest =
  if Item.volume latest = 0 then previous
  else
    let price = Item.last latest in
    let alpha = 2.0 /. (length +. 1.0) in
    previous +. (alpha *. (price -. previous))

module Point = struct
  type t = {
    timestamp : Time.t;
    accumulation_distribution_line : float;
    exponential_moving_average : float;
  }
  [@@deriving show, yojson]

  let initial timestamp : t =
    {
      timestamp;
      accumulation_distribution_line = 0.0;
      exponential_moving_average = 0.0;
    }

  let of_latest timestamp length (previous : t) (latest : Item.t) =
    {
      timestamp;
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

let initialize bars symbol =
  let initial_stats_vector = Vector.create () in
  let bars =
    Bars.get bars symbol |> function
    | Some x -> Vector.to_list x
    | None ->
        invalid_arg "Expected to have bars data when initializing indicators"
  in
  let _ =
    List.fold_left
      (fun previous item ->
        let timestamp = Item.timestamp item in
        match previous with
        | None ->
            let res = Point.initial timestamp in
            Vector.push initial_stats_vector res;
            Option.return res
        | Some previous ->
            let length = Vector.length initial_stats_vector |> Float.of_int in
            let res : Point.t =
              {
                timestamp;
                accumulation_distribution_line =
                  adl previous.accumulation_distribution_line item;
                exponential_moving_average =
                  ema length previous.exponential_moving_average item;
              }
            in
            Vector.push initial_stats_vector res;
            Option.return res)
      None bars
  in
  initial_stats_vector

let add_latest timestamp (original_bars : Bars.t) (latest_bars : Bars.Latest.t)
    (x : t) =
  Hashtbl.to_seq latest_bars |> fun seq ->
  let iter f = Seq.iter f seq in
  iter @@ fun (symbol, latest) ->
  let indicators_vector =
    match get x symbol with
    | Some i -> i
    | None ->
        Eio.traceln "Creating initial indicators for %s." symbol;
        let new_vector = initialize original_bars symbol in
        Hashtbl.replace x symbol new_vector;
        new_vector
  in
  let length = Vector.length indicators_vector |> Float.of_int in
  let previous =
    match Vector.top indicators_vector with
    | Some p -> p
    | None -> Point.initial timestamp
  in
  let new_indicators = Point.of_latest timestamp length previous latest in
  Vector.push indicators_vector new_indicators
