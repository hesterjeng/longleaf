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

let simple_moving_average n (l : Bars.symbol_history) =
  let length = Vector.length l in
  let close = Vector.map Item.close l in
  let start = Int.max (length - n) 0 in
  let n = Int.min n length in
  (* Eio.traceln "@[sma: %d %d@]" n (Vector.size close); *)
  let window = Vector.slice_iter close start n in
  let sum = Iter.fold ( +. ) 0.0 window in
  sum /. Float.of_int n

let upper_bollinger standard_deviation sma = sma +. (2.0 *. standard_deviation)
let lower_bollinger standard_deviation sma = sma -. (2.0 *. standard_deviation)

let bollinger n history =
  let length = Vector.length history in
  let sma = simple_moving_average n history in
  let standard_deviation =
    Vector.slice_iter history (Int.max (length - n) 0) (Int.min n length)
    |> (Iter.map @@ fun item -> Item.last item)
    |> Iter.to_array |> Owl_stats.std
  in
  ( lower_bollinger standard_deviation sma,
    upper_bollinger standard_deviation sma )

module Point = struct
  type t = {
    timestamp : Time.t;
    accumulation_distribution_line : float;
    exponential_moving_average : float;
    sma_5 : float;
    sma_34 : float;
  }
  [@@deriving show, yojson]

  let initial timestamp : t =
    {
      timestamp;
      accumulation_distribution_line = 0.0;
      exponential_moving_average = 0.0;
      sma_5 = 0.0;
      sma_34 = 0.0;
    }

  let of_latest timestamp symbol_history length (previous : t) (latest : Item.t)
      =
    {
      timestamp;
      accumulation_distribution_line =
        adl previous.accumulation_distribution_line latest;
      exponential_moving_average =
        ema length previous.exponential_moving_average latest;
      sma_5 = simple_moving_average 5 symbol_history;
      sma_34 = simple_moving_average 34 symbol_history;
    }

  let ema x = x.exponential_moving_average
  let sma_5 x = x.sma_5
  let sma_34 x = x.sma_34
end

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
  let bars_vec =
    Bars.get bars symbol |> function
    | Some x -> x
    | None ->
        invalid_arg "Expected to have bars data when initializing indicators"
  in
  let bars = Vector.to_list bars_vec in
  let _ =
    List.foldi
      (fun previous i item ->
        let timestamp = Item.timestamp item in
        match previous with
        | None ->
            let res = Point.initial timestamp in
            Vector.push initial_stats_vector res;
            Option.return res
        | Some previous ->
            let length = Vector.length initial_stats_vector |> Float.of_int in
            let bars_vec_upto_now =
              Vector.slice_iter bars_vec 0 i |> Vector.of_iter
            in
            let res : Point.t =
              {
                timestamp;
                accumulation_distribution_line =
                  adl previous.accumulation_distribution_line item;
                exponential_moving_average =
                  ema length previous.exponential_moving_average item;
                sma_5 = simple_moving_average 5 bars_vec_upto_now;
                sma_34 = simple_moving_average 34 bars_vec_upto_now;
              }
            in
            Vector.push initial_stats_vector res;
            Option.return res)
      None bars
  in
  initial_stats_vector

let add_latest timestamp (bars : Bars.t) (latest_bars : Bars.Latest.t) (x : t) =
  Hashtbl.to_seq latest_bars |> fun seq ->
  let iter f = Seq.iter f seq in
  iter @@ fun (symbol, latest) ->
  let symbol_history =
    Bars.get bars symbol |> function
    | Some x -> x
    | None ->
        Eio.traceln "No bars for %s when making indicators?" symbol;
        Vector.create ()
  in
  let indicators_vector =
    match get x symbol with
    | Some i -> i
    | None ->
        Eio.traceln "Creating initial indicators for %s." symbol;
        let new_vector = initialize bars symbol in
        Hashtbl.replace x symbol new_vector;
        new_vector
  in
  let length = Vector.length indicators_vector |> Float.of_int in
  let previous =
    match Vector.top indicators_vector with
    | Some p -> p
    | None -> Point.initial timestamp
  in
  let new_indicators =
    Point.of_latest timestamp symbol_history length previous latest
  in
  Vector.push indicators_vector new_indicators
