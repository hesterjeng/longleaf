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
  let last = Vector.map Item.last l in
  let start = Int.max (length - n) 0 in
  let n = Int.min n length in
  (* Eio.traceln "@[sma: %d %d@]" n (Vector.size close); *)
  let window = Vector.slice_iter last start n in
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

let awesome fast slow = fast -. slow

module RSI = struct
  (* Relative strength index calculation *)

  let u now previous =
    let upward_change = now -. previous in
    if upward_change >=. 0.0 then upward_change else 0.0

  let d now previous =
    let downward_change = previous -. now in
    if downward_change >=. 0.0 then downward_change else 0.0

  let mau period previous_mau now previous =
    let u = u now previous in
    u +. (previous_mau *. ((period -. 1.0) /. period))

  let mad period previous_mad now previous =
    let d = d now previous in
    d +. (previous_mad *. ((period -. 1.0) /. period))

  let rsi mau mad = 100.0 -. (100.0 *. (1.0 /. (1.0 +. (mau /. mad))))
end

module SO = struct
  (* Stochastic Oscillators *)

  let pK n (l : Bars.symbol_history) (last : Item.t) =
    let current = Item.last last in
    let length = Vector.length l in
    let window =
      Vector.slice_iter l (Int.max (length - n) 0) (Int.min n length)
      |> Iter.map Item.last |> Iter.to_array
      |> fun a -> Array.append a [| current |]
    in
    let min, max = Owl_stats.minmax window in
    if Float.equal max min then 0.0
    else
      let rhs = (current -. min) /. (max -. min) in
      (* Eio.traceln "%f %f %f" min current max; *)
      assert (rhs <=. 1.0);
      100.0 *. rhs
end

module FFT = struct
  open Owl
  module Genarray = Dense.Ndarray.Generic

  module Complex = struct
    include Complex

    let pp : t Format.printer =
     fun fmt x -> Format.fprintf fmt "(%f, %f)" x.re x.im

    let norm_complex x : t =
      let re = norm x in
      {
        re;
        im = 0.0;
      }

    let square x = Complex.mul x x
  end

  type t = (Complex.t, Bigarray.complex64_elt) Genarray.t

  let pp : t Format.printer =
   fun fmt _ -> Format.fprintf fmt "<fourier transform>"

  let empty : t = Dense.Ndarray.Generic.empty Bigarray.complex64 [||]

  let mean_squared_error (fft1 : t) (fft2 : t) =
    let l1, l2 = (Genarray.shape fft1, Genarray.shape fft2) in
    if
      match Array.get_safe l1 0, Array.get_safe l2 0 with
      | Some i, Some j ->
        i < 3 || j < 3
      | _ -> true
    then 0.0
    else (
      let fft1, fft2 =
        Pair.map_same
        (Genarray.get_slice [ [0; 2] ]) (fft1, fft2) in
      let conj = Genarray.conj fft2 in
      let minus = Genarray.sub fft1 conj in
      let magnitudes = Genarray.map Complex.norm_complex minus in
      let squared = Genarray.map Complex.square magnitudes in
      let sum = Genarray.sum' squared in
      let res = Complex.norm sum in
      Eio.traceln "%f" res;
      res /. 3.0
    )

  (* Fast fourier transform *)
  let fft (l : Bars.symbol_history) (last : Item.t) : t =
    let arr =
      Vector.map Item.last l |> Vector.to_array |> fun a ->
      Array.append a [| Item.last last |]
    in
    let bigarray = Genarray.of_array Float64 arr [| Array.length arr |] in
    let yf = Owl_fft.D.rfft ~axis:0 bigarray in
    Eio.traceln "fft: length : %a" (Array.pp Int.pp) (Genarray.shape yf);
    yf

  (* Normalized maginitude of fourier transform *)
  let fft_nm (yf : t) (l : Bars.symbol_history) =
    let length = Vector.length l + 1 |> Float.of_int in
    let mag = Genarray.l2norm yf |> Genarray.sum' |> Complex.norm in
    mag /. length
end

module Point = struct
  type t = {
    timestamp : Time.t;
    price : float;
    accumulation_distribution_line : float;
    exponential_moving_average : float;
    sma_5 : float;
    sma_34 : float;
    upper_bollinger : float;
    lower_bollinger : float;
    awesome_oscillator : float;
    average_gain : float;
    average_loss : float;
    relative_strength_index : float;
    fast_stochastic_oscillator_k : float;
    fourier_transform : (FFT.t[@yojson.opaque]);
    ft_normalized_magnitude : float;
  }
  [@@deriving show, yojson]

  let initial timestamp : t =
    {
      timestamp;
      accumulation_distribution_line = 0.0;
      exponential_moving_average = 0.0;
      price = 0.0;
      sma_5 = 0.0;
      sma_34 = 0.0;
      upper_bollinger = 0.0;
      lower_bollinger = 0.0;
      awesome_oscillator = 0.0;
      average_gain = 0.00001;
      average_loss = 0.00001;
      relative_strength_index = 50.0;
      fast_stochastic_oscillator_k = 50.0;
      fourier_transform = FFT.empty;
      ft_normalized_magnitude = 0.0;
    }

  let of_latest timestamp symbol_history length (previous : t) (latest : Item.t)
      =
    let lower_bollinger, upper_bollinger = bollinger 34 symbol_history in
    let sma_5 = simple_moving_average 5 symbol_history in
    let sma_34 = simple_moving_average 34 symbol_history in
    let awesome_oscillator = awesome sma_5 sma_34 in
    let price = Item.last latest in
    let previous_price = previous.price in
    let average_gain =
      RSI.mau 14.0 previous.average_gain price previous_price
    in
    let average_loss =
      RSI.mad 14.0 previous.average_loss price previous_price
    in
    let relative_strength_index = RSI.rsi average_gain average_loss in
    let fast_stochastic_oscillator_k = SO.pK 140 symbol_history latest in
    let fourier_transform = FFT.fft symbol_history latest in
    let ft_normalized_magnitude = FFT.fft_nm fourier_transform symbol_history in
    let fft_mse =
      FFT.mean_squared_error previous.fourier_transform fourier_transform
    in
    let res =
      {
        timestamp;
        accumulation_distribution_line =
          adl previous.accumulation_distribution_line latest;
        exponential_moving_average =
          ema length previous.exponential_moving_average latest;
        sma_5;
        sma_34;
        lower_bollinger;
        upper_bollinger;
        awesome_oscillator;
        price;
        average_gain;
        average_loss;
        relative_strength_index;
        fast_stochastic_oscillator_k;
        fourier_transform;
        ft_normalized_magnitude;
      }
    in
    (* if Float.equal previous.sma_5 res.sma_5 then ( *)
    (*   (\* Eio.traceln "%a" (Vector.pp Item.pp) symbol_history; *\) *)
    (*   Eio.traceln "error: identical sma5: %a %f %f" Time.pp timestamp *)
    (*     previous.sma_5 res.sma_5; *)
    (*   invalid_arg "try"); *)
    res

  let ema x = x.exponential_moving_average
  let sma_5 x = x.sma_5
  let sma_34 x = x.sma_34
  let lower_bollinger x = x.lower_bollinger
  let upper_bollinger x = x.upper_bollinger
  let awesome x = x.awesome_oscillator
  let rsi x = x.relative_strength_index
  let fso_pk x = x.fast_stochastic_oscillator_k
  let ft_normalized_magnitude x = x.ft_normalized_magnitude
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
            let res =
              Point.of_latest timestamp bars_vec_upto_now length previous item
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
