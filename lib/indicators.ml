module Hashtbl = Hashtbl.Make (String)

module Config = struct
  type t = { fft : bool }
end

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

module EMA = struct
  let make n (l : Bars.symbol_history) =
    let window = Util.last_n n l |> Iter.map Item.last in
    let smoothing_factor = 2.0 /. (Float.of_int n +. 1.0) in
    let initial_value = Iter.take 1 window |> Iter.fold ( +. ) 0.0 in
    let f previous_ema current_price =
      previous_ema +. (smoothing_factor *. (current_price -. previous_ema))
    in
    Iter.fold f initial_value window

  let macd ~ema_12 ~ema_26 = ema_12 -. ema_26
end

let simple_moving_average n (l : Bars.symbol_history) =
  let window = Util.last_n n l |> Iter.map Item.last in
  let sum = Iter.fold ( +. ) 0.0 window in
  sum /. Float.of_int n

let upper_bollinger n standard_deviation sma =
  let n = Float.of_int n in
  sma +. (n *. standard_deviation)

let lower_bollinger n standard_deviation sma =
  let n = Float.of_int n in
  sma -. (n *. standard_deviation)

let bollinger n deviations history =
  let sma = simple_moving_average n history in
  let standard_deviation =
    Util.last_n n history
    |> (Iter.map @@ fun item -> Item.last item)
    |> Iter.to_array |> Owl_stats.std
  in
  ( lower_bollinger deviations standard_deviation sma,
    upper_bollinger deviations standard_deviation sma )

let mk_awesome fast slow = fast -. slow

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
    let window =
      Util.last_n n l |> Iter.map Item.last |> Iter.cons current
      |> Iter.to_array
    in
    let min, max = Owl_stats.minmax window in
    if Float.equal max min then 0.0
    else
      let rhs = (current -. min) /. (max -. min) in
      (* Eio.traceln "%f %f %f" min current max; *)
      assert (rhs <=. 1.0);
      100.0 *. rhs

  let pD previous (previous_pK : float Iter.t) =
    Owl_stats.mean @@ Iter.to_array @@ Iter.cons previous previous_pK
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
      { re; im = 0.0 }

    let square x = Complex.mul x x
  end

  type t = (Complex.t, Bigarray.complex64_elt) Genarray.t

  let pp : t Format.printer =
   fun fmt _ -> Format.fprintf fmt "<fourier transform>"

  let empty : t = Dense.Ndarray.Generic.empty Bigarray.complex64 [||]

  let mean_squared_error (config : Config.t) (fft1 : t) (fft2 : t) =
    if not config.fft then 0.0
    else
      let l1, l2 = (Genarray.shape fft1, Genarray.shape fft2) in
      if
        match (Array.get_safe l1 0, Array.get_safe l2 0) with
        | Some i, Some j -> i < 3 || j < 3
        | _ -> true
      then 0.0
      else
        let fft1, fft2 =
          Pair.map_same (Genarray.get_slice [ [ 0; 2 ] ]) (fft1, fft2)
        in
        let max_mse =
          let fft1_mag, fft2_mag =
            Pair.map_same (Genarray.map Complex.norm_complex) (fft1, fft2)
          in
          let fft1_sq, fft2_sq =
            Pair.map_same (Genarray.map Complex.square) (fft1_mag, fft2_mag)
          in
          let added = Genarray.add fft1_sq fft2_sq in
          let sum = Genarray.sum' added in
          sum.re /. 3.0
        in
        let conj = Genarray.conj fft2 in
        let minus = Genarray.sub fft1 conj in
        let magnitudes = Genarray.map Complex.norm_complex minus in
        let squared = Genarray.map Complex.square magnitudes in
        let summed = Genarray.sum' squared in
        let sum = Complex.norm summed in
        let final = sum /. 3.0 /. max_mse *. 10000.0 in
        (* Eio.traceln "%f" final; *)
        final

  (* Fast fourier transform *)
  let fft (config : Config.t) (l : Bars.symbol_history) (last : Item.t) : t =
    if not config.fft then empty
    else
      let arr =
        Vector.map Item.last l |> Vector.to_array |> fun a ->
        Array.append a [| Item.last last |]
      in
      let bigarray = Genarray.of_array Float64 arr [| Array.length arr |] in
      let yf = Owl_fft.D.rfft ~axis:0 bigarray in
      (* Eio.traceln "fft: length : %a" (Array.pp Int.pp) (Genarray.shape yf); *)
      yf

  (* Normalized maginitude of fourier transform *)
  let fft_nm (config : Config.t) (yf : t) (l : Bars.symbol_history) =
    if not config.fft then 0.0
    else
      let length = Vector.length l + 1 |> Float.of_int in
      let mag = Genarray.l2norm yf |> Genarray.sum' |> Complex.norm in
      mag /. length
end

module Point = struct
  type t = {
    timestamp : Time.t;
    price : float;
    accumulation_distribution_line : float;
    ema_12 : float;
    ema_26 : float;
    macd : float;
    sma_5 : float;
    sma_34 : float;
    sma_75 : float;
    sma_233 : float;
    upper_bollinger : float;
    lower_bollinger : float;
    upper_bollinger_100_1 : float;
    lower_bollinger_100_1 : float;
    upper_bollinger_100_3 : float;
    lower_bollinger_100_3 : float;
    awesome_oscillator : float;
    awesome_slow : float;
    average_gain : float;
    average_loss : float;
    relative_strength_index : float;
    fast_stochastic_oscillator_k : float;
    fast_stochastic_oscillator_d : float;
    fourier_transform : (FFT.t[@yojson.opaque]);
    ft_normalized_magnitude : float;
    fft_mean_squared_error : float;
  }
  [@@deriving show, yojson]

  let initial timestamp : t =
    {
      timestamp;
      accumulation_distribution_line = 0.0;
      ema_12 = 0.0;
      ema_26 = 0.0;
      macd = 0.0;
      price = 0.0;
      sma_5 = 0.0;
      sma_34 = 0.0;
      sma_75 = 0.0;
      sma_233 = 0.0;
      upper_bollinger = 0.0;
      lower_bollinger = 0.0;
      upper_bollinger_100_1 = 0.0;
      lower_bollinger_100_1 = 0.0;
      upper_bollinger_100_3 = 0.0;
      lower_bollinger_100_3 = 0.0;
      awesome_oscillator = 0.0;
      awesome_slow = 0.0;
      average_gain = 0.00001;
      average_loss = 0.00001;
      relative_strength_index = 50.0;
      fast_stochastic_oscillator_k = 50.0;
      fast_stochastic_oscillator_d = 50.0;
      fourier_transform = FFT.empty;
      ft_normalized_magnitude = 0.0;
      fft_mean_squared_error = 0.0;
    }

  let ema_12 x = x.ema_12
  let ema_26 x = x.ema_26
  let macd x = x.macd
  let sma_5 x = x.sma_5
  let sma_34 x = x.sma_34
  let sma_75 x = x.sma_75
  let sma_233 x = x.sma_233
  let lower_bollinger x = x.lower_bollinger
  let upper_bollinger x = x.upper_bollinger
  let lower_bollinger_100_1 x = x.lower_bollinger_100_1
  let upper_bollinger_100_1 x = x.upper_bollinger_100_1
  let lower_bollinger_100_3 x = x.lower_bollinger_100_3
  let upper_bollinger_100_3 x = x.upper_bollinger_100_3
  let awesome x = x.awesome_oscillator
  let awesome_slow x = x.awesome_slow
  let rsi x = x.relative_strength_index
  let fso_pk x = x.fast_stochastic_oscillator_k
  let fso_pd x = x.fast_stochastic_oscillator_d
  let ft_normalized_magnitude x = x.ft_normalized_magnitude
  let fft_mean_squared_error x = x.fft_mean_squared_error

  let of_latest config timestamp symbol_history (previous : t)
      (previous_vec : (t, _) Vector.t) (latest : Item.t) =
    let lower_bollinger, upper_bollinger = bollinger 34 2 symbol_history in
    let lower_bollinger_100_3, upper_bollinger_100_3 =
      bollinger 100 3 symbol_history
    in
    let lower_bollinger_100_1, upper_bollinger_100_1 =
      bollinger 100 1 symbol_history
    in
    let sma_5 = simple_moving_average 5 symbol_history in
    let sma_34 = simple_moving_average 34 symbol_history in
    let awesome_oscillator = mk_awesome sma_5 sma_34 in
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
    let fast_stochastic_oscillator_d =
      SO.pD fast_stochastic_oscillator_k
      @@ (Util.last_n 34 previous_vec |> Iter.map fso_pk)
    in
    let fourier_transform = FFT.fft config symbol_history latest in
    let ft_normalized_magnitude =
      FFT.fft_nm config fourier_transform symbol_history
    in
    let fft_mean_squared_error =
      FFT.mean_squared_error config previous.fourier_transform fourier_transform
    in
    let sma_233 = simple_moving_average 233 symbol_history in
    let ema_12 = EMA.make 12 symbol_history in
    let ema_26 = EMA.make 26 symbol_history in
    let res =
      {
        timestamp;
        accumulation_distribution_line =
          adl previous.accumulation_distribution_line latest;
        ema_12;
        ema_26;
        macd = EMA.macd ~ema_12 ~ema_26;
        sma_5;
        sma_34;
        sma_75 = simple_moving_average 75 symbol_history;
        sma_233 = simple_moving_average 233 symbol_history;
        lower_bollinger;
        upper_bollinger;
        upper_bollinger_100_1;
        lower_bollinger_100_1;
        upper_bollinger_100_3;
        lower_bollinger_100_3;
        awesome_oscillator;
        awesome_slow = mk_awesome sma_34 sma_233;
        price;
        average_gain;
        average_loss;
        relative_strength_index;
        fast_stochastic_oscillator_k;
        fast_stochastic_oscillator_d;
        fourier_transform;
        ft_normalized_magnitude;
        fft_mean_squared_error;
      }
    in
    res
end

type t = Point.t Vector.vector Hashtbl.t

let pp : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp String.pp (Vector.pp Point.pp) in
  Format.fprintf fmt "@[%a@]@." pp seq

let empty () = Hashtbl.create 100
let get (x : t) symbol = Hashtbl.find_opt x symbol

let indicator (x : t) symbol f =
  let open Option.Infix in
  let* ind = get x symbol in
  let+ top = Vector.top ind in
  f top

let initialize config bars symbol =
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
            let bars_vec_upto_now =
              Vector.slice_iter bars_vec 0 i |> Vector.of_iter
            in
            let res =
              Point.of_latest config timestamp bars_vec_upto_now previous
                initial_stats_vector item
            in
            Vector.push initial_stats_vector res;
            Option.return res)
      None bars
  in
  initial_stats_vector

let add_latest config timestamp (bars : Bars.t) (latest_bars : Bars.Latest.t)
    (x : t) =
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
        (* Eio.traceln "Creating initial indicators for %s." symbol; *)
        let new_vector = initialize config bars symbol in
        Hashtbl.replace x symbol new_vector;
        new_vector
  in
  let previous =
    match Vector.top indicators_vector with
    | Some p -> p
    | None -> Point.initial timestamp
  in
  let new_indicators =
    Point.of_latest config timestamp symbol_history previous indicators_vector
      latest
  in
  Vector.push indicators_vector new_indicators
