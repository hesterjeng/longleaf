module Hashtbl = Hashtbl.Make (Instrument)

module Point_ty = struct
  type adx = {
    positive_directional_movement : float;
    negative_directional_movement : float;
    sma_positive_dm : float;
    sma_negative_dm : float;
    positive_directional_indicator : float;
    negative_directional_indicator : float;
    adx : float;
  }
  [@@deriving show, yojson, fields ~getters]

  type t = {
    timestamp : Time.t;
    item : Item.t;
    price : float;
    volume : int;
    accumulation_distribution_line : float;
    ema_12 : float;
    ema_26 : float;
    macd : float;
    sma_5 : float;
    sma_34 : float;
    sma_75 : float;
    sma_233 : float;
    (* positive_directional_movement : float; *)
    (* negative_directional_movement : float; *)
    (* sma_positive_dm : float; *)
    (* sma_negative_dm : float; *)
    adx : adx;
    average_true_range : float;
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
    fourier_transform : (Fourier.t[@yojson.opaque]);
    ft_normalized_magnitude : float;
    fft_mean_squared_error : float;
    previous : t option;
  }
  [@@deriving show, yojson, fields ~getters]
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
  let make n (l : Price_history.t) =
    let window = Util.last_n n l |> Iter.map Item.last in
    let smoothing_factor = 2.0 /. (Float.of_int n +. 1.0) in
    let initial_value = Iter.take 1 window |> Iter.fold ( +. ) 0.0 in
    let f previous_ema current_price =
      previous_ema +. (smoothing_factor *. (current_price -. previous_ema))
    in
    Iter.fold f initial_value window

  let macd ~ema_12 ~ema_26 = ema_12 -. ema_26
end

let simple_moving_average n (l : Price_history.t) =
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

module ATR = struct
  (* Average true range *)

  let true_range (previous : Point_ty.t) (current : Item.t) =
    let previous = previous.item in
    let high = Item.high current in
    let low = Item.low current in
    let close_prev = Item.last previous in
    Float.max high close_prev -. Float.min low close_prev

  let average_true_range n (previous : Point_ty.t option) (current : Item.t) =
    assert (not @@ Float.equal n 0.0);
    match previous with
    | Some prev ->
        ((prev.average_true_range *. (n -. 1.0)) +. true_range prev current)
        /. n
    | None -> Item.high current -. Item.low current
end

module ADX = struct
  (* Average directional movement index *)
  (* let top (history : Price_history.t) (current : Item.t) = *)
  (*   let last_two_days = *)
  (*     Util.last_n 78 history |> Iter.to_list |> List.sort Item.compare *)
  (*   in *)
  (*   let yesterday, today = *)
  (*     List.take_drop 39 last_two_days *)
  (*     |> Pair.map_same Array.of_list *)
  (*     |> Pair.map_same (Array.map Item.last) *)
  (*   in *)
  (*   let yesterday_low, yesterday_high = Owl_stats.minmax yesterday in *)
  (*   let today_low, today_high = Owl_stats.minmax yesterday in *)
  (*   let upmove = today_high -. yesterday_high in *)
  (*   let downmove = today_low -. yesterday_low in *)
  (*   let dm_plus = if upmove >. downmove && upmove >. 0.0 then upmove else 0.0 in *)
  (*   let dm_minus = *)
  (*     if downmove >. upmove && downmove >. 0.0 then downmove else 0.0 *)
  (*   in *)
  (*   () *)

  type t = Point_ty.adx

  let positive_directional_movement (previous : Point_ty.t) (current : Item.t) =
    let now_high = Item.high current in
    let now_low = Item.low current in
    let previous_high = Item.high previous.item in
    let previous_low = Item.low previous.item in
    let upmove = now_high -. previous_high in
    let downmove = now_low -. previous_low in
    if upmove >. downmove && upmove >. 0.0 then upmove else 0.0

  let negative_directional_movement (previous : Point_ty.t) (current : Item.t) =
    let now_high = Item.high current in
    let now_low = Item.low current in
    let previous_high = Item.high previous.item in
    let previous_low = Item.low previous.item in
    let upmove = now_high -. previous_high in
    let downmove = now_low -. previous_low in
    if downmove >. upmove && downmove >. 0.0 then downmove else 0.0

  (* let sma_pdm (previous : Point_ty.t) (current : Item.t) = *)

  (* let top (previous : Point_ty.t) (current : Item.t) = *)
end

module SO = struct
  (* Stochastic Oscillators *)

  let pK n (l : Price_history.t) (last : Item.t) =
    let current = Item.last last in
    let window =
      Util.last_n n l |> Iter.map Item.last |> Iter.cons current
      |> Iter.to_array
    in
    let min, max = Owl_stats.minmax window in
    if Float.equal max min then 50.0
    else
      let rhs = (current -. min) /. (max -. min) in
      (* Eio.traceln "%f %f %f" min current max; *)
      assert (rhs <=. 1.0);
      100.0 *. rhs

  let pD previous (previous_pK : float Iter.t) =
    Owl_stats.mean @@ Iter.to_array @@ Iter.cons previous previous_pK
end

(* module Fourier = Fourier *)

module Point = struct
  include Point_ty

  let of_latest config timestamp symbol_history (previous : t option)
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
    let previous_price, previous_average_gain, previous_average_loss =
      match previous with
      | None -> (price, 0.0, 0.0)
      | Some prev -> (prev.price, prev.average_gain, prev.average_loss)
    in
    let average_gain =
      RSI.mau 14.0 previous_average_gain price previous_price
    in
    let average_loss =
      RSI.mad 14.0 previous_average_loss price previous_price
    in
    let relative_strength_index = RSI.rsi average_gain average_loss in
    let fso_pk = SO.pK 140 symbol_history latest in
    let fast_stochastic_oscillator_d =
      SO.pD fso_pk
      @@ (Util.last_n 34 previous_vec |> Iter.map fast_stochastic_oscillator_k)
    in
    let fourier_transform = Fourier.fft config symbol_history latest in
    let ft_normalized_magnitude =
      Fourier.fft_nm config fourier_transform symbol_history
    in
    let fft_mean_squared_error =
      match previous with
      | None -> 0.0
      | Some prev ->
          Fourier.mean_squared_error config prev.fourier_transform
            fourier_transform
    in
    let previous_adl =
      match previous with
      | None -> 0.0
      | Some prev -> prev.accumulation_distribution_line
    in
    let sma_233 = simple_moving_average 233 symbol_history in
    let ema_12 = EMA.make 12 symbol_history in
    let ema_26 = EMA.make 26 symbol_history in
    let average_true_range = ATR.average_true_range 14.0 previous latest in
    let res : t =
      {
        timestamp;
        item = latest;
        average_true_range;
        accumulation_distribution_line = adl previous_adl latest;
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
        volume = Item.volume latest;
        average_gain;
        average_loss;
        relative_strength_index;
        fast_stochastic_oscillator_k = fso_pk;
        fast_stochastic_oscillator_d;
        fourier_transform;
        ft_normalized_magnitude;
        fft_mean_squared_error;
        previous;
      }
    in
    res
end

type t = Point.t Vector.vector Hashtbl.t

let pp : t Format.printer =
 fun fmt x ->
  let seq = Hashtbl.to_seq x in
  let pp = Seq.pp @@ Pair.pp Instrument.pp (Vector.pp Point.pp) in
  Format.fprintf fmt "@[%a@]@." pp seq

let empty () = Hashtbl.create 100
let get (x : t) symbol = Hashtbl.find_opt x symbol
let get_instrument (x : t) instrument = get x instrument

let get_top (x : t) symbol =
  match get x symbol with
  | None ->
      Error.missing_data
      @@ Format.asprintf "Missing indicators vector for %a" Instrument.pp symbol
  | Some vec -> (
      match Vector.top vec with
      | Some top -> Ok top
      | None ->
          Error.missing_data
          @@ Format.asprintf "Indicators vector for %a is empty" Instrument.pp
               symbol)

let get_indicator (x : t) symbol f =
  let res =
    let open Option.Infix in
    let* ind = get_instrument x symbol in
    let+ top = Vector.top ind in
    f top
  in
  Option.get_exn_or
    (Format.asprintf "indicators.ml: Unable to get indicator for symbol %a"
       Instrument.pp symbol)
    res

let initialize_single config bars symbol =
  let initial_stats_vector = Vector.create () in
  let bars_vec =
    Bars.get bars symbol |> function
    | Some x -> x
    | None ->
        invalid_arg "Expected to have bars data when initializing indicators"
  in
  (* Create a vector to store the data so far *)
  let bars_upto_now = Vector.create () in
  (* Function to generate the indicators from the bars *)
  let fold i previous item =
    let timestamp = Item.timestamp item in
    let res =
      Point.of_latest config timestamp bars_upto_now previous
        initial_stats_vector item
    in
    Vector.push initial_stats_vector res;
    Vector.push bars_upto_now (Vector.get bars_vec i);
    Option.return res
  in
  let _ =
    (* Fold, so that we have access to the previous indicator record *)
    Vector.foldi fold None bars_vec
  in
  initial_stats_vector

let add_latest config timestamp (bars : Bars.t) (latest_bars : Bars.Latest.t)
    (x : t) =
  let seq = Hashtbl.to_seq latest_bars in
  let iter f = Seq.iter f seq in
  iter @@ fun (symbol, latest) ->
  let symbol_history =
    Bars.get bars symbol |> function Some x -> x | None -> assert false
  in
  let indicators_vector =
    match get x symbol with
    | Some i -> i
    | None ->
        let new_vector = initialize_single config bars symbol in
        Hashtbl.replace x symbol new_vector;
        new_vector
  in
  let previous = Vector.top indicators_vector in
  let new_indicators =
    Point.of_latest config timestamp symbol_history previous indicators_vector
      latest
  in
  Vector.push indicators_vector new_indicators
