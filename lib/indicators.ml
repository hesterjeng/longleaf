module Point_ty = struct
  type cci = { cci : float; typical_price : float; ema_cci : float }
  [@@deriving show, yojson, fields ~getters, ord]

  type fso = { k : float; d : float; d_slow : float }
  [@@deriving show, yojson, fields ~getters, ord]

  (* Type for average divergance calculation *)
  type adx = {
    ema_positive_dm : float;
    ema_negative_dm : float;
    ema_di_diff : float;
    adx : float;
    ema_adx : float;
  }
  [@@deriving show, yojson, fields ~getters, ord]

  type t = {
    symbol : Instrument.t;
    timestamp : Time.t; [@compare Ptime.compare]
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
    (* fast_stochastic_oscillator_k : float; *)
    (* fast_stochastic_oscillator_d : float; *)
    (* fast_stochastic_oscillator_d68 : float; *)
    fourier_transform : (Fourier.t[@yojson.opaque]);
        [@opaque] [@compare fun _ _ -> 0]
    ft_normalized_magnitude : float;
    fft_mean_squared_error : float;
    adx : adx;
    cci : cci;
    fso : fso;
    previous : t option; [@yojson.opaque] [@opaque] [@compare fun _ _ -> 0]
  }
  [@@deriving show, yojson, fields ~getters, ord]

  let last_n n (x : t) =
    let rec aux (n : int) x acc =
      match n with
      | n when n > 0 -> (
        let acc = x :: acc in
        match x.previous with
        | Some next -> aux (n - 1) next acc
        | None -> acc)
      | _ -> acc
    in
    aux n x []

  let adx x = x.adx.adx
  let cci x = x.cci.cci
  let ema_cci x = x.cci.ema_cci
  let ema_adx x = x.adx.ema_adx
  let fso_k x = x.fso.k
  let fso_d x = x.fso.d
  let fso_d_slow x = x.fso.d_slow
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

  let rsi mau mad =
    assert (not @@ Float.is_nan mad);
    assert (not @@ Float.is_nan mau);
    let ratio =
      match mad with
      | 0.0 -> 0.0
      | _ -> mau /. mad
    in
    100.0 -. (100.0 *. (1.0 /. (1.0 +. ratio)))
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
    match previous with
    | Some prev -> Math.ema n prev.average_true_range @@ true_range prev current
    | None -> Item.high current -. Item.low current
end

module ADX = struct
  (* type adx = { *)
  (*   positive_directional_movement : float; *)
  (*   negative_directional_movement : float; *)
  (*   sma_positive_dm : float; *)
  (*   sma_negative_dm : float; *)
  (*   positive_directional_indicator : float; *)
  (*   negative_directional_indicator : float; *)
  (*   adx : float; *)
  (* } *)
  type t = Point_ty.adx

  let positive_directional_movement (previous : Point_ty.t) (current : Item.t) =
    let now_high = Item.high current in
    let now_low = Item.low current in
    let previous_high = Item.high previous.item in
    let previous_low = Item.low previous.item in
    let upmove = now_high -. previous_high in
    let downmove = previous_low -. now_low in
    if upmove >. downmove && upmove >. 0.0 then upmove else 0.0

  let negative_directional_movement (previous : Point_ty.t) (current : Item.t) =
    let now_high = Item.high current in
    let now_low = Item.low current in
    let previous_high = Item.high previous.item in
    let previous_low = Item.low previous.item in
    let upmove = now_high -. previous_high in
    let downmove = previous_low -. now_low in
    if downmove >. upmove && downmove >. 0.0 then downmove else 0.0

  let ema_pdm n (previous : Point_ty.t) (current : Item.t) =
    Math.ema n previous.adx.ema_positive_dm
    @@ positive_directional_movement previous current

  let ema_ndm n (previous : Point_ty.t) (current : Item.t) =
    Math.ema n previous.adx.ema_negative_dm
    @@ negative_directional_movement previous current

  let positive_directional_indicator n ~ema_pdm (previous : Point_ty.t)
      (current : Item.t) =
    let atr = ATR.average_true_range n (Some previous) current in
    assert (not @@ Float.equal atr 0.0);
    100.0 *. ema_pdm /. atr

  let negative_directional_indicator n ~ema_ndm (previous : Point_ty.t)
      (current : Item.t) =
    let atr = ATR.average_true_range n (Some previous) current in
    assert (not @@ Float.equal atr 0.0);
    100.0 *. ema_ndm /. atr

  let adx ~pdi ~ndi ~ema_di_diff =
    match pdi +. ndi with
    | 0.0 -> 0.0
    | sum when ema_di_diff >. sum -> 100.0
    | sum -> 100.0 *. (ema_di_diff /. sum)

  let top previous current : t =
    match previous with
    | None ->
      {
        ema_positive_dm = 0.0;
        ema_negative_dm = 0.0;
        ema_di_diff = 0.0;
        adx = 0.0;
        ema_adx = 0.0;
      }
    | Some previous ->
      let ema_positive_dm = ema_pdm 14 previous current in
      let ema_negative_dm = ema_ndm 14 previous current in
      let pdi =
        positive_directional_indicator ~ema_pdm:ema_positive_dm 14 previous
          current
      in
      let ndi =
        negative_directional_indicator ~ema_ndm:ema_negative_dm 14 previous
          current
      in
      let ema_di_diff =
        let abs_diff = Float.abs (pdi -. ndi) in
        assert (abs_diff <=. pdi +. ndi);
        Math.ema 14 previous.adx.ema_di_diff @@ Float.abs (pdi -. ndi)
      in
      let adx = adx ~pdi ~ndi ~ema_di_diff in
      let ema_adx = Math.ema 140 previous.adx.ema_adx adx in
      let res : t =
        { ema_positive_dm; ema_negative_dm; ema_di_diff; adx; ema_adx }
      in
      (* Eio.traceln "%a" Point_ty.pp_adx res; *)
      res
end

module CCI = struct
  (* Commodity Channel Index *)

  type t = Point_ty.cci

  let typical_price (item : Item.t) =
    (Item.high item +. Item.low item +. Item.last item) /. 3.0

  let top (previous : Point_ty.t option) (item : Item.t) : t =
    let typical_price = typical_price item in
    match previous with
    | None -> { typical_price; cci = 0.0; ema_cci = 0.0 }
    | Some prev ->
      let sma_pt =
        Math.simple_moving_average ~current:typical_price 140
          (fun (x : Point_ty.t) -> x.cci.typical_price)
          Point_ty.previous prev
      in
      let current_divergence = Float.abs @@ (typical_price -. sma_pt) in
      let mean_absolute_divergence =
        Math.simple_moving_average ~current:current_divergence 140
          (fun (x : Point_ty.t) -> Float.abs @@ (x.cci.typical_price -. sma_pt))
          Point_ty.previous prev
      in
      let cci =
        match mean_absolute_divergence with
        | 0.0 -> prev.cci.cci
        | _ -> (
          1.0 /. 0.015 *. ((typical_price -. sma_pt) /. mean_absolute_divergence)
          |> function
          | x when x >=. 100.0 -> 100.0
          | x when x <=. -100.0 -> -100.0
          | x -> x)
      in
      let ema_cci = Math.ema 140 prev.cci.ema_cci cci in
      { typical_price; cci; ema_cci }
end

module FSO = struct
  (* Stochastic Oscillators *)

  type t = Point_ty.fso

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

  let top price_history (previous : Point_ty.t option) (item : Item.t) : t =
    match previous with
    | None -> { k = 0.0; d = 0.0; d_slow = 0.0 }
    | Some prev ->
      let k = pK 140 price_history item in
      let d =
        Math.simple_moving_average ~current:k 35
          (fun (x : Point_ty.t) -> x.fso.k)
          Point_ty.previous prev
      in
      let d_slow =
        Math.simple_moving_average ~current:d 34
          (fun (x : Point_ty.t) -> x.fso.d)
          Point_ty.previous prev
      in
      { k; d; d_slow }
end

(* module Fourier = Fourier *)

module Point = struct
  include Point_ty

  let of_latest config timestamp symbol_history (previous : t option)
      (latest : Item.t) symbol =
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
      | Some prev ->
        assert (Instrument.equal symbol prev.symbol);
        (prev.price, prev.average_gain, prev.average_loss)
    in
    let average_gain =
      RSI.mau 14.0 previous_average_gain price previous_price
    in
    let average_loss =
      RSI.mad 14.0 previous_average_loss price previous_price
    in
    let relative_strength_index = RSI.rsi average_gain average_loss in
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
    let average_true_range = ATR.average_true_range 14 previous latest in
    let adx = ADX.top previous latest in
    let cci = CCI.top previous latest in
    let fso = FSO.top symbol_history previous latest in
    let res : t =
      {
        symbol;
        fso;
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
        fourier_transform;
        ft_normalized_magnitude;
        fft_mean_squared_error;
        previous;
        adx;
        cci;
      }
    in
    res
end

module TimestampedTbl = struct
  include CCHashtbl.Make (Instrument.Timestamped)

  type tbl = Point.t t
  type t = tbl

  let get_key tbl = get tbl
  let create () = create 200

  let get (tbl : t) instrument (time : Time.t) =
    let key = Instrument.Timestamped.{ instrument; time } in
    match find_opt tbl key with
    | None ->
      Error.missing_data
      @@ Format.asprintf
           "Missing data from precomputed indicator table for @[%a@]@."
           Instrument.Timestamped.pp key
    | Some x ->
      (* Eio.traceln "@[TimestampedTbl.get: Got @[%a@]@. from %a@]@." Point.pp x *)
      (*   Instrument.Timestamped.pp key; *)
      assert (Time.equal time x.timestamp);
      Result.return x

  let set (tbl : t) instrument time point =
    let key = Instrument.Timestamped.{ instrument; time } in
    match get_key tbl key with
    | None -> replace tbl key point
    | Some res -> (
      let cmp = Point.compare res point in
      match cmp with
      | 0 -> ()
      | _ ->
        Eio.traceln "%a %a" Point.pp res Point.pp point;
        invalid_arg "Attempt to rebind in indicators.ml")
end

type ty = Live | Precomputed
type t = { ty : ty; time : Time.t; tbl : TimestampedTbl.t }

(* FIXME:  This doesn't work I don't think *)
let to_vector_table (x : t) =
  Eio.traceln "BUGGED: Indicators.of_timestampedtbl";
  let keys = TimestampedTbl.keys_list x.tbl in
  let vector_tbl = Hashtbl.create 100 in
  let symbols =
    List.map (fun (k : Instrument.Timestamped.t) -> k.instrument) keys
  in
  let vector_of_symbol symbol =
    let keys =
      List.filter
        (fun (k : Instrument.Timestamped.t) ->
          Instrument.equal symbol k.instrument)
        keys
    in
    let results =
      List.map (fun k -> TimestampedTbl.get_key x.tbl k) keys
      |> List.filter_map Fun.id |> Vector.of_list
    in
    Vector.sort'
      (fun (x : Point.t) y -> Ptime.compare x.timestamp y.timestamp)
      results;
    Hashtbl.replace vector_tbl symbol results;
    ()
  in
  List.iter vector_of_symbol symbols;
  vector_tbl

let pp : t Format.printer =
 fun fmt x -> Format.fprintf fmt "new Indicator.t printer NYI"
(* match x with *)
(* | Live x -> *)
(*   Format.fprintf fmt "custom indicator printer %a" Time.pp x *)
(*   (\* let seq = Hashtbl.to_seq x in *\) *)
(*   (\* let pp = Seq.pp @@ Pair.pp Instrument.pp (Vector.pp Point.pp) in *\) *)
(*   (\* Format.fprintf fmt "@[%a@]@." pp seq *\) *)
(* | Precomputed _ -> invalid_arg "Printing precomputed indicators NYI" *)

(* let empty (x : Options.IndicatorType.t) = *)
(*   match x with *)
(*   | Live -> Live (Hashtbl.create 100) *)
(*   | Precomputed -> Precomputed *)

let empty ty = { ty; time = Ptime.min; tbl = TimestampedTbl.create () }
(* Live Ptime.min *)
(* let get (x : vectortbl) symbol = Hashtbl.find_opt x symbol *)

let get_top (x : t) symbol = TimestampedTbl.get x.tbl symbol x.time
(* let* res = *)
(*   match x with *)
(*   | Precomputed tbl -> *)
(*     let res = TimestampedTbl.get tbl symbol time in *)
(*     res *)
(*   | Live time -> *)
(*     let res = TimestampedTbl.get symbol (Some time) in *)
(*     res *)
(* in *)
(* (\* Eio.traceln "indicators.get_top:@[%a@]@." Point.pp res; *\) *)
(* Result.return res *)

(* let initialize_single ?(precompute = false) config bars symbol = *)
(*   let initial_stats_vector = Vector.create () in *)
(*   let bars_vec = *)
(*     Bars.get bars symbol |> function *)
(*     | Some x -> x *)
(*     | None -> *)
(*       invalid_arg "Expected to have bars data when initializing indicators" *)
(*   in *)
(*   (\* Create a vector to store the data so far *\) *)
(*   let bars_upto_now = Vector.create () in *)
(*   (\* Function to generate the indicators from the bars *\) *)
(*   let fold i previous item = *)
(*     let timestamp = Item.timestamp item in *)
(*     let res = *)
(*       Point.of_latest config timestamp bars_upto_now previous item symbol *)
(*     in *)
(*     if precompute then TimestampedTbl.set symbol timestamp res; *)
(*     Vector.push initial_stats_vector res; *)
(*     Vector.push bars_upto_now (Vector.get bars_vec i); *)
(*     Option.return res *)
(*   in *)
(*   let _ = *)
(*     (\* Fold, so that we have access to the previous indicator record *\) *)
(*     Vector.foldi fold None bars_vec *)
(*   in *)
(*   initial_stats_vector *)

(* let precompute (preload : Bars.t) (target : Bars.t) = *)
(*   let config = { Indicator_config.fft = false; compare_preloaded = false } in *)
(*   let symbols = Bars.keys preload in *)
(*   assert (not @@ List.is_empty symbols); *)
(*   let _ = *)
(*     List.map (initialize_single ~precompute:true config preload) symbols *)
(*   in *)
(*   let _ = List.map (initialize_single ~precompute:true config target) symbols in *)
(*   () *)

let compute_i indicators config bars i =
  let ( let* ) = Result.( let* ) in
  let* res =
    Bars.fold bars (Ok ()) @@ fun instrument price_history prev ->
    let* _prev = prev in
    let* current_item =
      try Result.return @@ Vector.get price_history i with
      | _ -> Error.missing_data "Missing index when computing price history"
    in
    let timestamp = Item.timestamp current_item in
    let previous_indicator =
      try
        let previous_item = Vector.get price_history (i - 1) in
        let previous_item_timestamp = Item.timestamp previous_item in
        assert (not @@ Time.equal timestamp previous_item_timestamp);
        let res =
          TimestampedTbl.get indicators instrument previous_item_timestamp
        in
        match res with
        | Ok x -> Some x
        | Error _ ->
          Eio.traceln
            "Indicators.compute_i: couldn't find indicators for previous";
          None
      with
      | _ -> None
    in
    let new_point =
      Point.of_latest config timestamp price_history previous_indicator
        current_item instrument
    in
    TimestampedTbl.set indicators instrument timestamp new_point;
    Result.return ()
  in
  Result.return res

let compute_latest config bars x =
  match x.ty with
  | Precomputed -> Result.return ()
  | Live ->
    let ( let* ) = Result.( let* ) in
    let* length = Bars.length_check bars in
    let* () = compute_i x.tbl config bars (length - 1) in
    Result.return ()

let compute config bars =
  let ( let* ) = Result.( let* ) in
  let indicators = TimestampedTbl.create () in
  let* length = Bars.length_check bars in
  let rec aux i =
    let* i = i in
    if i >= length then Result.return i
    else
      let* () = compute_i indicators config bars i in
      aux @@ Result.return @@ (i + 1)
  in
  let* res = aux @@ Result.return 0 in
  Result.return res

let precompute (preload : Bars.t) (target : Bars.t) =
  let ( let* ) = Result.( let* ) in
  let config = { Indicator_config.fft = false; compare_preloaded = false } in
  let combined = Bars.combine [ preload; target ] in
  Eio.traceln "Beginning Indicators.compute";
  let* _ = compute config combined in
  Eio.traceln "Finished Indicators.compute";
  Result.return ()

(* let add_latest config timestamp (bars : Bars.t) (latest_bars : Bars.Latest.t) *)
(*     (x : t) = *)
(*   match x with *)
(*   | Precomputed -> () *)
(*   | Live x -> *)
(*     let iter f = Bars.Latest.iter f latest_bars in *)
(*     iter @@ fun symbol latest -> *)
(*     let symbol_history = *)
(*       Bars.get bars symbol |> function *)
(*       | Some x -> x *)
(*       | None -> assert false *)
(*     in *)
(*     let indicators_vector = *)
(*       match get x symbol with *)
(*       | Some i -> i *)
(*       | None -> *)
(*         let new_vector = initialize_single config bars symbol in *)
(*         Hashtbl.replace x symbol new_vector; *)
(*         new_vector *)
(*     in *)
(*     let previous = Vector.top indicators_vector in *)
(*     let new_indicators = *)
(*       Point.of_latest config timestamp symbol_history previous latest symbol *)
(*     in *)
(*     (if config.compare_preloaded then *)
(*        let from_tbl = TimestampedTbl.get symbol (Some timestamp) in *)
(*        match from_tbl with *)
(*        | Ok x -> *)
(*          if x.price <>. new_indicators.price || x.fso.k <>. new_indicators.fso.k *)
(*          then ( *)
(*            Eio.traceln "Price mismatch: @[%a@]@.@[%a@]@." Point.pp *)
(*              new_indicators Point.pp x; *)
(*            () (\* invalid_arg "price mismatch" *\)) *)
(*          else () *)
(*        | Error _ -> ()); *)
(*     Vector.push indicators_vector new_indicators *)
