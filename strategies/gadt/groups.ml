open Gadt_fo.Variable

(* Strict categorization by value ranges for enumeration *)

(* Type representing all indicator groups *)
module ComparisonClass = struct
  type t =
    | Bounded_0_100_bullish_high
    | Bounded_0_100_strength
    | Bounded_neg100_pos100
    | Bounded_neg100_0
    | Price_comparable
    | Momentum_centered_zero
    | Rate_of_change_around_one
    | Rate_of_change_percentage
    | Volume_indicators
    | Volatility_positive
    | Correlation_indicators
    | Beta_indicators
    | Slope_indicators
    | Index_indicators
    (* | Crossover_pairs *)
    | Moving_averages_only
    | Bollinger_bands_only
    | MACD_family
    | Stochastic_family
    | Directional_movement_family
    (* Additional useful groupings *)
    | Candlestick_patterns
    | Mathematical_transforms
    | Price_aggregations_only
    | Linear_regression_family
    | Hilbert_transform_family
  [@@deriving show]

  (* Indicators bounded 0-100: higher values = more bullish/overbought
   Standard thresholds: >80 overbought, >50 bullish, <20 oversold, <50 bearish *)
  let bounded_0_100_bullish_high =
    [
      rsi;
      (* Relative Strength Index: >80 overbought, <20 oversold, 14-period default *)
      stoch_slow_k;
      (* Slow Stochastic %K: >80 overbought, <20 oversold *)
      stoch_slow_d;
      (* Slow Stochastic %D: smoothed %K, >80 overbought, <20 oversold *)
      stoch_f_fast_k;
      (* Fast Stochastic %K: more sensitive, >80 overbought, <20 oversold *)
      stoch_f_fast_d;
      (* Fast Stochastic %D: smoothed fast %K, >80 overbought, <20 oversold *)
      stoch_rsi_fast_k;
      (* Stochastic RSI %K: combines RSI with stochastic, >80 overbought, <20 oversold *)
      stoch_rsi_fast_d;
      (* Stochastic RSI %D: smoothed stochastic RSI, >80 overbought, <20 oversold *)
      ultosc;
      (* Ultimate Oscillator: >80 overbought, <20 oversold, multi-timeframe *)
      mfi;
      (* Money Flow Index: volume-weighted RSI, >80 overbought, <20 oversold *)
      imi;
      (* Intraday Momentum Index: intraday price momentum, >80 overbought, <20 oversold *)
    ]

  (* Indicators bounded 0-100: higher values = stronger trend (not directional)
   Standard thresholds: >25 strong, >50 very strong, <25 weak trend *)
  let bounded_0_100_strength =
    [
      adx;
      (* Average Directional Index: >25 strong trend, >50 very strong, <25 weak trend *)
      adxr;
      (* ADX Rating: smoothed ADX, >25 strong trend, >50 very strong, <25 weak trend *)
      dx;
      (* Directional Movement Index: >25 strong, >50 very strong, <25 weak *)
      aroon_up;
      (* Aroon Up: >50 recent high likely, >80 very recent high, <50 stale high *)
      aroon_down;
      (* Aroon Down: >50 recent low likely, >80 very recent low, <50 stale low *)
      plus_di;
      (* Plus Directional Indicator: >25 strong upward movement, >50 very strong *)
      minus_di;
      (* Minus Directional Indicator: >25 strong downward movement, >50 very strong *)
    ]

  (* Indicators bounded -100 to +100: positive = bullish, negative = bearish
   Standard thresholds: >50 overbought, >0 bullish, <-50 oversold, <0 bearish *)
  let bounded_neg100_pos100 =
    [
      cmo;
      (* Chande Momentum Oscillator: >50 overbought, <-50 oversold, >0 bullish *)
      aroon_osc;
      (* Aroon Oscillator: >50 strong uptrend, <-50 strong downtrend, >0 bullish *)
      cci;
      (* Commodity Channel Index: >50 overbought, <-50 oversold, >0 bullish (can exceed ±100) *)
    ]

  (* Indicators bounded -100 to 0: closer to 0 = more bullish
   Standard thresholds: >-20 overbought, >-50 bullish, <-80 oversold, <-50 bearish *)
  let bounded_neg100_0 =
    [
      willr;
      (* Williams %R: >-20 overbought, >-50 bullish, <-80 oversold, <-50 bearish *)
    ]

  (* Price-comparable indicators: higher = more bullish when compared to price
   These indicators output values in price units and can be directly compared
   to the current price to generate buy/sell signals. Price above indicator
   generally suggests bullish conditions. *)
  let price_comparable =
    [
      (* Moving averages - smooth price trends, act as dynamic support/resistance *)
      dema;
      (* Double Exponential MA: reduced lag, responsive to price changes *)
      ema;
      (* Exponential MA: gives more weight to recent prices *)
      kama;
      (* Kaufman Adaptive MA: adapts to market volatility *)
      ma;
      (* Simple MA: basic average of prices *)
      sma;
      (* Simple MA: arithmetic mean of prices over period *)
      tema;
      (* Triple Exponential MA: even more responsive than DEMA *)
      trima;
      (* Triangular MA: double-smoothed, very smooth *)
      wma;
      (* Weighted MA: linear weighting favoring recent prices *)

      (* Price-based indicators - derived from OHLC data *)
      midpoint;
      (* (High + Low) / 2 over period *)
      midprice;
      (* Same as midpoint *)
      linearreg;
      (* Linear regression line value *)
      tsf;
      (* Time Series Forecast: projected price *)

      (* Price aggregations - typical price calculations *)
      avg_price;
      (* (OHLC)/4 *)
      med_price;
      (* (High + Low)/2 *)
      typ_price;
      (* (High + Low + Close)/3 *)
      wcl_price;
      (* (High + Low + 2*Close)/4 *)

      (* Bollinger Bands - volatility-based bands around price *)
      upper_bband;
      (* Upper band: SMA + (stddev * multiplier) *)
      middle_bband;
      (* Middle band: typically SMA *)
      lower_bband;
      (* Lower band: SMA - (stddev * multiplier) *)

      (* Parabolic SAR - trend-following stop and reverse *)
      sar;
      (* Stop and Reverse: trailing stop levels *)

      (* MAMA/FAMA adaptive moving averages *)
      mama_mama;
      (* MESA Adaptive MA: adapts to cycle *)
      mama_fama;
      (* Following Adaptive MA: smoother MAMA *)

      (* Linear regression components *)
      linearreg_intercept;
      (* Y-intercept of linear regression *)

      (* Min/Max values over periods *)
      min_max_min;
      (* Minimum value over period *)
      min_max_max;
      (* Maximum value over period *)
      max;
      (* Highest value over period *)
      min;
      (* Lowest value over period *)

      (* Acceleration Bands - volatility-based price bands *)
      accbands_upper;
      (* Upper acceleration band *)
      accbands_middle;
      (* Middle acceleration band *)
      accbands_lower;
      (* Lower acceleration band *)
    ]

  (* Momentum indicators: positive = bullish momentum, negative = bearish
   Standard thresholds: >0.5 bullish, >1.0 strong bullish, <-0.5 bearish, <-1.0 strong bearish
   (Values scaled to typical price units for MACD-type indicators) *)
  let momentum_centered_zero =
    [
      macd_macd;
      (* MACD Line: 12EMA - 26EMA, positive = bullish momentum *)
      macd_signal;
      (* MACD Signal: EMA of MACD line, positive = bullish trend *)
      macd_hist;
      (* MACD Histogram: MACD - Signal, positive = bullish divergence *)
      macd_ext_macd;
      (* Extended MACD: customizable periods *)
      macd_ext_signal;
      (* Extended MACD Signal: customizable smoothing *)
      macd_ext_hist;
      (* Extended MACD Histogram: extended version *)
      apo;
      (* Absolute Price Oscillator: fast MA - slow MA in price units *)
      trix;
      (* TRIX: triple-smoothed EMA rate of change, momentum oscillator *)
      mom;
      (* Momentum: current price - price N periods ago *)
      ppo;
      (* Percentage Price Oscillator: APO as percentage, positive = bullish *)
    ]

  (* Rate of change indicators: value around 1.0 = no change, >1.0 = bullish
   Standard thresholds: >1.02 bullish, >1.05 strong bullish, <0.98 bearish, <0.95 strong bearish *)
  let rate_of_change_around_one =
    [
      rocr;
      (* Rate of Change Ratio: >1.02 bullish, >1.05 strong bullish, <0.98 bearish, <0.95 strong bearish *)
      rocr100;
      (* ROC Ratio * 100: >102 bullish, >105 strong bullish, <98 bearish, <95 strong bearish *)
    ]

  (* Rate of change percentage: positive = bullish, negative = bearish
   Standard thresholds: >2% bullish, >5% strong bullish, <-2% bearish, <-5% strong bearish *)
  let rate_of_change_percentage =
    [
      roc;
      (* Rate of Change: >2.0 bullish, >5.0 strong bullish, <-2.0 bearish, <-5.0 strong bearish *)
      rocp;
      (* ROC Percentage: >0.02 bullish, >0.05 strong bullish, <-0.02 bearish, <-0.05 strong bearish *)
    ]

  (* Volume indicators: higher = more volume confirmation
   Standard interpretation: rising values = accumulation/bullish, falling values = distribution/bearish
   Compare current vs previous periods, or use rate of change >2% significant increase *)
  let volume_indicators =
    [
      ad;
      (* Accumulation/Distribution: volume-weighted price momentum *)
      obv;
      (* On-Balance Volume: running total of volume based on price direction *)
      adosc;
      (* A/D Oscillator: fast A/D MA - slow A/D MA *)
    ]

  (* Volatility indicators: higher = more volatile (not directional)
   Standard thresholds: >1.5x recent average = high volatility, >2.0x = very high volatility
   (Compare current reading vs 20-period moving average of same indicator) *)
  let volatility_positive =
    [
      atr;
      (* Average True Range: average of true range over period *)
      natr;
      (* Normalized ATR: ATR as percentage of price *)
      trange;
      (* True Range: max(high-low, high-prev_close, prev_close-low) *)
      avgdev;
      (* Average Deviation: average distance from mean *)
      stddev;
      (* Standard Deviation: statistical measure of price dispersion *)
      var_indicator;
      (* Variance: square of standard deviation *)
      plus_dm;
      (* Plus Directional Movement: upward price movement magnitude *)
      minus_dm;
      (* Minus Directional Movement: downward price movement magnitude *)
    ]

  (* Correlation indicators: +1 = perfect positive, -1 = perfect negative
   Standard thresholds: >0.8 strong positive, >0.5 moderate positive, <-0.8 strong negative, <-0.5 moderate negative *)
  let correlation_indicators =
    [
      correl
      (* Pearson Correlation: >0.8 strong positive, >0.5 moderate positive, <-0.8 strong negative, <-0.5 moderate negative *);
    ]

  (* Beta indicators: 1.0 = market beta, >1.0 = more volatile than market
   Standard thresholds: >1.2 high volatility, >1.5 very high volatility, <0.8 low volatility, <0.5 very low volatility *)
  let beta_indicators =
    [
      beta
      (* Beta coefficient: >1.2 high volatility, >1.5 very high volatility, <0.8 low volatility, <0.5 very low volatility *);
    ]

  (* Slope indicators: positive = upward trend, negative = downward trend
   Standard thresholds: >0.1 bullish trend, >0.5 strong bullish, <-0.1 bearish trend, <-0.5 strong bearish
   (slope in price units/period), >10° bullish angle, >30° strong bullish, <-10° bearish, <-30° strong bearish *)
  let slope_indicators =
    [
      linearreg_slope;
      (* Linear Regression Slope: >0.1 bullish, >0.5 strong bullish, <-0.1 bearish, <-0.5 strong bearish *)
      linearreg_angle;
      (* Linear Regression Angle: >10° bullish, >30° strong bullish, <-10° bearish, <-30° strong bearish *)
    ]

  (* Index indicators: position in lookback period (0 = most recent, higher = older)
   Standard interpretation: <5 = very recent extreme, <10 = recent extreme, >15 = stale extreme
   (For typical 20-period lookback) *)
  let index_indicators =
    [
      min_index;
      (* Index of Minimum: periods ago when lowest value occurred *)
      max_index;
      (* Index of Maximum: periods ago when highest value occurred *)
      min_max_index_min;
      (* Min/Max Index Min: periods ago when minimum occurred *)
      min_max_index_max;
      (* Min/Max Index Max: periods ago when maximum occurred *)
    ]

  (* Moving averages only - for pure trend-following strategies *)
  let moving_averages_only =
    [ dema; ema; kama; ma; sma; tema; trima; wma; mama_mama; mama_fama ]

  (* Bollinger Bands family - volatility-based bands *)
  let bollinger_bands_only = [ upper_bband; middle_bband; lower_bband ]

  (* MACD indicator family - all MACD variations *)
  let macd_family =
    [
      macd_macd;
      macd_signal;
      macd_hist;
      macd_ext_macd;
      macd_ext_signal;
      macd_ext_hist;
      apo;
      ppo;
      (* Related oscillators *)
    ]

  (* Stochastic oscillator family - all stochastic variations *)
  let stochastic_family =
    [
      stoch_slow_k;
      stoch_slow_d;
      stoch_f_fast_k;
      stoch_f_fast_d;
      stoch_rsi_fast_k;
      stoch_rsi_fast_d;
    ]

  (* Directional Movement family - ADX and related indicators *)
  let directional_movement_family =
    [
      adx;
      adxr;
      dx;
      plus_di;
      minus_di;
      plus_dm;
      minus_dm;
      aroon_up;
      aroon_down;
      aroon_osc;
    ]

  (* Additional useful groupings *)

  (* Candlestick pattern indicators - binary signals *)
  let candlestick_patterns =
    [
      cdl_2crows;
      cdl_3blackcrows;
      cdl_3inside;
      cdl_3linestrike;
      cdl_3outside;
      cdl_3starsinsouth;
      cdl_3whitesoldiers;
      cdl_abandonedbaby;
      cdl_advanceblock;
      cdl_belthold;
      cdl_breakaway;
      cdl_closingmarubozu;
      cdl_concealbabyswall;
      cdl_counterattack;
      cdl_darkcloudcover;
      cdl_doji;
      cdl_dojistar;
      cdl_dragonflydoji;
      cdl_engulfing;
      cdl_eveningdojistar;
      cdl_eveningstar;
      cdl_gap_side_side_white;
      cdl_gravestonedoji;
      cdl_hammer;
      cdl_hangingman;
      cdl_harami;
      cdl_haramicross;
      cdl_highwave;
      cdl_hikkake;
      cdl_hikkakemod;
      cdl_homingpigeon;
      cdl_identical3crows;
      cdl_inneck;
      cdl_invertedhammer;
      cdl_kicking;
      cdl_kickingbylength;
      cdl_ladderbottom;
      cdl_longleggedDoji;
      cdl_longline;
      cdl_marubozu;
      cdl_matchinglow;
      cdl_mathold;
      cdl_morningdojistar;
      cdl_morningstar;
      cdl_onneck;
      cdl_piercing;
      cdl_rickshawman;
      cdl_risefall3methods;
      cdl_separatinglines;
      cdl_shootingstar;
      cdl_shortline;
      cdl_spinningtop;
      cdl_stalledpattern;
      cdl_sticksandwich;
      cdl_takuri;
      cdl_tasukigap;
      cdl_thrusting;
      cdl_tristar;
      cdl_unique3river;
      cdl_upsidegap2crows;
      cdl_xsidegap3methods;
    ]

  (* Mathematical transform functions *)
  let mathematical_transforms =
    [
      acos;
      asin;
      atan;
      ceil;
      cos;
      cosh;
      exp;
      floor;
      ln;
      log10;
      sin;
      sinh;
      sqrt;
      tan;
      tanh;
      add;
      div;
      mult;
      sub;
    ]

  (* Pure price aggregation indicators *)
  let price_aggregations_only =
    [ avg_price; med_price; typ_price; wcl_price; midpoint; midprice ]

  (* Linear regression family *)
  let linear_regression_family =
    [ linearreg; linearreg_slope; linearreg_angle; linearreg_intercept; tsf ]

  (* Hilbert Transform family - cycle analysis *)
  let hilbert_transform_family =
    [
      ht_trendline;
      ht_dc_period;
      ht_dc_phase;
      ht_phasor_inphase;
      ht_phasor_quadrature;
      ht_sine_sine;
      ht_sine_leadsine;
      ht_trend_mode;
    ]

  (* Function to get the indicator list for a given group type *)
  let to_list = function
    | Bounded_0_100_bullish_high -> bounded_0_100_bullish_high
    | Bounded_0_100_strength -> bounded_0_100_strength
    | Bounded_neg100_pos100 -> bounded_neg100_pos100
    | Bounded_neg100_0 -> bounded_neg100_0
    | Price_comparable -> price_comparable
    | Momentum_centered_zero -> momentum_centered_zero
    | Rate_of_change_around_one -> rate_of_change_around_one
    | Rate_of_change_percentage -> rate_of_change_percentage
    | Volume_indicators -> volume_indicators
    | Volatility_positive -> volatility_positive
    | Correlation_indicators -> correlation_indicators
    | Beta_indicators -> beta_indicators
    | Slope_indicators -> slope_indicators
    | Index_indicators -> index_indicators
    | Moving_averages_only -> moving_averages_only
    | Bollinger_bands_only -> bollinger_bands_only
    | MACD_family -> macd_family
    | Stochastic_family -> stochastic_family
    | Directional_movement_family -> directional_movement_family
    (* | Crossover_pairs -> *)
    (*   [] (\* Crossover pairs are handled separately since they're tuples *\) *)
    | Candlestick_patterns -> candlestick_patterns
    | Mathematical_transforms -> mathematical_transforms
    | Price_aggregations_only -> price_aggregations_only
    | Linear_regression_family -> linear_regression_family
    | Hilbert_transform_family -> hilbert_transform_family

  (* Function to get group size, with special handling for crossover pairs *)
  let group_size group = List.length (to_list group)

  (* Function to get all group types *)
  let all =
    [
      Bounded_0_100_bullish_high;
      Bounded_0_100_strength;
      Bounded_neg100_pos100;
      Bounded_neg100_0;
      Price_comparable;
      Momentum_centered_zero;
      Rate_of_change_around_one;
      Rate_of_change_percentage;
      Volume_indicators;
      Volatility_positive;
      Correlation_indicators;
      Beta_indicators;
      Slope_indicators;
      Index_indicators;
      Moving_averages_only;
      Bollinger_bands_only;
      MACD_family;
      Stochastic_family;
      Directional_movement_family;
      Candlestick_patterns;
      Mathematical_transforms;
      Price_aggregations_only;
      Linear_regression_family;
      Hilbert_transform_family;
    ]

  let random state =
    let res = List.random_choose all state in
    List.random_choose (to_list res) state
end

(* Module for indicator value bounds *)
module Bounds = struct
  open ComparisonClass

  (* Get the typical value range (lower_bound, upper_bound) for indicators in a group *)
  let get_bounds = function
    | Bounded_0_100_bullish_high -> (0.0, 100.0)
    | Bounded_0_100_strength -> (0.0, 100.0)
    | Bounded_neg100_pos100 -> (-100.0, 100.0)
    | Bounded_neg100_0 -> (-100.0, 0.0)
    | Price_comparable ->
      (0.0, Float.max_value) (* Price-based, no upper bound *)
    | Momentum_centered_zero ->
      (Float.min_value, Float.max_value) (* Unbounded around zero *)
    | Rate_of_change_around_one ->
      (0.0, 3.0) (* Typically 0.5 to 2.0, allow some buffer *)
    | Rate_of_change_percentage ->
      (-50.0, 50.0) (* Typically -20% to +20%, allow buffer *)
    | Volume_indicators ->
      (0.0, Float.max_value) (* Volume-based, positive unbounded *)
    | Volatility_positive ->
      (0.0, Float.max_value) (* Volatility measures, positive only *)
    | Correlation_indicators -> (-1.0, 1.0)
    | Beta_indicators -> (0.0, 3.0) (* Typically 0.2 to 2.0, allow buffer *)
    | Slope_indicators ->
      (Float.min_value, Float.max_value) (* Can be any slope value *)
    | Index_indicators ->
      (0.0, Float.max_value) (* Index positions, non-negative *)
    | Moving_averages_only ->
      (0.0, Float.max_value) (* Price-based, no upper bound *)
    | Bollinger_bands_only ->
      (0.0, Float.max_value) (* Price-based, no upper bound *)
    | MACD_family ->
      (Float.min_value, Float.max_value) (* Mix of bounded and unbounded *)
    | Stochastic_family ->
      (0.0, 100.0) (* All stochastic indicators are 0-100 *)
    | Directional_movement_family ->
      (0.0, 100.0) (* Most are 0-100, with some unbounded *)
    | Candlestick_patterns ->
      (-100.0, 100.0) (* Typically return -100, 0, or 100 *)
    | Mathematical_transforms ->
      (Float.min_value, Float.max_value) (* Math functions, unbounded *)
    | Price_aggregations_only ->
      (0.0, Float.max_value) (* Price-based, positive *)
    | Linear_regression_family ->
      (Float.min_value, Float.max_value) (* Mixed types *)
    | Hilbert_transform_family -> (Float.min_value, Float.max_value)
  (* Mixed types *)

  (* Get meaningful threshold values for each group type *)
  let get_thresholds = function
    | Bounded_0_100_bullish_high ->
      [ 20.0; 50.0; 80.0 ] (* oversold; neutral; overbought *)
    | Bounded_0_100_strength ->
      [ 25.0; 50.0 ] (* strong trend; very strong trend *)
    | Bounded_neg100_pos100 ->
      [ -50.0; 0.0; 50.0 ] (* oversold; neutral; overbought *)
    | Bounded_neg100_0 ->
      [ -80.0; -50.0; -20.0 ] (* oversold; neutral; overbought *)
    | Price_comparable ->
      [] (* Price comparison thresholds depend on actual price data *)
    | Momentum_centered_zero ->
      [ -1.0; -0.5; 0.0; 0.5; 1.0 ]
      (* strong bearish; bearish; neutral; bullish; strong bullish *)
    | Rate_of_change_around_one ->
      [ 0.95; 0.98; 1.0; 1.02; 1.05 ]
      (* strong bearish; bearish; neutral; bullish; strong bullish *)
    | Rate_of_change_percentage ->
      [ -5.0; -2.0; 0.0; 2.0; 5.0 ]
      (* strong bearish; bearish; neutral; bullish; strong bullish *)
    | Volume_indicators ->
      [] (* Volume thresholds are relative to recent averages *)
    | Volatility_positive ->
      [] (* Volatility thresholds are relative to recent averages *)
    | Correlation_indicators ->
      [ -0.8; -0.5; 0.0; 0.5; 0.8 ]
      (* strong negative; moderate negative; neutral; moderate positive; strong positive *)
    | Beta_indicators ->
      [ 0.5; 0.8; 1.0; 1.2; 1.5 ]
      (* very low; low; market; high; very high volatility *)
    | Slope_indicators ->
      [ -0.5; -0.1; 0.0; 0.1; 0.5 ]
      (* strong bearish; bearish; neutral; bullish; strong bullish *)
    | Index_indicators -> [ 5.0; 10.0; 15.0 ] (* very recent; recent; stale *)
    | Moving_averages_only ->
      [] (* Price comparison thresholds depend on actual price data *)
    | Bollinger_bands_only ->
      [] (* Band comparison depends on price position relative to bands *)
    | MACD_family ->
      [ -1.0; -0.5; 0.0; 0.5; 1.0 ] (* Similar to momentum_centered_zero *)
    | Stochastic_family ->
      [ 20.0; 50.0; 80.0 ] (* Same as other 0-100 oscillators *)
    | Directional_movement_family ->
      [ 25.0; 50.0 ] (* Same as strength indicators *)
    | Candlestick_patterns ->
      [ -100.0; 0.0; 100.0 ] (* bearish pattern; no pattern; bullish pattern *)
    | Mathematical_transforms ->
      [] (* Math functions - thresholds depend on context *)
    | Price_aggregations_only ->
      [] (* Price comparison thresholds depend on actual price data *)
    | Linear_regression_family ->
      [ -0.5; -0.1; 0.0; 0.1; 0.5 ] (* Similar to slope indicators *)
    | Hilbert_transform_family ->
      [] (* Complex cycle indicators - thresholds depend on context *)

  (* Check if a value is within the typical bounds for a group *)
  let is_within_bounds group value =
    let lower, upper = get_bounds group in
    value >=. lower && value <=. upper

  (* Normalize a value to 0-1 range based on group bounds *)
  (* let normalize group value = *)
  (*   let (lower, upper) = get_bounds group in *)
  (*   if Float.is_infinite upper || Float.is_infinite lower then *)
  (*     (\* For unbounded ranges, can't normalize - return original value *\) *)
  (*     value *)
  (*   else *)
  (*     (value -. lower) /. (upper -. lower) *)
end

(* let random () = *)
(*   let l = List.random_choose ComparisonClass.all Longleaf_util.random_state |> ComparisonClass.to_list in *)
(*   Gadt.conjunction l *)

(* Module for meaningful combinations of indicator groups *)
module Combo = struct
  open ComparisonClass

  (* A combo is simply a list of comparison classes *)
  type t = ComparisonClass.t list [@@deriving show]

  (* Classic strategy combinations with names *)
  let classic_combos : (string * t) list =
    [
      ("MACD_RSI", [ MACD_family; Bounded_0_100_bullish_high ]);
      ("BB_Stoch", [ Bollinger_bands_only; Stochastic_family ]);
      ("MA_Cross_Volume", [ Volume_indicators ]);
      ("ADX_MACD", [ Directional_movement_family; MACD_family ]);
      ( "Triple_Screen",
        [
          Moving_averages_only;
          Bounded_0_100_bullish_high;
          Momentum_centered_zero;
        ] );
      ( "Trend_Momentum",
        [ Moving_averages_only; MACD_family; Bounded_0_100_strength ] );
      ( "Mean_Reversion",
        [
          Bollinger_bands_only; Bounded_0_100_bullish_high; Candlestick_patterns;
        ] );
      ( "Breakout_System",
        [ Price_comparable; Volume_indicators; Volatility_positive ] );
      ( "Momentum_Divergence",
        [ Price_comparable; MACD_family; Bounded_0_100_bullish_high ] );
      ( "Volatility_Breakout",
        [ Bollinger_bands_only; Volume_indicators; Bounded_0_100_strength ] );
    ]

  (* Contrarian combinations - looking for opposite signals *)
  let contrarian_combos =
    [
      ( "Oversold_Bounce",
        [ Bounded_0_100_bullish_high; Candlestick_patterns; Volume_indicators ]
      );
      ( "Overbought_Fade",
        [
          Bounded_0_100_bullish_high; Bounded_neg100_pos100; Volatility_positive;
        ] );
      ( "Divergence_Reversal",
        [ Price_comparable; Momentum_centered_zero; Bounded_0_100_bullish_high ]
      );
      ( "Failed_Breakout",
        [ Bollinger_bands_only; Volume_indicators; Candlestick_patterns ] );
    ]

  (* Trend-following combinations *)
  let trend_following_combos =
    [
      ("MA_Alignment", [ Moving_averages_only; Directional_movement_family ]);
      ( "Momentum_Trend",
        [ MACD_family; Bounded_0_100_strength; Moving_averages_only ] );
      ( "Breakout_Trend",
        [ Price_comparable; Volume_indicators; Moving_averages_only ] );
      (* ( "Cross_Trend", *)
      (* [ Crossover_pairs; Directional_movement_family; Volume_indicators ] ); *)
    ]

  (* Range-bound market combinations *)
  let range_bound_combos =
    [
      ( "Bollinger_Oscillator",
        [ Bollinger_bands_only; Bounded_0_100_bullish_high ] );
      ( "Support_Resistance",
        [ Price_comparable; Bounded_0_100_bullish_high; Candlestick_patterns ]
      );
      ( "Range_Reversal",
        [ Price_aggregations_only; Bounded_neg100_pos100; Volume_indicators ] );
      ( "Sideways_Trade",
        [
          Moving_averages_only; Bounded_0_100_bullish_high; Volatility_positive;
        ] );
    ]

  (* Get all combination types *)
  (* Get all named combo collections *)
  let all_named =
    classic_combos @ contrarian_combos @ trend_following_combos
    @ range_bound_combos

  (* Common 2-group combos *)
  let all_simple : t list =
    [
      (* [ Crossover_pairs; Momentum_centered_zero ]; *)
      (* [ Crossover_pairs; Bounded_0_100_strength ]; *)
      [ Bollinger_bands_only; Bounded_0_100_bullish_high ];
      [ Moving_averages_only; Volume_indicators ];
      [ MACD_family; Bounded_0_100_bullish_high ];
      [ Price_comparable; Volatility_positive ];
    ]

  (* Get all named combos as Combo.t list *)
  let all = List.map snd all_named @ all_simple

  (* (\* Helper function to get all indicators from a combo (excluding crossover pairs) *\) *)
  (* let get_indicators (combo : t) = *)
  (*   List.fold_left *)
  (*     (fun acc group -> *)
  (*       match group with *)
  (*       | Crossover_pairs -> acc (\* Handle crossover pairs separately *\) *)
  (*       | _ -> ComparisonClass.to_list group @ acc) *)
  (*     [] combo *)

  (* (\* Get crossover pairs from a combo *\) *)
  (* let get_crossovers (combo : t) = *)
  (*   if List.mem Crossover_pairs combo then ComparisonClass.get_crossovers () *)
  (*   else [] *)

  (* let random_gen len = List.random_len len ComparisonClass.random *)

  (* Check if combo contains crossovers *)
  (* let has_crossovers (combo : t) = List.mem Crossover_pairs combo *)

  (* Get combo size (total number of indicator groups) *)
  let size (combo : t) = List.length combo

  (* let conjunction (x : t) = *)
  (*   Gadt_strategy.buy_disj *)

  (* let conjunction *)

  (* Random combo generator *)
  (* let random_combo ?(min_groups=2) ?(max_groups=4) () = *)
  (*   let num_groups = Random.State.int_incl Longleaf_util.random_state min_groups max_groups in *)
  (*   let available_groups = ComparisonClass.all in *)
  (*   List.take num_groups (List.shuffle available_groups Longleaf_util.random_state) *)
end

module Crossover = struct
  (* Common crossover pairs - fast/slow combinations *)
  (*  These are classic technical analysis crossover strategies where the fast *)
  (*  indicator crossing above the slow indicator generates bullish signals, *)
  (*  and crossing below generates bearish signals. *)
  let all =
    [
      (* Moving Average Crossovers *)
      (ema, ema);
      (* Fast EMA / Slow EMA: e.g., 12/26, 5/20, 8/21 *)
      (sma, sma);
      (* Fast SMA / Slow SMA: e.g., 10/30, 20/50 *)
      (ema, sma);
      (* EMA / SMA: combines responsive with smooth *)
      (dema, ema);
      (* DEMA / EMA: very responsive vs standard *)
      (tema, ema);
      (* TEMA / EMA: ultra-responsive vs standard *)
      (kama, ema);
      (* Adaptive MA / EMA: adaptive vs standard *)
      (tema, dema);
      (* TEMA / DEMA: ultra-responsive vs very responsive *)
      (wma, ema);
      (* Weighted MA / EMA: linear vs exponential weighting *)
      (kama, sma);
      (* Adaptive MA / SMA: adaptive vs simple *)

      (* MACD System Crossovers *)
      (macd_macd, macd_signal);
      (* MACD Line / Signal Line: classic MACD crossover *)
      (macd_ext_macd, macd_ext_signal);
      (* Extended MACD crossover *)

      (* Adaptive MA Crossovers *)
      (mama_mama, mama_fama);
      (* MAMA / FAMA: adaptive trend following *)

      (* Stochastic Crossovers (FSO = Fast Stochastic Oscillator) *)
      (stoch_f_fast_k, stoch_f_fast_d);
      (* Fast %K / Fast %D: quick reversals *)
      (stoch_slow_k, stoch_slow_d);
      (* Slow %K / Slow %D: smoother signals *)
      (stoch_rsi_fast_k, stoch_rsi_fast_d);
      (* Stoch RSI %K / %D: RSI-based stochastic *)

      (* Directional Indicator Crossovers *)
      (plus_di, minus_di);
      (* +DI / -DI: directional movement crossover *)

      (* Price vs Indicator Crossovers (when price data available) *)
      (sar, sar);
      (* Price / SAR: trend reversal signals (placeholder for price) *)

      (* Linear Regression Crossovers *)
      (linearreg, tsf);
      (* Linear Reg / Time Series Forecast *)

      (* Aroon Crossovers *)
      (aroon_up, aroon_down);
      (* Aroon Up / Aroon Down: trend direction *)
    ]

  let random state =
    let l, r = List.random_choose all state |> fun (f, g) -> (f (), g ()) in
    match Random.bool () with
    | true -> Gadt.cross_up l r
    | false -> Gadt.cross_down l r
end

module Comparator = struct
  let random state =
    let lhs = ComparisonClass.random state () in
    match Random.bool () with
    | true -> Gadt.(lhs >. Gadt_fo.var Gadt.Type.Float)
    | false -> Gadt.(lhs <. Gadt_fo.var Gadt.Type.Float)
end

let random_of_int i state =
  match i with
  | true -> Crossover.random state
  | false -> Comparator.random state

let random_len state =
  let len = Int.random_range 2 4 state in
  let gen () = random_of_int (Random.bool ()) state in
  List.init len (fun _ -> gen ()) |> Gadt.conjunction
(* List.random_len (Random.map random_of_int) *)

let rand () = random_len Longleaf_util.random_state
