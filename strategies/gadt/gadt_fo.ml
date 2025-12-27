open Gadt
module I = Tacaml.Indicator.Raw

(* Create a variable with optional bounds for optimizer
   Default to [5.0, 100.0] to match current NLopt behavior *)
let var ?(lower = 5.0) ?(upper = 100.0) ty =
  Var (Uuidm.v4_gen Util.random_state (), ty, { lower; upper })

(* Wrap indicator expression with tacaml - returns float Gadt.t directly *)
let tacaml x = Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x), x))

(* ============================================================================
   Indicator module - Core layer taking Gadt expressions directly

   Use this when you need to pass custom variables or complex expressions.
   Example:
     let shared_period = Gadt_fo.var ~lower:10.0 ~upper:100.0 Type.Int in
     let rsi = Indicator.rsi shared_period
     let bb = Indicator.lower_bband shared_period (Const (2.0, Float)) (Const (2.0, Float))
   ============================================================================ *)
module Indicator = struct
  (* Single argument indicators (int period) *)
  let dema period = tacaml (App1 (Fun ("I.dema", I.dema), period))
  let ema period = tacaml (App1 (Fun ("I.ema", I.ema), period))
  let kama period = tacaml (App1 (Fun ("I.kama", I.kama), period))
  let ma period = tacaml (App1 (Fun ("I.ma", I.ma), period))
  let midpoint period = tacaml (App1 (Fun ("I.midpoint", I.midpoint), period))
  let midprice period = tacaml (App1 (Fun ("I.midprice", I.midprice), period))
  let sma period = tacaml (App1 (Fun ("I.sma", I.sma), period))
  let tema period = tacaml (App1 (Fun ("I.tema", I.tema), period))
  let trima period = tacaml (App1 (Fun ("I.trima", I.trima), period))
  let wma period = tacaml (App1 (Fun ("I.wma", I.wma), period))
  let adx period = tacaml (App1 (Fun ("I.adx", I.adx), period))
  let adxr period = tacaml (App1 (Fun ("I.adxr", I.adxr), period))
  let aroon_osc period = tacaml (App1 (Fun ("I.aroon_osc", I.aroon_osc), period))
  let cci period = tacaml (App1 (Fun ("I.cci", I.cci), period))
  let cmo period = tacaml (App1 (Fun ("I.cmo", I.cmo), period))
  let dx period = tacaml (App1 (Fun ("I.dx", I.dx), period))
  let mfi period = tacaml (App1 (Fun ("I.mfi", I.mfi), period))
  let minus_di period = tacaml (App1 (Fun ("I.minus_di", I.minus_di), period))
  let minus_dm period = tacaml (App1 (Fun ("I.minus_dm", I.minus_dm), period))
  let mom period = tacaml (App1 (Fun ("I.mom", I.mom), period))
  let plus_di period = tacaml (App1 (Fun ("I.plus_di", I.plus_di), period))
  let plus_dm period = tacaml (App1 (Fun ("I.plus_dm", I.plus_dm), period))
  let roc period = tacaml (App1 (Fun ("I.roc", I.roc), period))
  let rocp period = tacaml (App1 (Fun ("I.rocp", I.rocp), period))
  let rocr period = tacaml (App1 (Fun ("I.rocr", I.rocr), period))
  let rocr100 period = tacaml (App1 (Fun ("I.rocr100", I.rocr100), period))
  let rsi period = tacaml (App1 (Fun ("I.rsi", I.rsi), period))
  let trix period = tacaml (App1 (Fun ("I.trix", I.trix), period))
  let willr period = tacaml (App1 (Fun ("I.willr", I.willr), period))
  let atr period = tacaml (App1 (Fun ("I.atr", I.atr), period))
  let natr period = tacaml (App1 (Fun ("I.natr", I.natr), period))
  let aroon_down period = tacaml (App1 (Fun ("I.aroon_down", I.aroon_down), period))
  let aroon_up period = tacaml (App1 (Fun ("I.aroon_up", I.aroon_up), period))
  let beta period = tacaml (App1 (Fun ("I.beta", I.beta), period))
  let correl period = tacaml (App1 (Fun ("I.correl", I.correl), period))
  let linearreg period = tacaml (App1 (Fun ("I.linearreg", I.linearreg), period))
  let linearreg_angle period = tacaml (App1 (Fun ("I.linearreg_angle", I.linearreg_angle), period))
  let linearreg_intercept period = tacaml (App1 (Fun ("I.linearreg_intercept", I.linearreg_intercept), period))
  let linearreg_slope period = tacaml (App1 (Fun ("I.linearreg_slope", I.linearreg_slope), period))
  let min_max_min period = tacaml (App1 (Fun ("I.min_max_min", I.min_max_min), period))
  let min_max_max period = tacaml (App1 (Fun ("I.min_max_max", I.min_max_max), period))
  let tsf period = tacaml (App1 (Fun ("I.tsf", I.tsf), period))
  let max period = tacaml (App1 (Fun ("I.max", I.max), period))
  let min period = tacaml (App1 (Fun ("I.min", I.min), period))
  let sum period = tacaml (App1 (Fun ("I.sum", I.sum), period))
  let avgdev period = tacaml (App1 (Fun ("I.avgdev", I.avgdev), period))
  let imi period = tacaml (App1 (Fun ("I.imi", I.imi), period))
  let accbands_upper period = tacaml (App1 (Fun ("I.accbands_upper", I.accbands_upper), period))
  let accbands_middle period = tacaml (App1 (Fun ("I.accbands_middle", I.accbands_middle), period))
  let accbands_lower period = tacaml (App1 (Fun ("I.accbands_lower", I.accbands_lower), period))
  let max_index period = tacaml (App1 (Fun ("I.max_index", I.max_index), period))
  let min_index period = tacaml (App1 (Fun ("I.min_index", I.min_index), period))
  let min_max_index_min period = tacaml (App1 (Fun ("I.min_max_index_min", I.min_max_index_min), period))
  let min_max_index_max period = tacaml (App1 (Fun ("I.min_max_index_max", I.min_max_index_max), period))

  (* Two argument indicators *)
  let mama fast_limit slow_limit = tacaml (App2 (Fun ("I.mama", I.mama), fast_limit, slow_limit))
  let mavp min_period max_period = tacaml (App2 (Fun ("I.mavp", I.mavp), min_period, max_period))
  let sar accel maximum = tacaml (App2 (Fun ("I.sar", I.sar), accel, maximum))
  let t3 period vfactor = tacaml (App2 (Fun ("I.t3", I.t3), period, vfactor))
  let apo fast_period slow_period = tacaml (App2 (Fun ("I.apo", I.apo), fast_period, slow_period))
  let ppo fast_period slow_period = tacaml (App2 (Fun ("I.ppo", I.ppo), fast_period, slow_period))
  let adosc fast_period slow_period = tacaml (App2 (Fun ("I.adosc", I.adosc), fast_period, slow_period))
  let stddev period nbdev = tacaml (App2 (Fun ("I.stddev", I.stddev), period, nbdev))
  let var_indicator period nbdev = tacaml (App2 (Fun ("I.var", I.var), period, nbdev))
  let mama_mama fast_limit slow_limit = tacaml (App2 (Fun ("I.mama_mama", I.mama_mama), fast_limit, slow_limit))
  let mama_fama fast_limit slow_limit = tacaml (App2 (Fun ("I.mama_fama", I.mama_fama), fast_limit, slow_limit))
  let stoch_f_fast_k fast_k_period fast_d_period = tacaml (App2 (Fun ("I.stoch_f_fast_k", I.stoch_f_fast_k), fast_k_period, fast_d_period))
  let stoch_f_fast_d fast_k_period fast_d_period = tacaml (App2 (Fun ("I.stoch_f_fast_d", I.stoch_f_fast_d), fast_k_period, fast_d_period))

  (* Three argument indicators *)
  let upper_bband period nbdev_up nbdev_dn = tacaml (App3 (Fun ("I.upper_bband", I.upper_bband), period, nbdev_up, nbdev_dn))
  let middle_bband period nbdev_up nbdev_dn = tacaml (App3 (Fun ("I.middle_bband", I.middle_bband), period, nbdev_up, nbdev_dn))
  let lower_bband period nbdev_up nbdev_dn = tacaml (App3 (Fun ("I.lower_bband", I.lower_bband), period, nbdev_up, nbdev_dn))
  let macd_macd fast slow signal = tacaml (App3 (Fun ("I.macd_macd", I.macd_macd), fast, slow, signal))
  let macd_signal fast slow signal = tacaml (App3 (Fun ("I.macd_signal", I.macd_signal), fast, slow, signal))
  let macd_hist fast slow signal = tacaml (App3 (Fun ("I.macd_hist", I.macd_hist), fast, slow, signal))
  let macd_ext_macd fast slow signal = tacaml (App3 (Fun ("I.macd_ext_macd", I.macd_ext_macd), fast, slow, signal))
  let macd_ext_signal fast slow signal = tacaml (App3 (Fun ("I.macd_ext_signal", I.macd_ext_signal), fast, slow, signal))
  let macd_ext_hist fast slow signal = tacaml (App3 (Fun ("I.macd_ext_hist", I.macd_ext_hist), fast, slow, signal))
  let ultosc period1 period2 period3 = tacaml (App3 (Fun ("I.ultosc", I.ultosc), period1, period2, period3))
  let stoch_slow_k fast_k slow_k slow_d = tacaml (App3 (Fun ("I.stoch_slow_k", I.stoch_slow_k), fast_k, slow_k, slow_d))
  let stoch_slow_d fast_k slow_k slow_d = tacaml (App3 (Fun ("I.stoch_slow_d", I.stoch_slow_d), fast_k, slow_k, slow_d))
  let stoch_rsi_fast_k period fast_k fast_d = tacaml (App3 (Fun ("I.stoch_rsi_fast_k", I.stoch_rsi_fast_k), period, fast_k, fast_d))
  let stoch_rsi_fast_d period fast_k fast_d = tacaml (App3 (Fun ("I.stoch_rsi_fast_d", I.stoch_rsi_fast_d), period, fast_k, fast_d))

  (* Single float argument candlestick patterns *)
  let cdl_abandonedbaby penetration = tacaml (App1 (Fun ("I.cdl_abandonedbaby", I.cdl_abandonedbaby), penetration))
  let cdl_darkcloudcover penetration = tacaml (App1 (Fun ("I.cdl_darkcloudcover", I.cdl_darkcloudcover), penetration))
  let cdl_eveningdojistar penetration = tacaml (App1 (Fun ("I.cdl_eveningdojistar", I.cdl_eveningdojistar), penetration))
  let cdl_eveningstar penetration = tacaml (App1 (Fun ("I.cdl_eveningstar", I.cdl_eveningstar), penetration))
  let cdl_mathold penetration = tacaml (App1 (Fun ("I.cdl_mathold", I.cdl_mathold), penetration))
  let cdl_morningdojistar penetration = tacaml (App1 (Fun ("I.cdl_morningdojistar", I.cdl_morningdojistar), penetration))
  let cdl_morningstar penetration = tacaml (App1 (Fun ("I.cdl_morningstar", I.cdl_morningstar), penetration))

  (* Zero argument indicators *)
  let ht_trendline = tacaml (Const (I.ht_trendline, Tacaml))
  let ad = tacaml (Const (I.ad, Tacaml))
  let obv = tacaml (Const (I.obv, Tacaml))
  let trange = tacaml (Const (I.trange, Tacaml))
  let avg_price = tacaml (Const (I.avg_price, Tacaml))
  let med_price = tacaml (Const (I.med_price, Tacaml))
  let typ_price = tacaml (Const (I.typ_price, Tacaml))
  let wcl_price = tacaml (Const (I.wcl_price, Tacaml))
  let ht_dc_period = tacaml (Const (I.ht_dc_period, Tacaml))
  let ht_dc_phase = tacaml (Const (I.ht_dc_phase, Tacaml))
  let ht_phasor_inphase = tacaml (Const (I.ht_phasor_inphase, Tacaml))
  let ht_phasor_quadrature = tacaml (Const (I.ht_phasor_quadrature, Tacaml))
  let ht_sine_sine = tacaml (Const (I.ht_sine_sine, Tacaml))
  let ht_sine_leadsine = tacaml (Const (I.ht_sine_leadsine, Tacaml))
  let acos = tacaml (Const (I.acos, Tacaml))
  let asin = tacaml (Const (I.asin, Tacaml))
  let atan = tacaml (Const (I.atan, Tacaml))
  let ceil = tacaml (Const (I.ceil, Tacaml))
  let cos = tacaml (Const (I.cos, Tacaml))
  let cosh = tacaml (Const (I.cosh, Tacaml))
  let exp = tacaml (Const (I.exp, Tacaml))
  let floor = tacaml (Const (I.floor, Tacaml))
  let ln = tacaml (Const (I.ln, Tacaml))
  let log10 = tacaml (Const (I.log10, Tacaml))
  let sin = tacaml (Const (I.sin, Tacaml))
  let sinh = tacaml (Const (I.sinh, Tacaml))
  let sqrt = tacaml (Const (I.sqrt, Tacaml))
  let tan = tacaml (Const (I.tan, Tacaml))
  let tanh = tacaml (Const (I.tanh, Tacaml))
  let add = tacaml (Const (I.add, Tacaml))
  let div = tacaml (Const (I.div, Tacaml))
  let mult = tacaml (Const (I.mult, Tacaml))
  let sub = tacaml (Const (I.sub, Tacaml))
  let bop = tacaml (Const (I.bop, Tacaml))

  (* Zero argument candlestick patterns *)
  let ht_trend_mode = tacaml (Const (I.ht_trend_mode, Tacaml))
  let cdl_2crows = tacaml (Const (I.cdl_2crows, Tacaml))
  let cdl_3blackcrows = tacaml (Const (I.cdl_3blackcrows, Tacaml))
  let cdl_3inside = tacaml (Const (I.cdl_3inside, Tacaml))
  let cdl_3linestrike = tacaml (Const (I.cdl_3linestrike, Tacaml))
  let cdl_3outside = tacaml (Const (I.cdl_3outside, Tacaml))
  let cdl_3starsinsouth = tacaml (Const (I.cdl_3starsinsouth, Tacaml))
  let cdl_3whitesoldiers = tacaml (Const (I.cdl_3whitesoldiers, Tacaml))
  let cdl_advanceblock = tacaml (Const (I.cdl_advanceblock, Tacaml))
  let cdl_belthold = tacaml (Const (I.cdl_belthold, Tacaml))
  let cdl_breakaway = tacaml (Const (I.cdl_breakaway, Tacaml))
  let cdl_closingmarubozu = tacaml (Const (I.cdl_closingmarubozu, Tacaml))
  let cdl_concealbabyswall = tacaml (Const (I.cdl_concealbabyswall, Tacaml))
  let cdl_counterattack = tacaml (Const (I.cdl_counterattack, Tacaml))
  let cdl_doji = tacaml (Const (I.cdl_doji, Tacaml))
  let cdl_dojistar = tacaml (Const (I.cdl_dojistar, Tacaml))
  let cdl_dragonflydoji = tacaml (Const (I.cdl_dragonflydoji, Tacaml))
  let cdl_engulfing = tacaml (Const (I.cdl_engulfing, Tacaml))
  let cdl_gap_side_side_white = tacaml (Const (I.cdl_gap_side_side_white, Tacaml))
  let cdl_gravestonedoji = tacaml (Const (I.cdl_gravestonedoji, Tacaml))
  let cdl_hammer = tacaml (Const (I.cdl_hammer, Tacaml))
  let cdl_hangingman = tacaml (Const (I.cdl_hangingman, Tacaml))
  let cdl_harami = tacaml (Const (I.cdl_harami, Tacaml))
  let cdl_haramicross = tacaml (Const (I.cdl_haramicross, Tacaml))
  let cdl_highwave = tacaml (Const (I.cdl_highwave, Tacaml))
  let cdl_hikkake = tacaml (Const (I.cdl_hikkake, Tacaml))
  let cdl_hikkakemod = tacaml (Const (I.cdl_hikkakemod, Tacaml))
  let cdl_homingpigeon = tacaml (Const (I.cdl_homingpigeon, Tacaml))
  let cdl_identical3crows = tacaml (Const (I.cdl_identical3crows, Tacaml))
  let cdl_inneck = tacaml (Const (I.cdl_inneck, Tacaml))
  let cdl_invertedhammer = tacaml (Const (I.cdl_invertedhammer, Tacaml))
  let cdl_kicking = tacaml (Const (I.cdl_kicking, Tacaml))
  let cdl_kickingbylength = tacaml (Const (I.cdl_kickingbylength, Tacaml))
  let cdl_ladderbottom = tacaml (Const (I.cdl_ladderbottom, Tacaml))
  let cdl_longleggedDoji = tacaml (Const (I.cdl_longleggedDoji, Tacaml))
  let cdl_longline = tacaml (Const (I.cdl_longline, Tacaml))
  let cdl_marubozu = tacaml (Const (I.cdl_marubozu, Tacaml))
  let cdl_matchinglow = tacaml (Const (I.cdl_matchinglow, Tacaml))
  let cdl_onneck = tacaml (Const (I.cdl_onneck, Tacaml))
  let cdl_piercing = tacaml (Const (I.cdl_piercing, Tacaml))
  let cdl_rickshawman = tacaml (Const (I.cdl_rickshawman, Tacaml))
  let cdl_risefall3methods = tacaml (Const (I.cdl_risefall3methods, Tacaml))
  let cdl_separatinglines = tacaml (Const (I.cdl_separatinglines, Tacaml))
  let cdl_shootingstar = tacaml (Const (I.cdl_shootingstar, Tacaml))
  let cdl_shortline = tacaml (Const (I.cdl_shortline, Tacaml))
  let cdl_spinningtop = tacaml (Const (I.cdl_spinningtop, Tacaml))
  let cdl_stalledpattern = tacaml (Const (I.cdl_stalledpattern, Tacaml))
  let cdl_sticksandwich = tacaml (Const (I.cdl_sticksandwich, Tacaml))
  let cdl_takuri = tacaml (Const (I.cdl_takuri, Tacaml))
  let cdl_tasukigap = tacaml (Const (I.cdl_tasukigap, Tacaml))
  let cdl_thrusting = tacaml (Const (I.cdl_thrusting, Tacaml))
  let cdl_tristar = tacaml (Const (I.cdl_tristar, Tacaml))
  let cdl_unique3river = tacaml (Const (I.cdl_unique3river, Tacaml))
  let cdl_upsidegap2crows = tacaml (Const (I.cdl_upsidegap2crows, Tacaml))
  let cdl_xsidegap3methods = tacaml (Const (I.cdl_xsidegap3methods, Tacaml))
end

(* ============================================================================
   Constant module - Convenience layer for fixed values

   Use this for strategies with hardcoded indicator parameters.
   Example: let rsi = Constant.rsi 14
   ============================================================================ *)
module Constant = struct
  (* Single argument indicators *)
  let dema x = Indicator.dema (Const (x, Int))
  let ema x = Indicator.ema (Const (x, Int))
  let kama x = Indicator.kama (Const (x, Int))
  let ma x = Indicator.ma (Const (x, Int))
  let midpoint x = Indicator.midpoint (Const (x, Int))
  let midprice x = Indicator.midprice (Const (x, Int))
  let sma x = Indicator.sma (Const (x, Int))
  let tema x = Indicator.tema (Const (x, Int))
  let trima x = Indicator.trima (Const (x, Int))
  let wma x = Indicator.wma (Const (x, Int))
  let adx x = Indicator.adx (Const (x, Int))
  let adxr x = Indicator.adxr (Const (x, Int))
  let aroon_osc x = Indicator.aroon_osc (Const (x, Int))
  let cci x = Indicator.cci (Const (x, Int))
  let cmo x = Indicator.cmo (Const (x, Int))
  let dx x = Indicator.dx (Const (x, Int))
  let mfi x = Indicator.mfi (Const (x, Int))
  let minus_di x = Indicator.minus_di (Const (x, Int))
  let minus_dm x = Indicator.minus_dm (Const (x, Int))
  let mom x = Indicator.mom (Const (x, Int))
  let plus_di x = Indicator.plus_di (Const (x, Int))
  let plus_dm x = Indicator.plus_dm (Const (x, Int))
  let roc x = Indicator.roc (Const (x, Int))
  let rocp x = Indicator.rocp (Const (x, Int))
  let rocr x = Indicator.rocr (Const (x, Int))
  let rocr100 x = Indicator.rocr100 (Const (x, Int))
  let rsi x = Indicator.rsi (Const (x, Int))
  let trix x = Indicator.trix (Const (x, Int))
  let willr x = Indicator.willr (Const (x, Int))
  let atr x = Indicator.atr (Const (x, Int))
  let natr x = Indicator.natr (Const (x, Int))
  let aroon_down x = Indicator.aroon_down (Const (x, Int))
  let aroon_up x = Indicator.aroon_up (Const (x, Int))
  let beta x = Indicator.beta (Const (x, Int))
  let correl x = Indicator.correl (Const (x, Int))
  let linearreg x = Indicator.linearreg (Const (x, Int))
  let linearreg_angle x = Indicator.linearreg_angle (Const (x, Int))
  let linearreg_intercept x = Indicator.linearreg_intercept (Const (x, Int))
  let linearreg_slope x = Indicator.linearreg_slope (Const (x, Int))
  let min_max_min x = Indicator.min_max_min (Const (x, Int))
  let min_max_max x = Indicator.min_max_max (Const (x, Int))
  let tsf x = Indicator.tsf (Const (x, Int))
  let max x = Indicator.max (Const (x, Int))
  let min x = Indicator.min (Const (x, Int))
  let sum x = Indicator.sum (Const (x, Int))
  let avgdev x = Indicator.avgdev (Const (x, Int))
  let imi x = Indicator.imi (Const (x, Int))
  let accbands_upper x = Indicator.accbands_upper (Const (x, Int))
  let accbands_middle x = Indicator.accbands_middle (Const (x, Int))
  let accbands_lower x = Indicator.accbands_lower (Const (x, Int))
  let max_index x = Indicator.max_index (Const (x, Int))
  let min_index x = Indicator.min_index (Const (x, Int))
  let min_max_index_min x = Indicator.min_max_index_min (Const (x, Int))
  let min_max_index_max x = Indicator.min_max_index_max (Const (x, Int))

  (* Two argument indicators *)
  let mama x y = Indicator.mama (Const (x, Float)) (Const (y, Float))
  let mavp x y = Indicator.mavp (Const (x, Int)) (Const (y, Int))
  let sar x y = Indicator.sar (Const (x, Float)) (Const (y, Float))
  let t3 x y = Indicator.t3 (Const (x, Int)) (Const (y, Float))
  let apo x y = Indicator.apo (Const (x, Int)) (Const (y, Int))
  let ppo x y = Indicator.ppo (Const (x, Int)) (Const (y, Int))
  let adosc x y = Indicator.adosc (Const (x, Int)) (Const (y, Int))
  let stddev x y = Indicator.stddev (Const (x, Int)) (Const (y, Float))
  let var_indicator x y = Indicator.var_indicator (Const (x, Int)) (Const (y, Float))
  let mama_mama x y = Indicator.mama_mama (Const (x, Float)) (Const (y, Float))
  let mama_fama x y = Indicator.mama_fama (Const (x, Float)) (Const (y, Float))
  let stoch_f_fast_k x y = Indicator.stoch_f_fast_k (Const (x, Int)) (Const (y, Int))
  let stoch_f_fast_d x y = Indicator.stoch_f_fast_d (Const (x, Int)) (Const (y, Int))

  (* Three argument indicators *)
  let upper_bband x y z = Indicator.upper_bband (Const (x, Int)) (Const (y, Float)) (Const (z, Float))
  let middle_bband x y z = Indicator.middle_bband (Const (x, Int)) (Const (y, Float)) (Const (z, Float))
  let lower_bband x y z = Indicator.lower_bband (Const (x, Int)) (Const (y, Float)) (Const (z, Float))
  let macd_macd x y z = Indicator.macd_macd (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let macd_signal x y z = Indicator.macd_signal (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let macd_hist x y z = Indicator.macd_hist (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let macd_ext_macd x y z = Indicator.macd_ext_macd (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let macd_ext_signal x y z = Indicator.macd_ext_signal (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let macd_ext_hist x y z = Indicator.macd_ext_hist (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let ultosc x y z = Indicator.ultosc (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let stoch_slow_k x y z = Indicator.stoch_slow_k (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let stoch_slow_d x y z = Indicator.stoch_slow_d (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let stoch_rsi_fast_k x y z = Indicator.stoch_rsi_fast_k (Const (x, Int)) (Const (y, Int)) (Const (z, Int))
  let stoch_rsi_fast_d x y z = Indicator.stoch_rsi_fast_d (Const (x, Int)) (Const (y, Int)) (Const (z, Int))

  (* Single float argument candlestick patterns *)
  let cdl_abandonedbaby x = Indicator.cdl_abandonedbaby (Const (x, Float))
  let cdl_darkcloudcover x = Indicator.cdl_darkcloudcover (Const (x, Float))
  let cdl_eveningdojistar x = Indicator.cdl_eveningdojistar (Const (x, Float))
  let cdl_eveningstar x = Indicator.cdl_eveningstar (Const (x, Float))
  let cdl_mathold x = Indicator.cdl_mathold (Const (x, Float))
  let cdl_morningdojistar x = Indicator.cdl_morningdojistar (Const (x, Float))
  let cdl_morningstar x = Indicator.cdl_morningstar (Const (x, Float))

  (* Zero argument indicators - just re-export *)
  let ht_trendline = Indicator.ht_trendline
  let ad = Indicator.ad
  let obv = Indicator.obv
  let trange = Indicator.trange
  let avg_price = Indicator.avg_price
  let med_price = Indicator.med_price
  let typ_price = Indicator.typ_price
  let wcl_price = Indicator.wcl_price
  let ht_dc_period = Indicator.ht_dc_period
  let ht_dc_phase = Indicator.ht_dc_phase
  let ht_phasor_inphase = Indicator.ht_phasor_inphase
  let ht_phasor_quadrature = Indicator.ht_phasor_quadrature
  let ht_sine_sine = Indicator.ht_sine_sine
  let ht_sine_leadsine = Indicator.ht_sine_leadsine
  let acos = Indicator.acos
  let asin = Indicator.asin
  let atan = Indicator.atan
  let ceil = Indicator.ceil
  let cos = Indicator.cos
  let cosh = Indicator.cosh
  let exp = Indicator.exp
  let floor = Indicator.floor
  let ln = Indicator.ln
  let log10 = Indicator.log10
  let sin = Indicator.sin
  let sinh = Indicator.sinh
  let sqrt = Indicator.sqrt
  let tan = Indicator.tan
  let tanh = Indicator.tanh
  let add = Indicator.add
  let div = Indicator.div
  let mult = Indicator.mult
  let sub = Indicator.sub
  let bop = Indicator.bop

  (* Zero argument candlestick patterns - just re-export *)
  let ht_trend_mode = Indicator.ht_trend_mode
  let cdl_2crows = Indicator.cdl_2crows
  let cdl_3blackcrows = Indicator.cdl_3blackcrows
  let cdl_3inside = Indicator.cdl_3inside
  let cdl_3linestrike = Indicator.cdl_3linestrike
  let cdl_3outside = Indicator.cdl_3outside
  let cdl_3starsinsouth = Indicator.cdl_3starsinsouth
  let cdl_3whitesoldiers = Indicator.cdl_3whitesoldiers
  let cdl_advanceblock = Indicator.cdl_advanceblock
  let cdl_belthold = Indicator.cdl_belthold
  let cdl_breakaway = Indicator.cdl_breakaway
  let cdl_closingmarubozu = Indicator.cdl_closingmarubozu
  let cdl_concealbabyswall = Indicator.cdl_concealbabyswall
  let cdl_counterattack = Indicator.cdl_counterattack
  let cdl_doji = Indicator.cdl_doji
  let cdl_dojistar = Indicator.cdl_dojistar
  let cdl_dragonflydoji = Indicator.cdl_dragonflydoji
  let cdl_engulfing = Indicator.cdl_engulfing
  let cdl_gap_side_side_white = Indicator.cdl_gap_side_side_white
  let cdl_gravestonedoji = Indicator.cdl_gravestonedoji
  let cdl_hammer = Indicator.cdl_hammer
  let cdl_hangingman = Indicator.cdl_hangingman
  let cdl_harami = Indicator.cdl_harami
  let cdl_haramicross = Indicator.cdl_haramicross
  let cdl_highwave = Indicator.cdl_highwave
  let cdl_hikkake = Indicator.cdl_hikkake
  let cdl_hikkakemod = Indicator.cdl_hikkakemod
  let cdl_homingpigeon = Indicator.cdl_homingpigeon
  let cdl_identical3crows = Indicator.cdl_identical3crows
  let cdl_inneck = Indicator.cdl_inneck
  let cdl_invertedhammer = Indicator.cdl_invertedhammer
  let cdl_kicking = Indicator.cdl_kicking
  let cdl_kickingbylength = Indicator.cdl_kickingbylength
  let cdl_ladderbottom = Indicator.cdl_ladderbottom
  let cdl_longleggedDoji = Indicator.cdl_longleggedDoji
  let cdl_longline = Indicator.cdl_longline
  let cdl_marubozu = Indicator.cdl_marubozu
  let cdl_matchinglow = Indicator.cdl_matchinglow
  let cdl_onneck = Indicator.cdl_onneck
  let cdl_piercing = Indicator.cdl_piercing
  let cdl_rickshawman = Indicator.cdl_rickshawman
  let cdl_risefall3methods = Indicator.cdl_risefall3methods
  let cdl_separatinglines = Indicator.cdl_separatinglines
  let cdl_shootingstar = Indicator.cdl_shootingstar
  let cdl_shortline = Indicator.cdl_shortline
  let cdl_spinningtop = Indicator.cdl_spinningtop
  let cdl_stalledpattern = Indicator.cdl_stalledpattern
  let cdl_sticksandwich = Indicator.cdl_sticksandwich
  let cdl_takuri = Indicator.cdl_takuri
  let cdl_tasukigap = Indicator.cdl_tasukigap
  let cdl_thrusting = Indicator.cdl_thrusting
  let cdl_tristar = Indicator.cdl_tristar
  let cdl_unique3river = Indicator.cdl_unique3river
  let cdl_upsidegap2crows = Indicator.cdl_upsidegap2crows
  let cdl_xsidegap3methods = Indicator.cdl_xsidegap3methods
end

(* ============================================================================
   Variable module - Convenience layer for independent optimization variables

   Use this when each indicator parameter should be a separate optimization
   variable. Each call creates fresh variables.
   Example: let rsi = Variable.rsi ~lower:5.0 ~upper:30.0 ()
   ============================================================================ *)
module Variable = struct
  (* Single argument indicators *)
  let dema ?lower ?upper () = Indicator.dema (var ?lower ?upper Type.Int)
  let ema ?lower ?upper () = Indicator.ema (var ?lower ?upper Type.Int)
  let kama ?lower ?upper () = Indicator.kama (var ?lower ?upper Type.Int)
  let ma ?lower ?upper () = Indicator.ma (var ?lower ?upper Type.Int)
  let midpoint ?lower ?upper () = Indicator.midpoint (var ?lower ?upper Type.Int)
  let midprice ?lower ?upper () = Indicator.midprice (var ?lower ?upper Type.Int)
  let sma ?lower ?upper () = Indicator.sma (var ?lower ?upper Type.Int)
  let tema ?lower ?upper () = Indicator.tema (var ?lower ?upper Type.Int)
  let trima ?lower ?upper () = Indicator.trima (var ?lower ?upper Type.Int)
  let wma ?lower ?upper () = Indicator.wma (var ?lower ?upper Type.Int)
  let adx ?lower ?upper () = Indicator.adx (var ?lower ?upper Type.Int)
  let adxr ?lower ?upper () = Indicator.adxr (var ?lower ?upper Type.Int)
  let aroon_osc ?lower ?upper () = Indicator.aroon_osc (var ?lower ?upper Type.Int)
  let cci ?lower ?upper () = Indicator.cci (var ?lower ?upper Type.Int)
  let cmo ?lower ?upper () = Indicator.cmo (var ?lower ?upper Type.Int)
  let dx ?lower ?upper () = Indicator.dx (var ?lower ?upper Type.Int)
  let mfi ?lower ?upper () = Indicator.mfi (var ?lower ?upper Type.Int)
  let minus_di ?lower ?upper () = Indicator.minus_di (var ?lower ?upper Type.Int)
  let minus_dm ?lower ?upper () = Indicator.minus_dm (var ?lower ?upper Type.Int)
  let mom ?lower ?upper () = Indicator.mom (var ?lower ?upper Type.Int)
  let plus_di ?lower ?upper () = Indicator.plus_di (var ?lower ?upper Type.Int)
  let plus_dm ?lower ?upper () = Indicator.plus_dm (var ?lower ?upper Type.Int)
  let roc ?lower ?upper () = Indicator.roc (var ?lower ?upper Type.Int)
  let rocp ?lower ?upper () = Indicator.rocp (var ?lower ?upper Type.Int)
  let rocr ?lower ?upper () = Indicator.rocr (var ?lower ?upper Type.Int)
  let rocr100 ?lower ?upper () = Indicator.rocr100 (var ?lower ?upper Type.Int)
  let rsi ?lower ?upper () = Indicator.rsi (var ?lower ?upper Type.Int)
  let trix ?lower ?upper () = Indicator.trix (var ?lower ?upper Type.Int)
  let willr ?lower ?upper () = Indicator.willr (var ?lower ?upper Type.Int)
  let atr ?lower ?upper () = Indicator.atr (var ?lower ?upper Type.Int)
  let natr ?lower ?upper () = Indicator.natr (var ?lower ?upper Type.Int)
  let aroon_down ?lower ?upper () = Indicator.aroon_down (var ?lower ?upper Type.Int)
  let aroon_up ?lower ?upper () = Indicator.aroon_up (var ?lower ?upper Type.Int)
  let beta ?lower ?upper () = Indicator.beta (var ?lower ?upper Type.Int)
  let correl ?lower ?upper () = Indicator.correl (var ?lower ?upper Type.Int)
  let linearreg ?lower ?upper () = Indicator.linearreg (var ?lower ?upper Type.Int)
  let linearreg_angle ?lower ?upper () = Indicator.linearreg_angle (var ?lower ?upper Type.Int)
  let linearreg_intercept ?lower ?upper () = Indicator.linearreg_intercept (var ?lower ?upper Type.Int)
  let linearreg_slope ?lower ?upper () = Indicator.linearreg_slope (var ?lower ?upper Type.Int)
  let min_max_min ?lower ?upper () = Indicator.min_max_min (var ?lower ?upper Type.Int)
  let min_max_max ?lower ?upper () = Indicator.min_max_max (var ?lower ?upper Type.Int)
  let tsf ?lower ?upper () = Indicator.tsf (var ?lower ?upper Type.Int)
  let max ?lower ?upper () = Indicator.max (var ?lower ?upper Type.Int)
  let min ?lower ?upper () = Indicator.min (var ?lower ?upper Type.Int)
  let sum ?lower ?upper () = Indicator.sum (var ?lower ?upper Type.Int)
  let avgdev ?lower ?upper () = Indicator.avgdev (var ?lower ?upper Type.Int)
  let imi ?lower ?upper () = Indicator.imi (var ?lower ?upper Type.Int)
  let accbands_upper ?lower ?upper () = Indicator.accbands_upper (var ?lower ?upper Type.Int)
  let accbands_middle ?lower ?upper () = Indicator.accbands_middle (var ?lower ?upper Type.Int)
  let accbands_lower ?lower ?upper () = Indicator.accbands_lower (var ?lower ?upper Type.Int)
  let max_index ?lower ?upper () = Indicator.max_index (var ?lower ?upper Type.Int)
  let min_index ?lower ?upper () = Indicator.min_index (var ?lower ?upper Type.Int)
  let min_max_index_min ?lower ?upper () = Indicator.min_max_index_min (var ?lower ?upper Type.Int)
  let min_max_index_max ?lower ?upper () = Indicator.min_max_index_max (var ?lower ?upper Type.Int)

  (* Two argument indicators *)
  let mama ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.mama (var ?lower:lower1 ?upper:upper1 Type.Float) (var ?lower:lower2 ?upper:upper2 Type.Float)
  let mavp ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.mavp (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int)
  let sar ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.sar (var ?lower:lower1 ?upper:upper1 Type.Float) (var ?lower:lower2 ?upper:upper2 Type.Float)
  let t3 ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.t3 (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Float)
  let apo ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.apo (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int)
  let ppo ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.ppo (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int)
  let adosc ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.adosc (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int)
  let stddev ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.stddev (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Float)
  let var_indicator ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.var_indicator (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Float)
  let mama_mama ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.mama_mama (var ?lower:lower1 ?upper:upper1 Type.Float) (var ?lower:lower2 ?upper:upper2 Type.Float)
  let mama_fama ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.mama_fama (var ?lower:lower1 ?upper:upper1 Type.Float) (var ?lower:lower2 ?upper:upper2 Type.Float)
  let stoch_f_fast_k ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.stoch_f_fast_k (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int)
  let stoch_f_fast_d ?lower1 ?upper1 ?lower2 ?upper2 () =
    Indicator.stoch_f_fast_d (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int)

  (* Three argument indicators *)
  let upper_bband ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.upper_bband (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Float) (var ?lower:lower3 ?upper:upper3 Type.Float)
  let middle_bband ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.middle_bband (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Float) (var ?lower:lower3 ?upper:upper3 Type.Float)
  let lower_bband ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.lower_bband (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Float) (var ?lower:lower3 ?upper:upper3 Type.Float)
  let macd_macd ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.macd_macd (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let macd_signal ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.macd_signal (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let macd_hist ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.macd_hist (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let macd_ext_macd ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.macd_ext_macd (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let macd_ext_signal ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.macd_ext_signal (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let macd_ext_hist ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.macd_ext_hist (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let ultosc ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.ultosc (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let stoch_slow_k ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.stoch_slow_k (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let stoch_slow_d ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.stoch_slow_d (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let stoch_rsi_fast_k ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.stoch_rsi_fast_k (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)
  let stoch_rsi_fast_d ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    Indicator.stoch_rsi_fast_d (var ?lower:lower1 ?upper:upper1 Type.Int) (var ?lower:lower2 ?upper:upper2 Type.Int) (var ?lower:lower3 ?upper:upper3 Type.Int)

  (* Single float argument candlestick patterns *)
  let cdl_abandonedbaby ?lower ?upper () = Indicator.cdl_abandonedbaby (var ?lower ?upper Type.Float)
  let cdl_darkcloudcover ?lower ?upper () = Indicator.cdl_darkcloudcover (var ?lower ?upper Type.Float)
  let cdl_eveningdojistar ?lower ?upper () = Indicator.cdl_eveningdojistar (var ?lower ?upper Type.Float)
  let cdl_eveningstar ?lower ?upper () = Indicator.cdl_eveningstar (var ?lower ?upper Type.Float)
  let cdl_mathold ?lower ?upper () = Indicator.cdl_mathold (var ?lower ?upper Type.Float)
  let cdl_morningdojistar ?lower ?upper () = Indicator.cdl_morningdojistar (var ?lower ?upper Type.Float)
  let cdl_morningstar ?lower ?upper () = Indicator.cdl_morningstar (var ?lower ?upper Type.Float)

  (* Zero argument indicators - wrap in function for consistency with () pattern *)
  let ht_trendline () = Indicator.ht_trendline
  let ad () = Indicator.ad
  let obv () = Indicator.obv
  let trange () = Indicator.trange
  let avg_price () = Indicator.avg_price
  let med_price () = Indicator.med_price
  let typ_price () = Indicator.typ_price
  let wcl_price () = Indicator.wcl_price
  let ht_dc_period () = Indicator.ht_dc_period
  let ht_dc_phase () = Indicator.ht_dc_phase
  let ht_phasor_inphase () = Indicator.ht_phasor_inphase
  let ht_phasor_quadrature () = Indicator.ht_phasor_quadrature
  let ht_sine_sine () = Indicator.ht_sine_sine
  let ht_sine_leadsine () = Indicator.ht_sine_leadsine
  let acos () = Indicator.acos
  let asin () = Indicator.asin
  let atan () = Indicator.atan
  let ceil () = Indicator.ceil
  let cos () = Indicator.cos
  let cosh () = Indicator.cosh
  let exp () = Indicator.exp
  let floor () = Indicator.floor
  let ln () = Indicator.ln
  let log10 () = Indicator.log10
  let sin () = Indicator.sin
  let sinh () = Indicator.sinh
  let sqrt () = Indicator.sqrt
  let tan () = Indicator.tan
  let tanh () = Indicator.tanh
  let add () = Indicator.add
  let div () = Indicator.div
  let mult () = Indicator.mult
  let sub () = Indicator.sub
  let bop () = Indicator.bop

  (* Zero argument candlestick patterns *)
  let ht_trend_mode () = Indicator.ht_trend_mode
  let cdl_2crows () = Indicator.cdl_2crows
  let cdl_3blackcrows () = Indicator.cdl_3blackcrows
  let cdl_3inside () = Indicator.cdl_3inside
  let cdl_3linestrike () = Indicator.cdl_3linestrike
  let cdl_3outside () = Indicator.cdl_3outside
  let cdl_3starsinsouth () = Indicator.cdl_3starsinsouth
  let cdl_3whitesoldiers () = Indicator.cdl_3whitesoldiers
  let cdl_advanceblock () = Indicator.cdl_advanceblock
  let cdl_belthold () = Indicator.cdl_belthold
  let cdl_breakaway () = Indicator.cdl_breakaway
  let cdl_closingmarubozu () = Indicator.cdl_closingmarubozu
  let cdl_concealbabyswall () = Indicator.cdl_concealbabyswall
  let cdl_counterattack () = Indicator.cdl_counterattack
  let cdl_doji () = Indicator.cdl_doji
  let cdl_dojistar () = Indicator.cdl_dojistar
  let cdl_dragonflydoji () = Indicator.cdl_dragonflydoji
  let cdl_engulfing () = Indicator.cdl_engulfing
  let cdl_gap_side_side_white () = Indicator.cdl_gap_side_side_white
  let cdl_gravestonedoji () = Indicator.cdl_gravestonedoji
  let cdl_hammer () = Indicator.cdl_hammer
  let cdl_hangingman () = Indicator.cdl_hangingman
  let cdl_harami () = Indicator.cdl_harami
  let cdl_haramicross () = Indicator.cdl_haramicross
  let cdl_highwave () = Indicator.cdl_highwave
  let cdl_hikkake () = Indicator.cdl_hikkake
  let cdl_hikkakemod () = Indicator.cdl_hikkakemod
  let cdl_homingpigeon () = Indicator.cdl_homingpigeon
  let cdl_identical3crows () = Indicator.cdl_identical3crows
  let cdl_inneck () = Indicator.cdl_inneck
  let cdl_invertedhammer () = Indicator.cdl_invertedhammer
  let cdl_kicking () = Indicator.cdl_kicking
  let cdl_kickingbylength () = Indicator.cdl_kickingbylength
  let cdl_ladderbottom () = Indicator.cdl_ladderbottom
  let cdl_longleggedDoji () = Indicator.cdl_longleggedDoji
  let cdl_longline () = Indicator.cdl_longline
  let cdl_marubozu () = Indicator.cdl_marubozu
  let cdl_matchinglow () = Indicator.cdl_matchinglow
  let cdl_onneck () = Indicator.cdl_onneck
  let cdl_piercing () = Indicator.cdl_piercing
  let cdl_rickshawman () = Indicator.cdl_rickshawman
  let cdl_risefall3methods () = Indicator.cdl_risefall3methods
  let cdl_separatinglines () = Indicator.cdl_separatinglines
  let cdl_shootingstar () = Indicator.cdl_shootingstar
  let cdl_shortline () = Indicator.cdl_shortline
  let cdl_spinningtop () = Indicator.cdl_spinningtop
  let cdl_stalledpattern () = Indicator.cdl_stalledpattern
  let cdl_sticksandwich () = Indicator.cdl_sticksandwich
  let cdl_takuri () = Indicator.cdl_takuri
  let cdl_tasukigap () = Indicator.cdl_tasukigap
  let cdl_thrusting () = Indicator.cdl_thrusting
  let cdl_tristar () = Indicator.cdl_tristar
  let cdl_unique3river () = Indicator.cdl_unique3river
  let cdl_upsidegap2crows () = Indicator.cdl_upsidegap2crows
  let cdl_xsidegap3methods () = Indicator.cdl_xsidegap3methods
end
