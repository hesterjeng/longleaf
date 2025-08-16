open Gadt
module I = Tacaml.Indicator.Raw

let var ty = Var (Uuidm.v4_gen Util.random_state (), ty)
let tacaml x = Data (App1 (Fun (fun x -> Data.Type.Tacaml x), x))

module Variable = struct
  (* Single argument indicators *)
  let dema = tacaml (App1 (Fun I.dema, var Type.Int))
  let ema = tacaml (App1 (Fun I.ema, var Type.Int))
  let kama = tacaml (App1 (Fun I.kama, var Type.Int))
  let ma = tacaml (App1 (Fun I.ma, var Type.Int))
  let midpoint = tacaml (App1 (Fun I.midpoint, var Type.Int))
  let midprice = tacaml (App1 (Fun I.midprice, var Type.Int))
  let sma = tacaml (App1 (Fun I.sma, var Type.Int))
  let tema = tacaml (App1 (Fun I.tema, var Type.Int))
  let trima = tacaml (App1 (Fun I.trima, var Type.Int))
  let wma = tacaml (App1 (Fun I.wma, var Type.Int))
  let adx = tacaml (App1 (Fun I.adx, var Type.Int))
  let adxr = tacaml (App1 (Fun I.adxr, var Type.Int))
  let aroon_osc = tacaml (App1 (Fun I.aroon_osc, var Type.Int))
  let cci = tacaml (App1 (Fun I.cci, var Type.Int))
  let cmo = tacaml (App1 (Fun I.cmo, var Type.Int))
  let dx = tacaml (App1 (Fun I.dx, var Type.Int))
  let mfi = tacaml (App1 (Fun I.mfi, var Type.Int))
  let minus_di = tacaml (App1 (Fun I.minus_di, var Type.Int))
  let minus_dm = tacaml (App1 (Fun I.minus_dm, var Type.Int))
  let mom = tacaml (App1 (Fun I.mom, var Type.Int))
  let plus_di = tacaml (App1 (Fun I.plus_di, var Type.Int))
  let plus_dm = tacaml (App1 (Fun I.plus_dm, var Type.Int))
  let roc = tacaml (App1 (Fun I.roc, var Type.Int))
  let rocp = tacaml (App1 (Fun I.rocp, var Type.Int))
  let rocr = tacaml (App1 (Fun I.rocr, var Type.Int))
  let rocr100 = tacaml (App1 (Fun I.rocr100, var Type.Int))
  let rsi = tacaml (App1 (Fun I.rsi, var Type.Int))
  let trix = tacaml (App1 (Fun I.trix, var Type.Int))
  let willr = tacaml (App1 (Fun I.willr, var Type.Int))
  let atr = tacaml (App1 (Fun I.atr, var Type.Int))
  let natr = tacaml (App1 (Fun I.natr, var Type.Int))
  let aroon_down = tacaml (App1 (Fun I.aroon_down, var Type.Int))
  let aroon_up = tacaml (App1 (Fun I.aroon_up, var Type.Int))
  let beta = tacaml (App1 (Fun I.beta, var Type.Int))
  let correl = tacaml (App1 (Fun I.correl, var Type.Int))
  let linearreg = tacaml (App1 (Fun I.linearreg, var Type.Int))
  let linearreg_angle = tacaml (App1 (Fun I.linearreg_angle, var Type.Int))

  let linearreg_intercept =
    tacaml (App1 (Fun I.linearreg_intercept, var Type.Int))

  let linearreg_slope = tacaml (App1 (Fun I.linearreg_slope, var Type.Int))
  let min_max_min = tacaml (App1 (Fun I.min_max_min, var Type.Int))
  let min_max_max = tacaml (App1 (Fun I.min_max_max, var Type.Int))
  let tsf = tacaml (App1 (Fun I.tsf, var Type.Int))
  let max = tacaml (App1 (Fun I.max, var Type.Int))
  let min = tacaml (App1 (Fun I.min, var Type.Int))
  let sum = tacaml (App1 (Fun I.sum, var Type.Int))
  let avgdev = tacaml (App1 (Fun I.avgdev, var Type.Int))
  let imi = tacaml (App1 (Fun I.imi, var Type.Int))
  let accbands_upper = tacaml (App1 (Fun I.accbands_upper, var Type.Int))
  let accbands_middle = tacaml (App1 (Fun I.accbands_middle, var Type.Int))
  let accbands_lower = tacaml (App1 (Fun I.accbands_lower, var Type.Int))

  (* Two argument indicators *)
  let mama = tacaml (App2 (Fun I.mama, var Type.Float, var Type.Float))
  let mavp = tacaml (App2 (Fun I.mavp, var Type.Int, var Type.Int))
  let sar = tacaml (App2 (Fun I.sar, var Type.Float, var Type.Float))
  let t3 = tacaml (App2 (Fun I.t3, var Type.Int, var Type.Float))
  let apo = tacaml (App2 (Fun I.apo, var Type.Int, var Type.Int))
  let ppo = tacaml (App2 (Fun I.ppo, var Type.Int, var Type.Int))
  let adosc = tacaml (App2 (Fun I.adosc, var Type.Int, var Type.Int))
  let stddev = tacaml (App2 (Fun I.stddev, var Type.Int, var Type.Float))
  let var_indicator = tacaml (App2 (Fun I.var, var Type.Int, var Type.Float))

  let mama_mama =
    tacaml (App2 (Fun I.mama_mama, var Type.Float, var Type.Float))

  let mama_fama =
    tacaml (App2 (Fun I.mama_fama, var Type.Float, var Type.Float))

  let stoch_f_fast_k =
    tacaml (App2 (Fun I.stoch_f_fast_k, var Type.Int, var Type.Int))

  let stoch_f_fast_d =
    tacaml (App2 (Fun I.stoch_f_fast_d, var Type.Int, var Type.Int))

  (* Three argument indicators *)
  let upper_bband =
    tacaml
      (App3 (Fun I.upper_bband, var Type.Int, var Type.Float, var Type.Float))

  let middle_bband =
    tacaml
      (App3 (Fun I.middle_bband, var Type.Int, var Type.Float, var Type.Float))

  let lower_bband =
    tacaml
      (App3 (Fun I.lower_bband, var Type.Int, var Type.Float, var Type.Float))

  let macd_macd =
    tacaml (App3 (Fun I.macd_macd, var Type.Int, var Type.Int, var Type.Int))

  let macd_signal =
    tacaml (App3 (Fun I.macd_signal, var Type.Int, var Type.Int, var Type.Int))

  let macd_hist =
    tacaml (App3 (Fun I.macd_hist, var Type.Int, var Type.Int, var Type.Int))

  let macd_ext_macd =
    tacaml
      (App3 (Fun I.macd_ext_macd, var Type.Int, var Type.Int, var Type.Int))

  let macd_ext_signal =
    tacaml
      (App3 (Fun I.macd_ext_signal, var Type.Int, var Type.Int, var Type.Int))

  let macd_ext_hist =
    tacaml
      (App3 (Fun I.macd_ext_hist, var Type.Int, var Type.Int, var Type.Int))

  let ultosc =
    tacaml (App3 (Fun I.ultosc, var Type.Int, var Type.Int, var Type.Int))

  let stoch_slow_k =
    tacaml (App3 (Fun I.stoch_slow_k, var Type.Int, var Type.Int, var Type.Int))

  let stoch_slow_d =
    tacaml (App3 (Fun I.stoch_slow_d, var Type.Int, var Type.Int, var Type.Int))

  let stoch_rsi_fast_k =
    tacaml
      (App3 (Fun I.stoch_rsi_fast_k, var Type.Int, var Type.Int, var Type.Int))

  let stoch_rsi_fast_d =
    tacaml
      (App3 (Fun I.stoch_rsi_fast_d, var Type.Int, var Type.Int, var Type.Int))

  (* Zero argument indicators *)
  let ht_trendline = tacaml (Const I.ht_trendline)
  let ad = tacaml (Const I.ad)
  let obv = tacaml (Const I.obv)
  let trange = tacaml (Const I.trange)
  let avg_price = tacaml (Const I.avg_price)
  let med_price = tacaml (Const I.med_price)
  let typ_price = tacaml (Const I.typ_price)
  let wcl_price = tacaml (Const I.wcl_price)
  let ht_dc_period = tacaml (Const I.ht_dc_period)
  let ht_dc_phase = tacaml (Const I.ht_dc_phase)
  let ht_phasor_inphase = tacaml (Const I.ht_phasor_inphase)
  let ht_phasor_quadrature = tacaml (Const I.ht_phasor_quadrature)
  let ht_sine_sine = tacaml (Const I.ht_sine_sine)
  let ht_sine_leadsine = tacaml (Const I.ht_sine_leadsine)
  let acos = tacaml (Const I.acos)
  let asin = tacaml (Const I.asin)
  let atan = tacaml (Const I.atan)
  let ceil = tacaml (Const I.ceil)
  let cos = tacaml (Const I.cos)
  let cosh = tacaml (Const I.cosh)
  let exp = tacaml (Const I.exp)
  let floor = tacaml (Const I.floor)
  let ln = tacaml (Const I.ln)
  let log10 = tacaml (Const I.log10)
  let sin = tacaml (Const I.sin)
  let sinh = tacaml (Const I.sinh)
  let sqrt = tacaml (Const I.sqrt)
  let tan = tacaml (Const I.tan)
  let tanh = tacaml (Const I.tanh)
  let add = tacaml (Const I.add)
  let div = tacaml (Const I.div)
  let mult = tacaml (Const I.mult)
  let sub = tacaml (Const I.sub)
  let bop = tacaml (Const I.bop)

  (* Candlestick patterns (most are zero args, some have one arg) *)
  let ht_trend_mode = tacaml (Const I.ht_trend_mode)
  let cdl_2crows = tacaml (Const I.cdl_2crows)
  let cdl_3blackcrows = tacaml (Const I.cdl_3blackcrows)
  let cdl_3inside = tacaml (Const I.cdl_3inside)
  let cdl_3linestrike = tacaml (Const I.cdl_3linestrike)
  let cdl_3outside = tacaml (Const I.cdl_3outside)
  let cdl_3starsinsouth = tacaml (Const I.cdl_3starsinsouth)
  let cdl_3whitesoldiers = tacaml (Const I.cdl_3whitesoldiers)

  let cdl_abandonedbaby =
    tacaml (App1 (Fun I.cdl_abandonedbaby, var Type.Float))

  let cdl_advanceblock = tacaml (Const I.cdl_advanceblock)
  let cdl_belthold = tacaml (Const I.cdl_belthold)
  let cdl_breakaway = tacaml (Const I.cdl_breakaway)
  let cdl_closingmarubozu = tacaml (Const I.cdl_closingmarubozu)
  let cdl_concealbabyswall = tacaml (Const I.cdl_concealbabyswall)
  let cdl_counterattack = tacaml (Const I.cdl_counterattack)

  let cdl_darkcloudcover =
    tacaml (App1 (Fun I.cdl_darkcloudcover, var Type.Float))

  let cdl_doji = tacaml (Const I.cdl_doji)
  let cdl_dojistar = tacaml (Const I.cdl_dojistar)
  let cdl_dragonflydoji = tacaml (Const I.cdl_dragonflydoji)
  let cdl_engulfing = tacaml (Const I.cdl_engulfing)

  let cdl_eveningdojistar =
    tacaml (App1 (Fun I.cdl_eveningdojistar, var Type.Float))

  let cdl_eveningstar = tacaml (App1 (Fun I.cdl_eveningstar, var Type.Float))
  let cdl_gap_side_side_white = tacaml (Const I.cdl_gap_side_side_white)
  let cdl_gravestonedoji = tacaml (Const I.cdl_gravestonedoji)
  let cdl_hammer = tacaml (Const I.cdl_hammer)
  let cdl_hangingman = tacaml (Const I.cdl_hangingman)
  let cdl_harami = tacaml (Const I.cdl_harami)
  let cdl_haramicross = tacaml (Const I.cdl_haramicross)
  let cdl_highwave = tacaml (Const I.cdl_highwave)
  let cdl_hikkake = tacaml (Const I.cdl_hikkake)
  let cdl_hikkakemod = tacaml (Const I.cdl_hikkakemod)
  let cdl_homingpigeon = tacaml (Const I.cdl_homingpigeon)
  let cdl_identical3crows = tacaml (Const I.cdl_identical3crows)
  let cdl_inneck = tacaml (Const I.cdl_inneck)
  let cdl_invertedhammer = tacaml (Const I.cdl_invertedhammer)
  let cdl_kicking = tacaml (Const I.cdl_kicking)
  let cdl_kickingbylength = tacaml (Const I.cdl_kickingbylength)
  let cdl_ladderbottom = tacaml (Const I.cdl_ladderbottom)
  let cdl_longleggedDoji = tacaml (Const I.cdl_longleggedDoji)
  let cdl_longline = tacaml (Const I.cdl_longline)
  let cdl_marubozu = tacaml (Const I.cdl_marubozu)
  let cdl_matchinglow = tacaml (Const I.cdl_matchinglow)
  let cdl_mathold = tacaml (App1 (Fun I.cdl_mathold, var Type.Float))

  let cdl_morningdojistar =
    tacaml (App1 (Fun I.cdl_morningdojistar, var Type.Float))

  let cdl_morningstar = tacaml (App1 (Fun I.cdl_morningstar, var Type.Float))
  let cdl_onneck = tacaml (Const I.cdl_onneck)
  let cdl_piercing = tacaml (Const I.cdl_piercing)
  let cdl_rickshawman = tacaml (Const I.cdl_rickshawman)
  let cdl_risefall3methods = tacaml (Const I.cdl_risefall3methods)
  let cdl_separatinglines = tacaml (Const I.cdl_separatinglines)
  let cdl_shootingstar = tacaml (Const I.cdl_shootingstar)
  let cdl_shortline = tacaml (Const I.cdl_shortline)
  let cdl_spinningtop = tacaml (Const I.cdl_spinningtop)
  let cdl_stalledpattern = tacaml (Const I.cdl_stalledpattern)
  let cdl_sticksandwich = tacaml (Const I.cdl_sticksandwich)
  let cdl_takuri = tacaml (Const I.cdl_takuri)
  let cdl_tasukigap = tacaml (Const I.cdl_tasukigap)
  let cdl_thrusting = tacaml (Const I.cdl_thrusting)
  let cdl_tristar = tacaml (Const I.cdl_tristar)
  let cdl_unique3river = tacaml (Const I.cdl_unique3river)
  let cdl_upsidegap2crows = tacaml (Const I.cdl_upsidegap2crows)
  let cdl_xsidegap3methods = tacaml (Const I.cdl_xsidegap3methods)
  let max_index = tacaml (App1 (Fun I.max_index, var Type.Int))
  let min_index = tacaml (App1 (Fun I.min_index, var Type.Int))
  let min_max_index_min = tacaml (App1 (Fun I.min_max_index_min, var Type.Int))
  let min_max_index_max = tacaml (App1 (Fun I.min_max_index_max, var Type.Int))
end

module Constant = struct
  (* Single argument indicators *)
  let dema x = tacaml (App1 (Fun I.dema, Const x))
  let ema x = tacaml (App1 (Fun I.ema, Const x))
  let kama x = tacaml (App1 (Fun I.kama, Const x))
  let ma x = tacaml (App1 (Fun I.ma, Const x))
  let midpoint x = tacaml (App1 (Fun I.midpoint, Const x))
  let midprice x = tacaml (App1 (Fun I.midprice, Const x))
  let sma x = tacaml (App1 (Fun I.sma, Const x))
  let tema x = tacaml (App1 (Fun I.tema, Const x))
  let trima x = tacaml (App1 (Fun I.trima, Const x))
  let wma x = tacaml (App1 (Fun I.wma, Const x))
  let adx x = tacaml (App1 (Fun I.adx, Const x))
  let adxr x = tacaml (App1 (Fun I.adxr, Const x))
  let aroon_osc x = tacaml (App1 (Fun I.aroon_osc, Const x))
  let cci x = tacaml (App1 (Fun I.cci, Const x))
  let cmo x = tacaml (App1 (Fun I.cmo, Const x))
  let dx x = tacaml (App1 (Fun I.dx, Const x))
  let mfi x = tacaml (App1 (Fun I.mfi, Const x))
  let minus_di x = tacaml (App1 (Fun I.minus_di, Const x))
  let minus_dm x = tacaml (App1 (Fun I.minus_dm, Const x))
  let mom x = tacaml (App1 (Fun I.mom, Const x))
  let plus_di x = tacaml (App1 (Fun I.plus_di, Const x))
  let plus_dm x = tacaml (App1 (Fun I.plus_dm, Const x))
  let roc x = tacaml (App1 (Fun I.roc, Const x))
  let rocp x = tacaml (App1 (Fun I.rocp, Const x))
  let rocr x = tacaml (App1 (Fun I.rocr, Const x))
  let rocr100 x = tacaml (App1 (Fun I.rocr100, Const x))
  let rsi x = tacaml (App1 (Fun I.rsi, Const x))
  let trix x = tacaml (App1 (Fun I.trix, Const x))
  let willr x = tacaml (App1 (Fun I.willr, Const x))
  let atr x = tacaml (App1 (Fun I.atr, Const x))
  let natr x = tacaml (App1 (Fun I.natr, Const x))
  let aroon_down x = tacaml (App1 (Fun I.aroon_down, Const x))
  let aroon_up x = tacaml (App1 (Fun I.aroon_up, Const x))
  let beta x = tacaml (App1 (Fun I.beta, Const x))
  let correl x = tacaml (App1 (Fun I.correl, Const x))
  let linearreg x = tacaml (App1 (Fun I.linearreg, Const x))
  let linearreg_angle x = tacaml (App1 (Fun I.linearreg_angle, Const x))
  let linearreg_intercept x = tacaml (App1 (Fun I.linearreg_intercept, Const x))
  let linearreg_slope x = tacaml (App1 (Fun I.linearreg_slope, Const x))
  let min_max_min x = tacaml (App1 (Fun I.min_max_min, Const x))
  let min_max_max x = tacaml (App1 (Fun I.min_max_max, Const x))
  let tsf x = tacaml (App1 (Fun I.tsf, Const x))
  let max x = tacaml (App1 (Fun I.max, Const x))
  let min x = tacaml (App1 (Fun I.min, Const x))
  let sum x = tacaml (App1 (Fun I.sum, Const x))
  let avgdev x = tacaml (App1 (Fun I.avgdev, Const x))
  let imi x = tacaml (App1 (Fun I.imi, Const x))
  let accbands_upper x = tacaml (App1 (Fun I.accbands_upper, Const x))
  let accbands_middle x = tacaml (App1 (Fun I.accbands_middle, Const x))
  let accbands_lower x = tacaml (App1 (Fun I.accbands_lower, Const x))

  (* Two argument indicators *)
  let mama x y = tacaml (App2 (Fun I.mama, Const x, Const y))
  let mavp x y = tacaml (App2 (Fun I.mavp, Const x, Const y))
  let sar x y = tacaml (App2 (Fun I.sar, Const x, Const y))
  let t3 x y = tacaml (App2 (Fun I.t3, Const x, Const y))
  let apo x y = tacaml (App2 (Fun I.apo, Const x, Const y))
  let ppo x y = tacaml (App2 (Fun I.ppo, Const x, Const y))
  let adosc x y = tacaml (App2 (Fun I.adosc, Const x, Const y))
  let stddev x y = tacaml (App2 (Fun I.stddev, Const x, Const y))
  let var_indicator x y = tacaml (App2 (Fun I.var, Const x, Const y))
  let mama_mama x y = tacaml (App2 (Fun I.mama_mama, Const x, Const y))
  let mama_fama x y = tacaml (App2 (Fun I.mama_fama, Const x, Const y))

  let stoch_f_fast_k x y =
    tacaml (App2 (Fun I.stoch_f_fast_k, Const x, Const y))

  let stoch_f_fast_d x y =
    tacaml (App2 (Fun I.stoch_f_fast_d, Const x, Const y))

  (* Three argument indicators *)
  let upper_bband x y z =
    tacaml (App3 (Fun I.upper_bband, Const x, Const y, Const z))

  let middle_bband x y z =
    tacaml (App3 (Fun I.middle_bband, Const x, Const y, Const z))

  let lower_bband x y z =
    tacaml (App3 (Fun I.lower_bband, Const x, Const y, Const z))

  let macd_macd x y z =
    tacaml (App3 (Fun I.macd_macd, Const x, Const y, Const z))

  let macd_signal x y z =
    tacaml (App3 (Fun I.macd_signal, Const x, Const y, Const z))

  let macd_hist x y z =
    tacaml (App3 (Fun I.macd_hist, Const x, Const y, Const z))

  let macd_ext_macd x y z =
    tacaml (App3 (Fun I.macd_ext_macd, Const x, Const y, Const z))

  let macd_ext_signal x y z =
    tacaml (App3 (Fun I.macd_ext_signal, Const x, Const y, Const z))

  let macd_ext_hist x y z =
    tacaml (App3 (Fun I.macd_ext_hist, Const x, Const y, Const z))

  let ultosc x y z = tacaml (App3 (Fun I.ultosc, Const x, Const y, Const z))

  let stoch_slow_k x y z =
    tacaml (App3 (Fun I.stoch_slow_k, Const x, Const y, Const z))

  let stoch_slow_d x y z =
    tacaml (App3 (Fun I.stoch_slow_d, Const x, Const y, Const z))

  let stoch_rsi_fast_k x y z =
    tacaml (App3 (Fun I.stoch_rsi_fast_k, Const x, Const y, Const z))

  let stoch_rsi_fast_d x y z =
    tacaml (App3 (Fun I.stoch_rsi_fast_d, Const x, Const y, Const z))

  (* Zero argument indicators *)
  let ht_trendline = tacaml (Const I.ht_trendline)
  let ad = tacaml (Const I.ad)
  let obv = tacaml (Const I.obv)
  let trange = tacaml (Const I.trange)
  let avg_price = tacaml (Const I.avg_price)
  let med_price = tacaml (Const I.med_price)
  let typ_price = tacaml (Const I.typ_price)
  let wcl_price = tacaml (Const I.wcl_price)
  let ht_dc_period = tacaml (Const I.ht_dc_period)
  let ht_dc_phase = tacaml (Const I.ht_dc_phase)
  let ht_phasor_inphase = tacaml (Const I.ht_phasor_inphase)
  let ht_phasor_quadrature = tacaml (Const I.ht_phasor_quadrature)
  let ht_sine_sine = tacaml (Const I.ht_sine_sine)
  let ht_sine_leadsine = tacaml (Const I.ht_sine_leadsine)
  let acos = tacaml (Const I.acos)
  let asin = tacaml (Const I.asin)
  let atan = tacaml (Const I.atan)
  let ceil = tacaml (Const I.ceil)
  let cos = tacaml (Const I.cos)
  let cosh = tacaml (Const I.cosh)
  let exp = tacaml (Const I.exp)
  let floor = tacaml (Const I.floor)
  let ln = tacaml (Const I.ln)
  let log10 = tacaml (Const I.log10)
  let sin = tacaml (Const I.sin)
  let sinh = tacaml (Const I.sinh)
  let sqrt = tacaml (Const I.sqrt)
  let tan = tacaml (Const I.tan)
  let tanh = tacaml (Const I.tanh)
  let add = tacaml (Const I.add)
  let div = tacaml (Const I.div)
  let mult = tacaml (Const I.mult)
  let sub = tacaml (Const I.sub)
  let bop = tacaml (Const I.bop)

  (* Candlestick patterns (most are zero args, some have one arg) *)
  let ht_trend_mode = tacaml (Const I.ht_trend_mode)
  let cdl_2crows = tacaml (Const I.cdl_2crows)
  let cdl_3blackcrows = tacaml (Const I.cdl_3blackcrows)
  let cdl_3inside = tacaml (Const I.cdl_3inside)
  let cdl_3linestrike = tacaml (Const I.cdl_3linestrike)
  let cdl_3outside = tacaml (Const I.cdl_3outside)
  let cdl_3starsinsouth = tacaml (Const I.cdl_3starsinsouth)
  let cdl_3whitesoldiers = tacaml (Const I.cdl_3whitesoldiers)
  let cdl_abandonedbaby x = tacaml (App1 (Fun I.cdl_abandonedbaby, Const x))
  let cdl_advanceblock = tacaml (Const I.cdl_advanceblock)
  let cdl_belthold = tacaml (Const I.cdl_belthold)
  let cdl_breakaway = tacaml (Const I.cdl_breakaway)
  let cdl_closingmarubozu = tacaml (Const I.cdl_closingmarubozu)
  let cdl_concealbabyswall = tacaml (Const I.cdl_concealbabyswall)
  let cdl_counterattack = tacaml (Const I.cdl_counterattack)
  let cdl_darkcloudcover x = tacaml (App1 (Fun I.cdl_darkcloudcover, Const x))
  let cdl_doji = tacaml (Const I.cdl_doji)
  let cdl_dojistar = tacaml (Const I.cdl_dojistar)
  let cdl_dragonflydoji = tacaml (Const I.cdl_dragonflydoji)
  let cdl_engulfing = tacaml (Const I.cdl_engulfing)
  let cdl_eveningdojistar x = tacaml (App1 (Fun I.cdl_eveningdojistar, Const x))
  let cdl_eveningstar x = tacaml (App1 (Fun I.cdl_eveningstar, Const x))
  let cdl_gap_side_side_white = tacaml (Const I.cdl_gap_side_side_white)
  let cdl_gravestonedoji = tacaml (Const I.cdl_gravestonedoji)
  let cdl_hammer = tacaml (Const I.cdl_hammer)
  let cdl_hangingman = tacaml (Const I.cdl_hangingman)
  let cdl_harami = tacaml (Const I.cdl_harami)
  let cdl_haramicross = tacaml (Const I.cdl_haramicross)
  let cdl_highwave = tacaml (Const I.cdl_highwave)
  let cdl_hikkake = tacaml (Const I.cdl_hikkake)
  let cdl_hikkakemod = tacaml (Const I.cdl_hikkakemod)
  let cdl_homingpigeon = tacaml (Const I.cdl_homingpigeon)
  let cdl_identical3crows = tacaml (Const I.cdl_identical3crows)
  let cdl_inneck = tacaml (Const I.cdl_inneck)
  let cdl_invertedhammer = tacaml (Const I.cdl_invertedhammer)
  let cdl_kicking = tacaml (Const I.cdl_kicking)
  let cdl_kickingbylength = tacaml (Const I.cdl_kickingbylength)
  let cdl_ladderbottom = tacaml (Const I.cdl_ladderbottom)
  let cdl_longleggedDoji = tacaml (Const I.cdl_longleggedDoji)
  let cdl_longline = tacaml (Const I.cdl_longline)
  let cdl_marubozu = tacaml (Const I.cdl_marubozu)
  let cdl_matchinglow = tacaml (Const I.cdl_matchinglow)
  let cdl_mathold x = tacaml (App1 (Fun I.cdl_mathold, Const x))
  let cdl_morningdojistar x = tacaml (App1 (Fun I.cdl_morningdojistar, Const x))
  let cdl_morningstar x = tacaml (App1 (Fun I.cdl_morningstar, Const x))
  let cdl_onneck = tacaml (Const I.cdl_onneck)
  let cdl_piercing = tacaml (Const I.cdl_piercing)
  let cdl_rickshawman = tacaml (Const I.cdl_rickshawman)
  let cdl_risefall3methods = tacaml (Const I.cdl_risefall3methods)
  let cdl_separatinglines = tacaml (Const I.cdl_separatinglines)
  let cdl_shootingstar = tacaml (Const I.cdl_shootingstar)
  let cdl_shortline = tacaml (Const I.cdl_shortline)
  let cdl_spinningtop = tacaml (Const I.cdl_spinningtop)
  let cdl_stalledpattern = tacaml (Const I.cdl_stalledpattern)
  let cdl_sticksandwich = tacaml (Const I.cdl_sticksandwich)
  let cdl_takuri = tacaml (Const I.cdl_takuri)
  let cdl_tasukigap = tacaml (Const I.cdl_tasukigap)
  let cdl_thrusting = tacaml (Const I.cdl_thrusting)
  let cdl_tristar = tacaml (Const I.cdl_tristar)
  let cdl_unique3river = tacaml (Const I.cdl_unique3river)
  let cdl_upsidegap2crows = tacaml (Const I.cdl_upsidegap2crows)
  let cdl_xsidegap3methods = tacaml (Const I.cdl_xsidegap3methods)
  let max_index x = tacaml (App1 (Fun I.max_index, Const x))
  let min_index x = tacaml (App1 (Fun I.min_index, Const x))
  let min_max_index_min x = tacaml (App1 (Fun I.min_max_index_min, Const x))
  let min_max_index_max x = tacaml (App1 (Fun I.min_max_index_max, Const x))
end
