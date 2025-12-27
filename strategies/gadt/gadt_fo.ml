open Gadt
module I = Tacaml.Indicator.Raw

(* Create a variable with optional bounds for optimizer
   Default to [5.0, 100.0] to match current NLopt behavior *)
let var ?(lower = 5.0) ?(upper = 100.0) ty =
  Var (Uuidm.v4_gen Util.random_state (), ty, { lower; upper })

let tacaml x () = Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x), x))

module Variable = struct
  (* Single argument indicators with optional bounds *)
  let dema ?lower ?upper () =
    tacaml (App1 (Fun ("I.dema", I.dema), var ?lower ?upper Type.Int)) ()

  let ema ?lower ?upper () =
    tacaml (App1 (Fun ("I.ema", I.ema), var ?lower ?upper Type.Int)) ()

  let kama ?lower ?upper () =
    tacaml (App1 (Fun ("I.kama", I.kama), var ?lower ?upper Type.Int)) ()

  let ma ?lower ?upper () =
    tacaml (App1 (Fun ("I.ma", I.ma), var ?lower ?upper Type.Int)) ()

  let midpoint ?lower ?upper () =
    tacaml (App1 (Fun ("I.midpoint", I.midpoint), var ?lower ?upper Type.Int)) ()

  let midprice ?lower ?upper () =
    tacaml (App1 (Fun ("I.midprice", I.midprice), var ?lower ?upper Type.Int)) ()

  let sma ?lower ?upper () =
    tacaml (App1 (Fun ("I.sma", I.sma), var ?lower ?upper Type.Int)) ()

  let tema ?lower ?upper () =
    tacaml (App1 (Fun ("I.tema", I.tema), var ?lower ?upper Type.Int)) ()

  let trima ?lower ?upper () =
    tacaml (App1 (Fun ("I.trima", I.trima), var ?lower ?upper Type.Int)) ()

  let wma ?lower ?upper () =
    tacaml (App1 (Fun ("I.wma", I.wma), var ?lower ?upper Type.Int)) ()

  let adx ?lower ?upper () =
    tacaml (App1 (Fun ("I.adx", I.adx), var ?lower ?upper Type.Int)) ()

  let adxr ?lower ?upper () =
    tacaml (App1 (Fun ("I.adxr", I.adxr), var ?lower ?upper Type.Int)) ()

  let aroon_osc ?lower ?upper () =
    tacaml (App1 (Fun ("I.aroon_osc", I.aroon_osc), var ?lower ?upper Type.Int)) ()

  let cci ?lower ?upper () =
    tacaml (App1 (Fun ("I.cci", I.cci), var ?lower ?upper Type.Int)) ()

  let cmo ?lower ?upper () =
    tacaml (App1 (Fun ("I.cmo", I.cmo), var ?lower ?upper Type.Int)) ()

  let dx ?lower ?upper () =
    tacaml (App1 (Fun ("I.dx", I.dx), var ?lower ?upper Type.Int)) ()

  let mfi ?lower ?upper () =
    tacaml (App1 (Fun ("I.mfi", I.mfi), var ?lower ?upper Type.Int)) ()

  let minus_di ?lower ?upper () =
    tacaml (App1 (Fun ("I.minus_di", I.minus_di), var ?lower ?upper Type.Int)) ()

  let minus_dm ?lower ?upper () =
    tacaml (App1 (Fun ("I.minus_dm", I.minus_dm), var ?lower ?upper Type.Int)) ()

  let mom ?lower ?upper () =
    tacaml (App1 (Fun ("I.mom", I.mom), var ?lower ?upper Type.Int)) ()

  let plus_di ?lower ?upper () =
    tacaml (App1 (Fun ("I.plus_di", I.plus_di), var ?lower ?upper Type.Int)) ()

  let plus_dm ?lower ?upper () =
    tacaml (App1 (Fun ("I.plus_dm", I.plus_dm), var ?lower ?upper Type.Int)) ()

  let roc ?lower ?upper () =
    tacaml (App1 (Fun ("I.roc", I.roc), var ?lower ?upper Type.Int)) ()

  let rocp ?lower ?upper () =
    tacaml (App1 (Fun ("I.rocp", I.rocp), var ?lower ?upper Type.Int)) ()

  let rocr ?lower ?upper () =
    tacaml (App1 (Fun ("I.rocr", I.rocr), var ?lower ?upper Type.Int)) ()

  let rocr100 ?lower ?upper () =
    tacaml (App1 (Fun ("I.rocr100", I.rocr100), var ?lower ?upper Type.Int)) ()

  let rsi ?lower ?upper () =
    tacaml (App1 (Fun ("I.rsi", I.rsi), var ?lower ?upper Type.Int)) ()

  let trix ?lower ?upper () =
    tacaml (App1 (Fun ("I.trix", I.trix), var ?lower ?upper Type.Int)) ()

  let willr ?lower ?upper () =
    tacaml (App1 (Fun ("I.willr", I.willr), var ?lower ?upper Type.Int)) ()

  let atr ?lower ?upper () =
    tacaml (App1 (Fun ("I.atr", I.atr), var ?lower ?upper Type.Int)) ()

  let natr ?lower ?upper () =
    tacaml (App1 (Fun ("I.natr", I.natr), var ?lower ?upper Type.Int)) ()

  let aroon_down ?lower ?upper () =
    tacaml
      (App1 (Fun ("I.aroon_down", I.aroon_down), var ?lower ?upper Type.Int)) ()

  let aroon_up ?lower ?upper () =
    tacaml (App1 (Fun ("I.aroon_up", I.aroon_up), var ?lower ?upper Type.Int)) ()

  let beta ?lower ?upper () =
    tacaml (App1 (Fun ("I.beta", I.beta), var ?lower ?upper Type.Int)) ()

  let correl ?lower ?upper () =
    tacaml (App1 (Fun ("I.correl", I.correl), var ?lower ?upper Type.Int)) ()

  let linearreg ?lower ?upper () =
    tacaml (App1 (Fun ("I.linearreg", I.linearreg), var ?lower ?upper Type.Int)) ()

  let linearreg_angle ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.linearreg_angle", I.linearreg_angle), var ?lower ?upper Type.Int)) ()

  let linearreg_intercept ?lower ?upper () =
    tacaml
      (App1
         ( Fun ("I.linearreg_intercept", I.linearreg_intercept),
           var ?lower ?upper Type.Int )) ()

  let linearreg_slope ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.linearreg_slope", I.linearreg_slope), var ?lower ?upper Type.Int)) ()

  let min_max_min ?lower ?upper () =
    tacaml
      (App1 (Fun ("I.min_max_min", I.min_max_min), var ?lower ?upper Type.Int)) ()

  let min_max_max ?lower ?upper () =
    tacaml
      (App1 (Fun ("I.min_max_max", I.min_max_max), var ?lower ?upper Type.Int)) ()

  let tsf ?lower ?upper () =
    tacaml (App1 (Fun ("I.tsf", I.tsf), var ?lower ?upper Type.Int)) ()

  let max ?lower ?upper () =
    tacaml (App1 (Fun ("I.max", I.max), var ?lower ?upper Type.Int)) ()

  let min ?lower ?upper () =
    tacaml (App1 (Fun ("I.min", I.min), var ?lower ?upper Type.Int)) ()

  let sum ?lower ?upper () =
    tacaml (App1 (Fun ("I.sum", I.sum), var ?lower ?upper Type.Int)) ()

  let avgdev ?lower ?upper () =
    tacaml (App1 (Fun ("I.avgdev", I.avgdev), var ?lower ?upper Type.Int)) ()

  let imi ?lower ?upper () =
    tacaml (App1 (Fun ("I.imi", I.imi), var ?lower ?upper Type.Int)) ()

  let accbands_upper ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.accbands_upper", I.accbands_upper), var ?lower ?upper Type.Int)) ()

  let accbands_middle ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.accbands_middle", I.accbands_middle), var ?lower ?upper Type.Int)) ()

  let accbands_lower ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.accbands_lower", I.accbands_lower), var ?lower ?upper Type.Int)) ()

  (* Two argument indicators *)
  let mama ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.mama", I.mama),
           var ?lower:lower1 ?upper:upper1 Type.Float,
           var ?lower:lower2 ?upper:upper2 Type.Float )) ()

  let mavp ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.mavp", I.mavp),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int )) ()

  let sar ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.sar", I.sar),
           var ?lower:lower1 ?upper:upper1 Type.Float,
           var ?lower:lower2 ?upper:upper2 Type.Float )) ()

  let t3 ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.t3", I.t3),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Float )) ()

  let apo ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.apo", I.apo),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int )) ()

  let ppo ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.ppo", I.ppo),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int )) ()

  let adosc ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.adosc", I.adosc),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int )) ()

  let stddev ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.stddev", I.stddev),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Float )) ()

  let var_indicator ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.var", I.var),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Float )) ()

  let mama_mama ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.mama_mama", I.mama_mama),
           var ?lower:lower1 ?upper:upper1 Type.Float,
           var ?lower:lower2 ?upper:upper2 Type.Float )) ()

  let mama_fama ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.mama_fama", I.mama_fama),
           var ?lower:lower1 ?upper:upper1 Type.Float,
           var ?lower:lower2 ?upper:upper2 Type.Float )) ()

  let stoch_f_fast_k ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.stoch_f_fast_k", I.stoch_f_fast_k),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int )) ()

  let stoch_f_fast_d ?lower1 ?upper1 ?lower2 ?upper2 () =
    tacaml
      (App2
         ( Fun ("I.stoch_f_fast_d", I.stoch_f_fast_d),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int )) ()

  (* Three argument indicators *)
  let upper_bband ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.upper_bband", I.upper_bband),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Float,
           var ?lower:lower3 ?upper:upper3 Type.Float )) ()

  let middle_bband ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.middle_bband", I.middle_bband),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Float,
           var ?lower:lower3 ?upper:upper3 Type.Float )) ()

  let lower_bband ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.lower_bband", I.lower_bband),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Float,
           var ?lower:lower3 ?upper:upper3 Type.Float )) ()

  let macd_macd ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.macd_macd", I.macd_macd),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let macd_signal ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.macd_signal", I.macd_signal),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let macd_hist ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.macd_hist", I.macd_hist),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let macd_ext_macd ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.macd_ext_macd", I.macd_ext_macd),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let macd_ext_signal ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.macd_ext_signal", I.macd_ext_signal),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let macd_ext_hist ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.macd_ext_hist", I.macd_ext_hist),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let ultosc ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.ultosc", I.ultosc),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let stoch_slow_k ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.stoch_slow_k", I.stoch_slow_k),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let stoch_slow_d ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.stoch_slow_d", I.stoch_slow_d),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let stoch_rsi_fast_k ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.stoch_rsi_fast_k", I.stoch_rsi_fast_k),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

  let stoch_rsi_fast_d ?lower1 ?upper1 ?lower2 ?upper2 ?lower3 ?upper3 () =
    tacaml
      (App3
         ( Fun ("I.stoch_rsi_fast_d", I.stoch_rsi_fast_d),
           var ?lower:lower1 ?upper:upper1 Type.Int,
           var ?lower:lower2 ?upper:upper2 Type.Int,
           var ?lower:lower3 ?upper:upper3 Type.Int )) ()

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

  (* Candlestick patterns (most are zero args, some have one arg) *)
  let ht_trend_mode = tacaml (Const (I.ht_trend_mode, Tacaml))
  let cdl_2crows = tacaml (Const (I.cdl_2crows, Tacaml))
  let cdl_3blackcrows = tacaml (Const (I.cdl_3blackcrows, Tacaml))
  let cdl_3inside = tacaml (Const (I.cdl_3inside, Tacaml))
  let cdl_3linestrike = tacaml (Const (I.cdl_3linestrike, Tacaml))
  let cdl_3outside = tacaml (Const (I.cdl_3outside, Tacaml))
  let cdl_3starsinsouth = tacaml (Const (I.cdl_3starsinsouth, Tacaml))
  let cdl_3whitesoldiers = tacaml (Const (I.cdl_3whitesoldiers, Tacaml))

  let cdl_abandonedbaby ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.cdl_abandonedbaby", I.cdl_abandonedbaby), var ?lower ?upper Type.Float)) ()

  let cdl_advanceblock = tacaml (Const (I.cdl_advanceblock, Tacaml))
  let cdl_belthold = tacaml (Const (I.cdl_belthold, Tacaml))
  let cdl_breakaway = tacaml (Const (I.cdl_breakaway, Tacaml))
  let cdl_closingmarubozu = tacaml (Const (I.cdl_closingmarubozu, Tacaml))
  let cdl_concealbabyswall = tacaml (Const (I.cdl_concealbabyswall, Tacaml))
  let cdl_counterattack = tacaml (Const (I.cdl_counterattack, Tacaml))

  let cdl_darkcloudcover ?lower ?upper () =
    tacaml
      (App1
         ( Fun ("I.cdl_darkcloudcover", I.cdl_darkcloudcover),
           var ?lower ?upper Type.Float )) ()

  let cdl_doji = tacaml (Const (I.cdl_doji, Tacaml))
  let cdl_dojistar = tacaml (Const (I.cdl_dojistar, Tacaml))
  let cdl_dragonflydoji = tacaml (Const (I.cdl_dragonflydoji, Tacaml))
  let cdl_engulfing = tacaml (Const (I.cdl_engulfing, Tacaml))

  let cdl_eveningdojistar ?lower ?upper () =
    tacaml
      (App1
         ( Fun ("I.cdl_eveningdojistar", I.cdl_eveningdojistar),
           var ?lower ?upper Type.Float )) ()

  let cdl_eveningstar ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.cdl_eveningstar", I.cdl_eveningstar), var ?lower ?upper Type.Float)) ()

  let cdl_gap_side_side_white =
    tacaml (Const (I.cdl_gap_side_side_white, Tacaml))

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

  let cdl_mathold ?lower ?upper () =
    tacaml
      (App1 (Fun ("I.cdl_mathold", I.cdl_mathold), var ?lower ?upper Type.Float)) ()

  let cdl_morningdojistar ?lower ?upper () =
    tacaml
      (App1
         ( Fun ("I.cdl_morningdojistar", I.cdl_morningdojistar),
           var ?lower ?upper Type.Float )) ()

  let cdl_morningstar ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.cdl_morningstar", I.cdl_morningstar), var ?lower ?upper Type.Float)) ()

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
  let max_index ?lower ?upper () =
    tacaml (App1 (Fun ("I.max_index", I.max_index), var ?lower ?upper Type.Int)) ()

  let min_index ?lower ?upper () =
    tacaml (App1 (Fun ("I.min_index", I.min_index), var ?lower ?upper Type.Int)) ()

  let min_max_index_min ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.min_max_index_min", I.min_max_index_min), var ?lower ?upper Type.Int)) ()

  let min_max_index_max ?lower ?upper () =
    tacaml
      (App1
         (Fun ("I.min_max_index_max", I.min_max_index_max), var ?lower ?upper Type.Int)) ()
end

module Constant = struct
  (* Single argument indicators *)
  let dema x = tacaml (App1 (Fun ("I.dema", I.dema), Const (x, Int)))
  let ema x = tacaml (App1 (Fun ("I.ema", I.ema), Const (x, Int)))
  let kama x = tacaml (App1 (Fun ("I.kama", I.kama), Const (x, Int)))
  let ma x = tacaml (App1 (Fun ("I.ma", I.ma), Const (x, Int)))

  let midpoint x =
    tacaml (App1 (Fun ("I.midpoint", I.midpoint), Const (x, Int)))

  let midprice x =
    tacaml (App1 (Fun ("I.midprice", I.midprice), Const (x, Int)))

  let sma x = tacaml (App1 (Fun ("I.sma", I.sma), Const (x, Int)))
  let tema x = tacaml (App1 (Fun ("I.tema", I.tema), Const (x, Int)))
  let trima x = tacaml (App1 (Fun ("I.trima", I.trima), Const (x, Int)))
  let wma x = tacaml (App1 (Fun ("I.wma", I.wma), Const (x, Int)))
  let adx x = tacaml (App1 (Fun ("I.adx", I.adx), Const (x, Int)))
  let adxr x = tacaml (App1 (Fun ("I.adxr", I.adxr), Const (x, Int)))

  let aroon_osc x =
    tacaml (App1 (Fun ("I.aroon_osc", I.aroon_osc), Const (x, Int)))

  let cci x = tacaml (App1 (Fun ("I.cci", I.cci), Const (x, Int)))
  let cmo x = tacaml (App1 (Fun ("I.cmo", I.cmo), Const (x, Int)))
  let dx x = tacaml (App1 (Fun ("I.dx", I.dx), Const (x, Int)))
  let mfi x = tacaml (App1 (Fun ("I.mfi", I.mfi), Const (x, Int)))

  let minus_di x =
    tacaml (App1 (Fun ("I.minus_di", I.minus_di), Const (x, Int)))

  let minus_dm x =
    tacaml (App1 (Fun ("I.minus_dm", I.minus_dm), Const (x, Int)))

  let mom x = tacaml (App1 (Fun ("I.mom", I.mom), Const (x, Int)))
  let plus_di x = tacaml (App1 (Fun ("I.plus_di", I.plus_di), Const (x, Int)))
  let plus_dm x = tacaml (App1 (Fun ("I.plus_dm", I.plus_dm), Const (x, Int)))
  let roc x = tacaml (App1 (Fun ("I.roc", I.roc), Const (x, Int)))
  let rocp x = tacaml (App1 (Fun ("I.rocp", I.rocp), Const (x, Int)))
  let rocr x = tacaml (App1 (Fun ("I.rocr", I.rocr), Const (x, Int)))
  let rocr100 x = tacaml (App1 (Fun ("I.rocr100", I.rocr100), Const (x, Int)))
  let rsi x = tacaml (App1 (Fun ("I.rsi", I.rsi), Const (x, Int)))
  let trix x = tacaml (App1 (Fun ("I.trix", I.trix), Const (x, Int)))
  let willr x = tacaml (App1 (Fun ("I.willr", I.willr), Const (x, Int)))
  let atr x = tacaml (App1 (Fun ("I.atr", I.atr), Const (x, Int)))
  let natr x = tacaml (App1 (Fun ("I.natr", I.natr), Const (x, Int)))

  let aroon_down x =
    tacaml (App1 (Fun ("I.aroon_down", I.aroon_down), Const (x, Int)))

  let aroon_up x =
    tacaml (App1 (Fun ("I.aroon_up", I.aroon_up), Const (x, Int)))

  let beta x = tacaml (App1 (Fun ("I.beta", I.beta), Const (x, Int)))
  let correl x = tacaml (App1 (Fun ("I.correl", I.correl), Const (x, Int)))

  let linearreg x =
    tacaml (App1 (Fun ("I.linearreg", I.linearreg), Const (x, Int)))

  let linearreg_angle x =
    tacaml (App1 (Fun ("I.linearreg_angle", I.linearreg_angle), Const (x, Int)))

  let linearreg_intercept x =
    tacaml
      (App1
         (Fun ("I.linearreg_intercept", I.linearreg_intercept), Const (x, Int)))

  let linearreg_slope x =
    tacaml (App1 (Fun ("I.linearreg_slope", I.linearreg_slope), Const (x, Int)))

  let min_max_min x =
    tacaml (App1 (Fun ("I.min_max_min", I.min_max_min), Const (x, Int)))

  let min_max_max x =
    tacaml (App1 (Fun ("I.min_max_max", I.min_max_max), Const (x, Int)))

  let tsf x = tacaml (App1 (Fun ("I.tsf", I.tsf), Const (x, Int)))
  let max x = tacaml (App1 (Fun ("I.max", I.max), Const (x, Int)))
  let min x = tacaml (App1 (Fun ("I.min", I.min), Const (x, Int)))
  let sum x = tacaml (App1 (Fun ("I.sum", I.sum), Const (x, Int)))
  let avgdev x = tacaml (App1 (Fun ("I.avgdev", I.avgdev), Const (x, Int)))
  let imi x = tacaml (App1 (Fun ("I.imi", I.imi), Const (x, Int)))

  let accbands_upper x =
    tacaml (App1 (Fun ("I.accbands_upper", I.accbands_upper), Const (x, Int)))

  let accbands_middle x =
    tacaml (App1 (Fun ("I.accbands_middle", I.accbands_middle), Const (x, Int)))

  let accbands_lower x =
    tacaml (App1 (Fun ("I.accbands_lower", I.accbands_lower), Const (x, Int)))

  (* Two argument indicators *)
  let mama x y =
    tacaml (App2 (Fun ("I.mama", I.mama), Const (x, Float), Const (y, Float)))

  let mavp x y =
    tacaml (App2 (Fun ("I.mavp", I.mavp), Const (x, Int), Const (y, Int)))

  let sar x y =
    tacaml (App2 (Fun ("I.sar", I.sar), Const (x, Float), Const (y, Float)))

  let t3 x y =
    tacaml (App2 (Fun ("I.t3", I.t3), Const (x, Int), Const (y, Float)))

  let apo x y =
    tacaml (App2 (Fun ("I.apo", I.apo), Const (x, Int), Const (y, Int)))

  let ppo x y =
    tacaml (App2 (Fun ("I.ppo", I.ppo), Const (x, Int), Const (y, Int)))

  let adosc x y =
    tacaml (App2 (Fun ("I.adosc", I.adosc), Const (x, Int), Const (y, Int)))

  let stddev x y =
    tacaml (App2 (Fun ("I.stddev", I.stddev), Const (x, Int), Const (y, Float)))

  let var_indicator x y =
    tacaml (App2 (Fun ("I.var", I.var), Const (x, Int), Const (y, Float)))

  let mama_mama x y =
    tacaml
      (App2
         (Fun ("I.mama_mama", I.mama_mama), Const (x, Float), Const (y, Float)))

  let mama_fama x y =
    tacaml
      (App2
         (Fun ("I.mama_fama", I.mama_fama), Const (x, Float), Const (y, Float)))

  let stoch_f_fast_k x y =
    tacaml
      (App2
         ( Fun ("I.stoch_f_fast_k", I.stoch_f_fast_k),
           Const (x, Int),
           Const (y, Int) ))

  let stoch_f_fast_d x y =
    tacaml
      (App2
         ( Fun ("I.stoch_f_fast_d", I.stoch_f_fast_d),
           Const (x, Int),
           Const (y, Int) ))

  (* Three argument indicators *)
  let upper_bband x y z =
    tacaml
      (App3
         ( Fun ("I.upper_bband", I.upper_bband),
           Const (x, Int),
           Const (y, Float),
           Const (z, Float) ))

  let middle_bband x y z =
    tacaml
      (App3
         ( Fun ("I.middle_bband", I.middle_bband),
           Const (x, Int),
           Const (y, Float),
           Const (z, Float) ))

  let lower_bband x y z =
    tacaml
      (App3
         ( Fun ("I.lower_bband", I.lower_bband),
           Const (x, Int),
           Const (y, Float),
           Const (z, Float) ))

  let macd_macd x y z =
    tacaml
      (App3
         ( Fun ("I.macd_macd", I.macd_macd),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let macd_signal x y z =
    tacaml
      (App3
         ( Fun ("I.macd_signal", I.macd_signal),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let macd_hist x y z =
    tacaml
      (App3
         ( Fun ("I.macd_hist", I.macd_hist),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let macd_ext_macd x y z =
    tacaml
      (App3
         ( Fun ("I.macd_ext_macd", I.macd_ext_macd),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let macd_ext_signal x y z =
    tacaml
      (App3
         ( Fun ("I.macd_ext_signal", I.macd_ext_signal),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let macd_ext_hist x y z =
    tacaml
      (App3
         ( Fun ("I.macd_ext_hist", I.macd_ext_hist),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let ultosc x y z =
    tacaml
      (App3
         ( Fun ("I.ultosc", I.ultosc),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let stoch_slow_k x y z =
    tacaml
      (App3
         ( Fun ("I.stoch_slow_k", I.stoch_slow_k),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let stoch_slow_d x y z =
    tacaml
      (App3
         ( Fun ("I.stoch_slow_d", I.stoch_slow_d),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let stoch_rsi_fast_k x y z =
    tacaml
      (App3
         ( Fun ("I.stoch_rsi_fast_k", I.stoch_rsi_fast_k),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

  let stoch_rsi_fast_d x y z =
    tacaml
      (App3
         ( Fun ("I.stoch_rsi_fast_d", I.stoch_rsi_fast_d),
           Const (x, Int),
           Const (y, Int),
           Const (z, Int) ))

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

  (* Candlestick patterns (most are zero args, some have one arg) *)
  let ht_trend_mode = tacaml (Const (I.ht_trend_mode, Tacaml))
  let cdl_2crows = tacaml (Const (I.cdl_2crows, Tacaml))
  let cdl_3blackcrows = tacaml (Const (I.cdl_3blackcrows, Tacaml))
  let cdl_3inside = tacaml (Const (I.cdl_3inside, Tacaml))
  let cdl_3linestrike = tacaml (Const (I.cdl_3linestrike, Tacaml))
  let cdl_3outside = tacaml (Const (I.cdl_3outside, Tacaml))
  let cdl_3starsinsouth = tacaml (Const (I.cdl_3starsinsouth, Tacaml))
  let cdl_3whitesoldiers = tacaml (Const (I.cdl_3whitesoldiers, Tacaml))

  let cdl_abandonedbaby x =
    tacaml
      (App1 (Fun ("I.cdl_abandonedbaby", I.cdl_abandonedbaby), Const (x, Float)))

  let cdl_advanceblock = tacaml (Const (I.cdl_advanceblock, Tacaml))
  let cdl_belthold = tacaml (Const (I.cdl_belthold, Tacaml))
  let cdl_breakaway = tacaml (Const (I.cdl_breakaway, Tacaml))
  let cdl_closingmarubozu = tacaml (Const (I.cdl_closingmarubozu, Tacaml))
  let cdl_concealbabyswall = tacaml (Const (I.cdl_concealbabyswall, Tacaml))
  let cdl_counterattack = tacaml (Const (I.cdl_counterattack, Tacaml))

  let cdl_darkcloudcover x =
    tacaml
      (App1
         (Fun ("I.cdl_darkcloudcover", I.cdl_darkcloudcover), Const (x, Float)))

  let cdl_doji = tacaml (Const (I.cdl_doji, Tacaml))
  let cdl_dojistar = tacaml (Const (I.cdl_dojistar, Tacaml))
  let cdl_dragonflydoji = tacaml (Const (I.cdl_dragonflydoji, Tacaml))
  let cdl_engulfing = tacaml (Const (I.cdl_engulfing, Tacaml))

  let cdl_eveningdojistar x =
    tacaml
      (App1
         (Fun ("I.cdl_eveningdojistar", I.cdl_eveningdojistar), Const (x, Float)))

  let cdl_eveningstar x =
    tacaml
      (App1 (Fun ("I.cdl_eveningstar", I.cdl_eveningstar), Const (x, Float)))

  let cdl_gap_side_side_white =
    tacaml (Const (I.cdl_gap_side_side_white, Tacaml))

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

  let cdl_mathold x =
    tacaml (App1 (Fun ("I.cdl_mathold", I.cdl_mathold), Const (x, Float)))

  let cdl_morningdojistar x =
    tacaml
      (App1
         (Fun ("I.cdl_morningdojistar", I.cdl_morningdojistar), Const (x, Float)))

  let cdl_morningstar x =
    tacaml
      (App1 (Fun ("I.cdl_morningstar", I.cdl_morningstar), Const (x, Float)))

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

  let max_index x =
    tacaml (App1 (Fun ("I.max_index", I.max_index), Const (x, Int)))

  let min_index x =
    tacaml (App1 (Fun ("I.min_index", I.min_index), Const (x, Int)))

  let min_max_index_min x =
    tacaml
      (App1 (Fun ("I.min_max_index_min", I.min_max_index_min), Const (x, Int)))

  let min_max_index_max x =
    tacaml
      (App1 (Fun ("I.min_max_index_max", I.min_max_index_max), Const (x, Int)))
end
