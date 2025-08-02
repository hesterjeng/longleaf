open Gadt

(* Golden Cross with Bullish Candlestick Confirmation *)
let golden_cross_candlestick =
  let ema_50 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:50 ())))
  in
  let ema_200 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:200 ())))
  in
  let hammer_pattern =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_hammer ())))
  in
  let piercing_pattern =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_piercing ())))
  in
  let engulfing_pattern =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_engulfing ())))
  in
  let rsi_14 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.rsi ~timeperiod:14 ())))
  in
  {
    name = "Golden Cross Candlestick";
    buy_trigger =
      (* Golden cross: 50 EMA crosses above 200 EMA *)
      cross_up ema_50 ema_200
      &&.
      (* Bullish candlestick confirmation *)
      (IntGT (hammer_pattern, Int 0)
      ||. IntGT (piercing_pattern, Int 0)
      ||. IntGT (engulfing_pattern, Int 0))
      &&.
      (* RSI not overbought *)
      (rsi_14 <. Float 70.0)
      &&.
      (* Volume confirmation *)
      (volume >. volume_sma *. Float 1.2);
    sell_trigger =
      (* Death cross: 50 EMA crosses below 200 EMA *)
      cross_down ema_50 ema_200
      ||.
      (* RSI overbought *)
      (rsi_14 >. Float 80.0)
      ||.
      (* Price falls significantly below 50 EMA *)
      (close <. ema_50 *. Float 0.95);
    max_positions = 3;
    position_size = 0.33;
  }

(* MACD Crossover with Doji Reversal *)
let macd_doji_reversal =
  let macd_line =
    Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.macd_macd ())))
  in
  let macd_signal_line =
    Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.macd_signal ())))
  in
  let doji_pattern =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_doji ())))
  in
  let dragonfly_doji =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_dragonflydoji ())))
  in
  let morning_star =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_morningstar ())))
  in
  let bb_lower =
    Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.lower_bband ())))
  in
  {
    name = "MACD Doji Reversal";
    buy_trigger =
      (* MACD bullish crossover *)
      cross_up macd_line macd_signal_line
      &&.
      (* Doji reversal patterns near support *)
      (IntGT (doji_pattern, Int 0)
      ||. IntGT (dragonfly_doji, Int 0)
      ||. IntGT (morning_star, Int 0))
      &&.
      (* Price near lower Bollinger Band (oversold) *)
      (close <. bb_lower *. Float 1.05)
      &&.
      (* MACD below zero (coming from oversold) *)
      (macd_line <. Float 0.02);
    sell_trigger =
      (* MACD bearish crossover *)
      cross_down macd_line macd_signal_line
      ||.
      (* Price above upper Bollinger Band *)
      (close >. bb_upper)
      ||.
      (* Strong profit target *)
      (close >. sma *. Float 1.08);
    max_positions = 4;
    position_size = 0.25;
  }

(* Stochastic Crossover with Three White Soldiers *)
let stochastic_three_soldiers =
  let stoch_k =
    Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.stoch_slow_k ())))
  in
  let stoch_d =
    Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.stoch_slow_d ())))
  in
  let three_white_soldiers =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_3whitesoldiers ())))
  in
  let three_black_crows =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_3blackcrows ())))
  in
  let evening_star =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_eveningstar ())))
  in
  let atr_14 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.atr ~timeperiod:14 ())))
  in
  {
    name = "Stochastic Three Soldiers";
    buy_trigger =
      (* Stochastic bullish crossover from oversold *)
      cross_up stoch_k stoch_d
      &&.
      (* Stochastic was oversold *)
      (lag stoch_k 1 <. Float 30.0)
      &&.
      (* Three white soldiers pattern *)
      IntGT (three_white_soldiers, Int 0)
      &&.
      (* Above short-term average *)
      (close >. sma)
      &&.
      (* Reasonable volatility *)
      (atr_14 >. close *. Float 0.01);
    sell_trigger =
      (* Bearish patterns *)
      IntLT (three_black_crows, Int 0)
      ||. IntLT (evening_star, Int 0)
      ||.
      (* Stochastic overbought crossover *)
      (cross_down stoch_k stoch_d &&. (stoch_k >. Float 70.0))
      ||.
      (* Stop loss *)
      (close <. sma *. Float 0.93);
    max_positions = 5;
    position_size = 0.2;
  }

(* RSI Divergence with Hanging Man *)
let rsi_hanging_man =
  let rsi_21 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.rsi ~timeperiod:21 ())))
  in
  let hanging_man =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_hangingman ())))
  in
  let shooting_star =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_shootingstar ())))
  in
  let inverted_hammer =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_invertedhammer ())))
  in
  let dark_cloud =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_darkcloudcover ())))
  in
  let ema_20 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:20 ())))
  in
  let ema_50 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:50 ())))
  in
  {
    name = "RSI Hanging Man Reversal";
    buy_trigger =
      (* RSI oversold but turning up *)
      rsi_21 <. Float 35.0
      &&. (rsi_21 >. lag rsi_21 1)
      &&.
      (* Bullish hammer patterns *)
      (IntGT (inverted_hammer, Int 0) ||. IntGT (hammer, Int 0))
      &&.
      (* Uptrend context (20 EMA above 50 EMA) *)
      (ema_20 >. ema_50)
      &&.
      (* Price not too far from moving average *)
      (close >. ema_20 *. Float 0.95);
    sell_trigger =
      (* Bearish reversal patterns *)
      IntLT (hanging_man, Int 0)
      ||. IntLT (shooting_star, Int 0)
      ||. IntLT (dark_cloud, Int 0)
      ||.
      (* RSI overbought *)
      (rsi_21 >. Float 75.0)
      ||.
      (* EMA crossover breakdown *)
      cross_down ema_20 ema_50;
    max_positions = 4;
    position_size = 0.25;
  }

(* Williams %R with Engulfing Patterns *)
let williams_engulfing =
  let williams_r =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.willr ~timeperiod:14 ())))
  in
  let bullish_engulfing =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_engulfing ())))
  in
  let belt_hold =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_belthold ())))
  in
  let harami =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_harami ())))
  in
  let adx_14 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.adx ~timeperiod:14 ())))
  in
  let mom_10 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.mom ~timeperiod:10 ())))
  in
  {
    name = "Williams R Engulfing";
    buy_trigger =
      (* Williams %R oversold turning up *)
      williams_r <. Float (-80.0)
      &&. (williams_r >. lag williams_r 1)
      &&.
      (* Bullish engulfing patterns *)
      (IntGT (bullish_engulfing, Int 0) ||. IntGT (belt_hold, Int 0))
      &&.
      (* Trend strength *)
      (adx_14 >. Float 20.0)
      &&.
      (* Momentum turning positive *)
      (mom_10 >. lag mom_10 1)
      &&.
      (* Volume confirmation *)
      (volume >. volume_sma *. Float 1.4);
    sell_trigger =
      (* Williams %R overbought *)
      williams_r >. Float (-20.0)
      ||.
      (* Bearish harami pattern *)
      IntLT (harami, Int 0)
      ||.
      (* Momentum turning negative *)
      (mom_10 <. lag mom_10 1 &&. (mom_10 <. Float 0.0))
      ||.
      (* Weak trend *)
      (adx_14 <. Float 15.0);
    max_positions = 6;
    position_size = 0.16;
  }

(* CCI Crossover with Morning/Evening Stars *)
let cci_star_patterns =
  let cci_20 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.cci ~timeperiod:20 ())))
  in
  let morning_star =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_morningstar ())))
  in
  let evening_star =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_eveningstar ())))
  in
  let abandoned_baby =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_abandonedbaby ())))
  in
  let bb_middle =
    Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.middle_bband ())))
  in
  let roc_10 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.roc ~timeperiod:10 ())))
  in
  {
    name = "CCI Star Patterns";
    buy_trigger =
      (* CCI oversold and turning up *)
      cci_20 <. Float (-150.0)
      &&. (cci_20 >. lag cci_20 1)
      &&.
      (* Morning star or abandoned baby patterns *)
      (IntGT (morning_star, Int 0) ||. IntGT (abandoned_baby, Int 0))
      &&.
      (* Price above middle Bollinger Band *)
      (close >. bb_middle)
      &&.
      (* Rate of change turning positive *)
      (roc_10 >. lag roc_10 1);
    sell_trigger =
      (* CCI overbought *)
      cci_20 >. Float 150.0
      ||.
      (* Evening star pattern *)
      IntLT (evening_star, Int 0)
      ||.
      (* Price below middle Bollinger Band *)
      (close <. bb_middle *. Float 0.98)
      ||.
      (* Rate of change turning negative *)
      (roc_10 <. Float (-2.0));
    max_positions = 5;
    position_size = 0.2;
  }

(* Moving Average Ribbon with Harami Cross *)
let ma_ribbon_harami =
  let ema_8 =
    Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:8 ())))
  in
  let ema_13 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:13 ())))
  in
  let ema_21 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:21 ())))
  in
  let ema_34 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:34 ())))
  in
  let harami_cross =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_haramicross ())))
  in
  let gravestone_doji =
    Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_gravestonedoji ())))
  in
  let trix_14 =
    Data
      (Float_type (Data.Type.Tacaml (Tacaml.Indicator.trix ~timeperiod:14 ())))
  in
  {
    name = "MA Ribbon Harami Cross";
    buy_trigger =
      (* Moving average ribbon alignment (bullish) *)
      ema_8 >. ema_13 &&. (ema_13 >. ema_21) &&. (ema_21 >. ema_34)
      &&.
      (* Price crosses above fastest MA *)
      cross_up close ema_8
      &&.
      (* Harami cross bullish signal *)
      IntGT (harami_cross, Int 0)
      &&.
      (* TRIX momentum positive *)
      (trix_14 >. Float 0.0)
      &&.
      (* Volume surge *)
      (volume >. volume_sma *. Float 1.6);
    sell_trigger =
      (* MA ribbon breakdown *)
      cross_down ema_8 ema_13
      ||.
      (* Bearish harami cross or gravestone doji *)
      IntLT (harami_cross, Int 0)
      ||. IntLT (gravestone_doji, Int 0)
      ||.
      (* TRIX momentum turns negative *)
      (trix_14 <. Float 0.0)
      ||.
      (* Price falls below 21 EMA *)
      (close <. ema_21);
    max_positions = 4;
    position_size = 0.25;
  }

(* Collect all crossover-candlestick strategies *)
let crossover_candlestick_strategies =
  [
    golden_cross_candlestick;
    macd_doji_reversal;
    stochastic_three_soldiers;
    rsi_hanging_man;
    williams_engulfing;
    cci_star_patterns;
    ma_ribbon_harami;
  ]
