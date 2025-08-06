open Gadt

(* Intraday momentum strategy - uses 3-hour RSI (18 periods * 10min) *)
let intraday_momentum_strategy =
  let rsi_3h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.rsi ~timeperiod:18 ()))
  in
  let sma_2h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.sma ~timeperiod:12 ()))
  in
  {
    name = "Intraday Momentum";
    buy_trigger =
      rsi_3h <. Float 30.0 &&. (close >. sma_2h)
      &&. (volume >. volume_sma *. Float 1.3);
    sell_trigger = rsi_3h >. Float 70.0 ||. (close <. sma *. Float 0.97);
    max_positions = 4;
    position_size = 0.25;
  }

(* Daily trend following - uses 4-hour periods (24 periods * 10min) *)
let daily_trend_following =
  let ema_4h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:24 ()))
  in
  let ema_2h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:12 ()))
  in
  let macd_4h =
    Data
      (Data.Type.Tacaml
         (Tacaml.Indicator.macd_macd ~fast_period:12 ~slow_period:26
            ~signal_period:9 ()))
  in
  let macd_signal_4h =
    Data
      (Data.Type.Tacaml
         (Tacaml.Indicator.macd_signal ~fast_period:12 ~slow_period:26
            ~signal_period:9 ()))
  in
  {
    name = "Daily Trend Following";
    buy_trigger =
      cross_up ema_2h ema_4h
      &&. (macd_4h >. macd_signal_4h)
      &&. (volume >. volume_sma *. Float 1.2);
    sell_trigger = cross_down ema_2h ema_4h ||. (close <. ema_4h *. Float 0.95);
    max_positions = 3;
    position_size = 0.33;
  }

(* Volatility breakout - uses hourly ATR (6 periods * 10min) *)
let volatility_breakout_strategy =
  let atr_1h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.atr ~timeperiod:6 ()))
  in
  let bb_20h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.upper_bband ~timeperiod:120 ()))
  in
  let bb_low_20h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.lower_bband ~timeperiod:120 ()))
  in
  let bb_width = bb_20h -. bb_low_20h in
  {
    name = "Hourly Volatility Breakout";
    buy_trigger =
      close >. bb_20h
      &&. (bb_width >. atr_1h *. Float 1.5)
      &&. (volume >. volume_sma *. Float 1.8)
      &&. (adx >. Float 25.0);
    sell_trigger =
      close <. bb_low_20h ||. (close <. lag high 1 -. (atr_1h *. Float 2.0));
    max_positions = 3;
    position_size = 0.33;
  }

(* Swing trading - uses 8-hour periods (48 periods * 10min) *)
let swing_trading_strategy =
  let rsi_8h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.rsi ~timeperiod:48 ()))
  in
  let stoch_k_4h =
    Data
      (Data.Type.Tacaml
         (Tacaml.Indicator.stoch_slow_k ~fast_k_period:30 ~slow_k_period:18 ()))
  in
  let stoch_d_4h =
    Data
      (Data.Type.Tacaml
         (Tacaml.Indicator.stoch_slow_d ~fast_k_period:30 ~slow_k_period:18 ()))
  in
  {
    name = "Intraday Swing Trading";
    buy_trigger =
      rsi_8h <. Float 35.0 &&. (stoch_k_4h <. Float 25.0)
      &&. cross_up stoch_k_4h stoch_d_4h
      &&. (close >. sma);
    sell_trigger =
      rsi_8h >. Float 65.0 ||. (stoch_k_4h >. Float 75.0)
      ||. (close <. sma *. Float 0.93);
    max_positions = 5;
    position_size = 0.2;
  }

(* Mean reversion - uses 2-hour periods (12 periods * 10min) *)
let mean_reversion_2h =
  let bb_upper_2h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.upper_bband ~timeperiod:12 ()))
  in
  let bb_lower_2h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.lower_bband ~timeperiod:12 ()))
  in
  let bb_middle_2h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.middle_bband ~timeperiod:12 ()))
  in
  let rsi_2h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.rsi ~timeperiod:12 ()))
  in
  {
    name = "2-Hour Mean Reversion";
    buy_trigger =
      close <. bb_lower_2h &&. (rsi_2h <. Float 30.0) &&. (close >. lag low 6);
    (* Not making new lows over 1 hour *)
    sell_trigger = close >. bb_middle_2h ||. (close >. bb_upper_2h);
    max_positions = 4;
    position_size = 0.25;
  }

(* Williams %R momentum - uses 30-minute periods (3 periods * 10min) *)
let williams_30min_momentum =
  let wr_30m =
    Data (Data.Type.Tacaml (Tacaml.Indicator.willr ~timeperiod:3 ()))
  in
  let wr_1h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.willr ~timeperiod:6 ()))
  in
  let roc_1h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.roc ~timeperiod:6 ()))
  in
  {
    name = "Williams 30min Momentum";
    buy_trigger =
      wr_30m >. Float (-80.0) &&. (wr_1h >. Float (-75.0))
      &&. cross_up wr_30m wr_1h &&. (roc_1h >. Float 0.5)
      &&. (volume >. volume_sma);
    sell_trigger = wr_30m <. Float (-20.0) ||. cross_down macd macd_signal;
    max_positions = 6;
    position_size = 0.16;
  }

(* CCI divergence - uses 1-hour periods (6 periods * 10min) *)
let cci_1h_divergence =
  let cci_1h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.cci ~timeperiod:6 ()))
  in
  let cci_lag1 = lag cci_1h 1 in
  let cci_lag3 = lag cci_1h 3 in
  {
    name = "CCI 1-Hour Divergence";
    buy_trigger =
      cci_1h <. Float (-100.0) &&. (cci_1h >. cci_lag1)
      &&.
      (* CCI turning up *)
      (cci_1h >. cci_lag3)
      &&.
      (* CCI higher than 30 min ago *)
      (close <. lag close 3)
      &&.
      (* Price lower - divergence *)
      (rsi <. Float 45.0);
    sell_trigger = cci_1h >. Float 100.0 ||. (close >. bb_upper);
    max_positions = 4;
    position_size = 0.25;
  }

(* Triple EMA - uses 30min, 1h, 2h periods *)
let triple_ema_intraday =
  let ema_30m =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:3 ()))
  in
  let ema_1h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:6 ()))
  in
  let ema_2h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:12 ()))
  in
  let mom_30m =
    Data (Data.Type.Tacaml (Tacaml.Indicator.mom ~timeperiod:3 ()))
  in
  {
    name = "Triple EMA Intraday";
    buy_trigger =
      ema_30m >. ema_1h &&. (ema_1h >. ema_2h) &&. cross_up close ema_30m
      &&. (mom_30m >. Float 0.0)
      &&. (volume >. volume_sma *. Float 1.1);
    sell_trigger = cross_down ema_30m ema_1h ||. (close <. ema_30m *. Float 0.96);
    max_positions = 5;
    position_size = 0.2;
  }

(* TRIX oscillator - uses 2-hour periods (12 periods * 10min) *)
let trix_2h_oscillator =
  let trix_2h =
    Data (Data.Type.Tacaml (Tacaml.Indicator.trix ~timeperiod:12 ()))
  in
  let trix_lag1 = lag trix_2h 1 in
  {
    name = "TRIX 2-Hour Oscillator";
    buy_trigger =
      trix_2h >. Float 0.0 &&. (trix_2h >. trix_lag1)
      &&.
      (* TRIX turning up *)
      (lag trix_2h 1 <=. Float 0.0)
      &&.
      (* Just crossed above zero *)
      (macd_hist >. Float 0.0);
    sell_trigger =
      trix_2h <. Float 0.0 ||. (trix_2h <. trix_lag1 &&. (trix_2h >. Float 0.05));
    max_positions = 3;
    position_size = 0.33;
  }

(* Advanced candlestick with volume confirmation *)
let candlestick_with_volume =
  let three_soldiers =
    Data (Data.Type.Tacaml (Tacaml.Indicator.cdl_3whitesoldiers ()))
  in
  let morning_doji =
    Data (Data.Type.Tacaml (Tacaml.Indicator.cdl_morningdojistar ()))
  in
  let piercing_line =
    Data (Data.Type.Tacaml (Tacaml.Indicator.cdl_piercing ()))
  in
  let three_crows =
    Data (Data.Type.Tacaml (Tacaml.Indicator.cdl_3blackcrows ()))
  in
  let evening_doji =
    Data (Data.Type.Tacaml (Tacaml.Indicator.cdl_eveningdojistar ()))
  in
  let dark_cloud =
    Data (Data.Type.Tacaml (Tacaml.Indicator.cdl_darkcloudcover ()))
  in
  let volume_3bar_avg =
    (lag volume 1 +. lag volume 2 +. lag volume 3) /. Float 3.0
  in
  {
    name = "Candlestick Volume Confirmation";
    buy_trigger =
      three_soldiers >. Float 0.0
      ||. (morning_doji >. Float 0.0)
      ||. (piercing_line >. Float 0.0)
      &&. (rsi <. Float 55.0)
      &&. (volume >. volume_3bar_avg *. Float 1.5)
      &&. (close >. open_);
    sell_trigger =
      three_crows <. Float 0.0
      ||. (evening_doji <. Float 0.0)
      ||. (dark_cloud <. Float 0.0)
      ||. (close >. sma *. Float 1.06);
    max_positions = 6;
    position_size = 0.16;
  }

(* Medium-term trend following - uses 20/50 crossover with filters *)
let medium_term_crossover =
  (* With 10min bars: 20 periods = 3.3 hours, 50 = 8.3 hours, 100 = 16.7 hours *)
  let ema_20 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:20 ()))
  in
  let ema_50 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:50 ()))
  in
  let sma_100 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.sma ~timeperiod:100 ()))
  in
  let rsi_50 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.rsi ~timeperiod:50 ()))
  in
  let volume_50 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.sma ~timeperiod:50 ()))
  in
  let adx_50 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.adx ~timeperiod:50 ()))
  in
  {
    name = "Medium Term Crossover";
    buy_trigger =
      (* Main signal: 20-period EMA crosses above 50-period EMA *)
      cross_up ema_20 ema_50
      &&.
      (* Trend filter: price above 100-period SMA *)
      (close >. sma_100)
      &&.
      (* Momentum filter: RSI healthy range *)
      (rsi_50 >. Float 40.0) &&. (rsi_50 <. Float 70.0)
      &&.
      (* Volume confirmation: above 50-period average *)
      (volume >. volume_50 *. Float 1.2)
      &&.
      (* Trend strength: ADX above 25 *)
      (adx_50 >. Float 25.0);
    sell_trigger =
      (* Exit signal: 20-period EMA crosses below 50-period EMA *)
      cross_down ema_20 ema_50
      ||.
      (* Stop loss: close below 100-period SMA *)
      (close <. sma_100)
      ||.
      (* Take profit: RSI overbought *)
      (rsi_50 >. Float 75.0)
      ||.
      (* Trend weakness: ADX below 20 *)
      (adx_50 <. Float 20.0);
    max_positions = 4;
    position_size = 0.25;
  }

(* MACD crossover with moving average filter *)
let macd_crossover_filtered =
  let macd_line =
    Data
      (Data.Type.Tacaml
         (Tacaml.Indicator.macd_macd ~fast_period:12 ~slow_period:26
            ~signal_period:9 ()))
  in
  let macd_signal_line =
    Data
      (Data.Type.Tacaml
         (Tacaml.Indicator.macd_signal ~fast_period:12 ~slow_period:26
            ~signal_period:9 ()))
  in
  let ema_100 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:100 ()))
  in
  let rsi_30 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.rsi ~timeperiod:30 ()))
  in
  let atr_20 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.atr ~timeperiod:20 ()))
  in
  {
    name = "MACD Crossover Filtered";
    buy_trigger =
      (* Main signal: MACD line crosses above signal line *)
      cross_up macd_line macd_signal_line
      &&.
      (* Trend filter: price above 100-period EMA *)
      (close >. ema_100)
      &&.
      (* MACD above zero line (bullish momentum) *)
      (macd_line >. Float 0.0)
      &&.
      (* RSI not overbought *)
      (rsi_30 >. Float 35.0) &&. (rsi_30 <. Float 70.0)
      &&.
      (* Price not too extended (within 2 ATR of EMA) *)
      (close <. ema_100 +. (atr_20 *. Float 2.0));
    sell_trigger =
      (* Exit signal: MACD line crosses below signal line *)
      cross_down macd_line macd_signal_line
      ||.
      (* Stop loss: close below 100-period EMA *)
      (close <. ema_100 *. Float 0.98)
      ||.
      (* Take profit: RSI overbought *)
      (rsi_30 >. Float 75.0)
      ||.
      (* MACD below zero (bearish momentum) *)
      (macd_line <. Float 0.0);
    max_positions = 5;
    position_size = 0.2;
  }

(* Triple moving average system with momentum *)
let triple_ma_momentum =
  let ema_10 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:10 ()))
  in
  let ema_30 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.ema ~timeperiod:30 ()))
  in
  let sma_60 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.sma ~timeperiod:60 ()))
  in
  let rsi_20 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.rsi ~timeperiod:20 ()))
  in
  let mom_10 =
    Data (Data.Type.Tacaml (Tacaml.Indicator.mom ~timeperiod:10 ()))
  in
  {
    name = "Triple MA Momentum";
    buy_trigger =
      (* Moving average alignment: 10 > 30 > 60 *)
      ema_10 >. ema_30 &&. (ema_30 >. sma_60)
      &&.
      (* Entry signal: price crosses above 10-period EMA *)
      cross_up close ema_10
      &&.
      (* Momentum confirmation *)
      (mom_10 >. Float 0.0)
      &&.
      (* RSI not oversold but not overbought *)
      (rsi_20 >. Float 40.0) &&. (rsi_20 <. Float 70.0)
      &&.
      (* Volume confirmation *)
      (volume >. volume_sma *. Float 1.1);
    sell_trigger =
      (* Exit when MA alignment breaks *)
      ema_10 <. ema_30
      ||.
      (* Price falls below 30-period EMA *)
      (close <. ema_30)
      ||.
      (* Momentum turns negative *)
      (mom_10 <. Float 0.0)
      ||.
      (* RSI overbought *)
      (rsi_20 >. Float 75.0);
    max_positions = 6;
    position_size = 0.16;
  }

(* Collect all strategies in a list *)
let all_strategies =
  [
    intraday_momentum_strategy;
    daily_trend_following;
    volatility_breakout_strategy;
    swing_trading_strategy;
    mean_reversion_2h;
    williams_30min_momentum;
    cci_1h_divergence;
    triple_ema_intraday;
    trix_2h_oscillator;
    candlestick_with_volume;
    medium_term_crossover;
    macd_crossover_filtered;
    triple_ma_momentum;
  ]
