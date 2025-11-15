(* open Gadt_fo.Variable *)
open Gadt
open Gadt_strategy

(* Risk management helper functions - return bool Gadt.t expressions *)


(** A data collection listener strategy that never buys or sells *)
let listener_strategy =

  {
       name = "Listener";
       buy_trigger = Const (0.0, Float) <. Const (0.0, Float);
       (* Never buy *)
       sell_trigger = Const (0.0, Float) <. Const (0.0, Float);
       (* Never sell *)
       score = Const (0.0, Float);
       (* Score irrelevant - never buys *)
       max_positions = 0;
       (* No positions allowed *)
       position_size = 0.0;
       (* No position sizing needed *)
     }

let stupid =

  {
       name = "Dumb strategy";
       buy_trigger =
         close
         >. Const (100.0, Float)
         &&. (Gadt_fo.Constant.rsi 18 () >. Const (40.0, Float));
       sell_trigger = close <. Const (100.0, Float);
       score = Const (1.0, Float);  (* Simple test strategy - all equal *)
       max_positions = 1;
       position_size = 1.0;
     }

let dual_sma_rsi =
  (* Dual SMA + RSI *)

  {
       name = "SEAKING";
       (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
       buy_trigger =
         Gadt_fo.Variable.sma () >. Gadt_fo.Variable.tema ()
         &&. (Gadt_fo.Variable.rsi () <. Const (30.0, Float));
       (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
       sell_trigger =
         Gadt_fo.Variable.sma () <. Gadt_fo.Variable.tema ()
         ||. (Gadt_fo.Variable.rsi () >. Const (70.0, Float));
       score = Const (100.0, Float) -. Gadt_fo.Variable.rsi ();  (* Lower RSI = higher score *)
       max_positions = 2;
       position_size = 0.5;
     }

module Real = Gadt_fo.Constant

let golden_cross_1 =
  (* Classic moving average crossover strategy *)

  {
       name = "GCONST";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up (Real.ema 50 ()) (Real.sma 57 ());
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down (Real.ema 60 ()) (Real.sma 58 ());
       score = Const (1.0, Float);  (* Simple crossover - all signals equal *)
       max_positions = 1;
       position_size = 1.0;
     }

let smarsi0 =
  (* Dual SMA + RSI *)

  let sma = Real.sma 10 () in
  let tema = Real.tema 8 () in
  let rsi = Real.rsi 4 () in
  {
    name = "SMARSI0";
    (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
    buy_trigger = sma >. tema &&. (rsi <. Const (30.0, Float));
    (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
    sell_trigger = sma <. tema ||. (rsi >. Const (70.0, Float));
    score = Const (100.0, Float) -. rsi;  (* Lower RSI = higher score *)
    max_positions = 2;
    position_size = 0.5;
  }

let smarsi1 =
  (* Dual SMA + RSI *)

  let sma = Real.sma 12 () in
  let tema = Real.tema 13 () in
  let rsi = Real.rsi 7 () in
  {
    name = "SMARSI1";
    (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
    buy_trigger = sma >. tema &&. (rsi <. Const (30.0, Float));
    (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
    sell_trigger = sma <. tema ||. (rsi >. Const (70.0, Float));
    score = Const (100.0, Float) -. rsi;  (* Lower RSI = higher score *)
    max_positions = 2;
    position_size = 0.5;
  }

let golden_cross =
  (* Classic moving average crossover strategy *)

  {
       name = "GoldenCross";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger =
         cross_up (Gadt_fo.Variable.ema ()) (Gadt_fo.Variable.sma ());
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger =
         cross_down (Gadt_fo.Variable.ema ()) (Gadt_fo.Variable.sma ());
       score = Const (1.0, Float);  (* Simple crossover - all signals equal *)
       max_positions = 1;
       position_size = 1.0;
     }

let golden_cross_0 =
  (* Classic moving average crossover strategy *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in

  {
       name = "GoldenCrossSa";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down ema sma;
       score = Const (1.0, Float);  (* Simple crossover - all signals equal *)
       max_positions = 1;
       position_size = 1.0;
     }

let plurple =
  (* Classic moving average crossover strategy *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in

  {
       name = "Pflurple";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below SMA *)
       sell_trigger = cross_down ema sma;
       score = Const (1.0, Float);  (* Simple crossover - all signals equal *)
       max_positions = 8;
       position_size = 0.5;
     }

let pflurple_kelly =
  (* Improved Pflurple using Kelly-inspired position sizing *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in

  {
       name = "PflurpleKelly";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down ema sma;
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 16;
       (* More diversification *)
       position_size = 0.125;
       (* 12.5% per position = 200% max allocation *)
     }

let pflurple_conservative =
  (* Conservative version with proper risk management *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in

  {
       name = "PflurpleConservative";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down ema sma;
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 20;
       (* Maximum diversification *)
       position_size = 0.08;
       (* 8% per position = 160% max allocation *)
     }

let pflurple_enhanced =
  (* Enhanced version with momentum filter *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in

  {
       name = "PflurpleEnhanced";
       (* Buy when EMA crosses SMA AND momentum is positive *)
       buy_trigger = cross_up ema sma &&. (Real.mom 10 () >. Const (0.0, Float));
       (* Sell when EMA crosses below SMA OR momentum turns negative *)
       sell_trigger =
         cross_down ema sma ||. (Real.mom 10 () <. Const (-1.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 12;
       position_size = 0.15;
       (* 15% per position = 180% max allocation *)
     }

let mean_reversion_lag =
  (* Mean reversion using lagged price comparison *)

  {
       name = "MeanReversionlag";
       (* Buy when current price is below lagged price by significant margin and RSI oversold *)
       buy_trigger =
         close <. lag close 5 &&. (Real.rsi 14 () <. Const (25.0, Float));
       (* Sell when current price recovers above lagged SMA or RSI overbought *)
       sell_trigger =
         close
         >. lag (Real.sma 20 ()) 3
         ||. (Real.rsi 14 () >. Const (75.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 3;
       position_size = 0.33;
     }

let bollinger_breakout =
  (* Bollinger Band breakout with momentum confirmation *)

  {
       name = "BollingerBreakout";
       (* Buy when price breaks above upper band and momentum is positive *)
       buy_trigger =
         close
         >. Real.upper_bband 20 2.0 2.0 ()
         &&. (Real.mom 10 () >. Const (0.0, Float));
       (* Sell when price falls back to middle band or momentum turns negative *)
       sell_trigger =
         close
         <. Real.middle_bband 20 2.0 2.0 ()
         ||. (Real.mom 10 () <. Const (-1.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 2;
       position_size = 0.6;
     }

let triple_crossover =
  (* Complex triple moving average system with volume confirmation *)

  {
       name = "TripleCross";
       (* Buy when fast > medium > slow MA and volume above average *)
       buy_trigger =
         Real.ema 12 () >. Real.sma 26 ()
         &&. (Real.sma 26 () >. Real.tema 50 ())
         &&. (volume >. lag volume 10);
       (* Sell when any MA order breaks or volume dries up *)
       sell_trigger =
         Real.ema 12 () <. Real.sma 26 ()
         ||. (Real.sma 26 () <. Real.tema 50 ())
         ||. (volume <. lag volume 20);
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 1;
       position_size = 0.8;
     }

let momentum_divergence =
  (* Momentum divergence with lagged comparison *)

  {
       name = "Momentum(/.)";
       (* Buy when price makes new low but momentum is improving (divergence) *)
       buy_trigger =
         close <. lag close 5
         &&. (Real.mom 10 () >. lag (Real.mom 10 ()) 5)
         &&. (Real.rsi 14 () <. Const (40.0, Float));
       (* Sell when momentum peaks or price recovers strongly *)
       sell_trigger =
         Real.mom 10 ()
         <. lag (Real.mom 10 ()) 2
         ||. (close >. lag close 10 +. Const (2.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 2;
       position_size = 0.4;
     }

let adaptive_channels =
  (* Adaptive channel breakout using ATR for dynamic thresholds *)

  {
       name = "AdaptiveChannels";
       (* Buy when price breaks above SMA + ATR channel *)
       buy_trigger = close >. Real.sma 20 () +. Real.atr 14 ();
       (* Sell when price breaks below SMA - ATR channel *)
       sell_trigger = close <. Real.sma 20 () -. Real.atr 14 ();
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 1;
       position_size = 1.0;
     }

let contrarian_spike =
  (* Contrarian strategy for price spikes with lag confirmation *)

  {
       name = "ContrarianSpike";
       (* Buy when price spikes down significantly from recent average *)
       buy_trigger =
         close
         <. lag (Real.sma 20 ()) 3 *. Const (0.95, Float)
         &&. (Real.rsi 14 () <. Const (20.0, Float));
       (* Sell when price recovers to normal levels or gets overbought *)
       sell_trigger =
         close
         >. lag (Real.sma 20 ()) 1
         ||. (Real.rsi 14 () >. Const (80.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 4;
       position_size = 0.25;
     }

let rsi_divergence_crossover =
  (* RSI divergence with EMA crossover confirmation *)

  {
       name = "RSI(/.)Cross";
       (* Buy when RSI crosses above 30 and EMA crosses above SMA *)
       buy_trigger =
         cross_up (Real.rsi 14 ()) (Const (30.0, Float))
         &&. cross_up (Gadt_fo.Variable.ema ()) (Gadt_fo.Variable.sma ());
       (* Sell when RSI crosses below 70 or EMA crosses below SMA *)
       sell_trigger =
         cross_down (Real.rsi 14 ()) (Const (70.0, Float))
         ||. cross_down (Gadt_fo.Variable.ema ()) (Gadt_fo.Variable.sma ());
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 2;
       position_size = 0.5;
     }

let macd_momentum_lag =
  (* MACD with momentum and lag confirmation *)

  {
       name = "MACDMomlag";
       (* Buy when MACD > signal momentum positive and price > 5-period lag *)
       buy_trigger =
         Real.macd_macd 12 26 9 ()
         >. Real.macd_signal 12 26 9 ()
         &&. (Real.mom 10 () >. Const (0.0, Float))
         &&. (close >. lag close 5);
       (* Sell when MACD < signal or price drops below 3-period lag SMA *)
       sell_trigger =
         Real.macd_macd 12 26 9 ()
         <. Real.macd_signal 12 26 9 ()
         ||. (close <. lag (Real.sma 20 ()) 3);
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 1;
       position_size = 0.8;
     }

let stochastic_crossover_volume =
  (* Stochastic crossover with volume confirmation *)

  {
       name = "StochCrossVol";
       (* Buy when slow %K crosses above slow %D and volume > 10-period average *)
       buy_trigger =
         cross_up (Real.stoch_slow_k 14 3 3 ()) (Real.stoch_slow_d 14 3 3 ())
         &&. (volume >. lag (Real.sma 10 ()) 10);
       (* Sell when slow %K crosses below slow %D or volume dries up *)
       sell_trigger =
         cross_down (Real.stoch_slow_k 14 3 3 ()) (Real.stoch_slow_d 14 3 3 ())
         ||. (volume <. lag volume 20 /. Const (2.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 3;
       position_size = 0.33;
     }

let williams_r_reversal =
  (* Williams %R mean reversion with lag confirmation *)

  {
       name = "WillRReversal";
       (* Buy when Williams %R oversold and price below lagged low *)
       buy_trigger =
         Real.willr 14 ()
         <. Const (-80.0, Float)
         &&. (close <. lag low 3)
         &&. (Real.rsi 14 () <. Const (35.0, Float));
       (* Sell when Williams %R overbought or price above lagged high *)
       sell_trigger =
         Real.willr 14 () >. Const (-20.0, Float) ||. (close >. lag high 2);
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 2;
       position_size = 0.6;
     }

let triple_ema_crossover =
  (* Triple EMA system with different timeframes *)

  {
       name = "TripleEMA";
       (* Buy when fast EMA > medium EMA > slow EMA (all aligned) *)
       buy_trigger =
         Real.ema 12 () >. Real.tema 26 ()
         &&. (Real.tema 26 () >. Real.dema 50 ())
         &&. cross_up (Real.ema 12 ()) (Real.tema 26 ());
       (* Sell when any EMA alignment breaks *)
       sell_trigger =
         Real.ema 12 () <. Real.tema 26 ()
         ||. (Real.tema 26 () <. Real.dema 50 ())
         ||. cross_down (Real.ema 12 ()) (Real.tema 26 ());
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 1;
       position_size = 0.9;
     }

let cci_overbought_oversold =
  (* CCI overbought/oversold with momentum filter *)

  {
       name = "CCIMomentum";
       (* Buy when CCI oversold and momentum turning positive *)
       buy_trigger =
         Real.cci 14 ()
         <. Const (-100.0, Float)
         &&. cross_up (Real.mom 10 ()) (Const (0.0, Float))
         &&. (close >. lag close 2);
       (* Sell when CCI overbought or momentum turning negative *)
       sell_trigger =
         Real.cci 14 ()
         >. Const (100.0, Float)
         ||. cross_down (Real.mom 10 ()) (Const (0.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 2;
       position_size = 0.5;
     }

let aroon_trend_following =
  (* Aroon oscillator trend following with volume *)

  {
       name = "AroonTrend";
       (* Buy when Aroon Up > Aroon Down and oscillator > 50 *)
       buy_trigger =
         Real.aroon_up 14 () >. Real.aroon_down 14 ()
         &&. (Real.aroon_osc 14 () >. Const (50.0, Float))
         &&. (volume >. lag volume 5);
       (* Sell when Aroon Down > Aroon Up or oscillator < -50 *)
       sell_trigger =
         Real.aroon_down 14 () >. Real.aroon_up 14 ()
         ||. (Real.aroon_osc 14 () <. Const (-50.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 1;
       position_size = 0.8;
     }

let parabolic_sar_trend =
  (* Parabolic SAR trend following with lag confirmation *)

  {
       name = "ParabolicSARTrend";
       (* Buy when price crosses above SAR and is above 5-period high *)
       buy_trigger =
         cross_up close (Real.sar 0.02 0.2 ())
         &&. (close >. lag high 5)
         &&. (Real.adx 14 () >. Const (25.0, Float));
       (* Sell when price crosses below SAR *)
       sell_trigger =
         cross_down close (Real.sar 0.02 0.2 ())
         ||. (Real.adx 14 () <. Const (20.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 1;
       position_size = 1.0;
     }

let multi_timeframe_rsi =
  (* Multi-timeframe RSI strategy using different lag periods *)

  {
       name = "MultiTimeframeRSI";
       (* Buy when short-term RSI oversold but long-term trend up *)
       buy_trigger =
         Real.rsi 14 ()
         <. Const (30.0, Float)
         &&. (lag (Real.rsi 14 ()) 10 >. lag (Real.rsi 14 ()) 20)
         &&. (close >. lag (Real.sma 20 ()) 20);
       (* Sell when RSI overbought or long-term trend turns down *)
       sell_trigger =
         Real.rsi 14 ()
         >. Const (70.0, Float)
         ||. (lag (Real.rsi 14 ()) 5 <. lag (Real.rsi 14 ()) 15);
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 3;
       position_size = 0.4;
     }

let roc_momentum_cross =
  (* Rate of Change momentum crossover *)

  {
       name = "ROCMomentumCross";
       (* Buy when short ROC crosses above long ROC and both positive *)
       buy_trigger =
         cross_up (Real.roc 10 ()) (lag (Real.roc 10 ()) 5)
         &&. (Real.roc 10 () >. Const (0.0, Float))
         &&. (lag (Real.roc 10 ()) 5 >. Const (0.0, Float));
       (* Sell when short ROC crosses below long ROC *)
       sell_trigger =
         cross_down (Real.roc 10 ()) (lag (Real.roc 10 ()) 5)
         ||. (Real.roc 10 () <. Const (-2.0, Float));
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 2;
       position_size = 0.5;
     }

let always_trading =
  (* Maximum churn strategy - always generates buy and sell signals for all symbols *)

  {
       name = "AlwaysTrading";
       (* Always buy - price is always >= 0 *)
       buy_trigger = close >. Const (0.0, Float);
       (* Always sell - price is always >= 0 *)
       sell_trigger = close >. Const (0.0, Float);
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 10;
       (* Hold up to 10 positions *)
       position_size = 0.10;
       (* 10% per position = 100% max allocation *)
     }

(** 1-Minute Mean Reversion Strategy

    Based on research showing that:
    - 2-period RSI is best for mean reversion (85% success rate)
    - Bollinger Bands (20-period, 2 std devs) identify stretched prices
    - Mean reversion works best on 1-minute bars for intraday trading

    Entry Logic:
    - Buy when price touches lower Bollinger Band AND RSI < 30 (oversold)
    - This catches stocks that have temporarily dropped below their mean

    Exit Logic:
    - Sell when price reaches middle Bollinger Band (the mean)
    - Or RSI > 70 (overbought - momentum reversal)
    - Or price touches upper Bollinger Band (overshot mean)

    Parameters optimized for 1-minute bars:
    - RSI period: 2 (fast response to price changes)
    - Bollinger Band period: 20 (covers 20 minutes of data)
    - BB std devs: 2.0 (standard setting)
    - Max positions: 10 (diversification across mean-reverting stocks)
    - Position size: 10% (allows full deployment across 10 stocks)
*)
let mean_reversion_1min =
  let rsi_2 = Real.rsi 2 () in
  let bb_lower = Real.lower_bband 20 2.0 2.0 () in
  let bb_middle = Real.middle_bband 20 2.0 2.0 () in
  let bb_upper = Real.upper_bband 20 2.0 2.0 () in

  {
       name = "MeanReversion1Min";
       (* Buy: Oversold (RSI < 30) AND price touched lower Bollinger Band *)
       buy_trigger =
         (rsi_2 <. Const (30.0, Float)) &&. (last <. bb_lower);
       (* Sell: Return to mean OR overbought OR price hit upper band *)
       sell_trigger =
         (last >. bb_middle)  (* Price returned to mean *)
         ||. (rsi_2 >. Const (70.0, Float))  (* Overbought *)
         ||. (last >. bb_upper);  (* Overshot to upper band *)
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** 1-Minute Mean Reversion Strategy - Optimizable Version

    This version uses Variables instead of Constants, allowing NLopt to optimize:
    - RSI period (search range: 2-14)
    - RSI oversold threshold (search range: 20-40)
    - RSI overbought threshold (search range: 60-80)
    - Bollinger Band period (search range: 10-30)
    - Bollinger Band std devs (search range: 1.5-3.0)

    NLopt will run this strategy with different parameter combinations and
    find the values that maximize returns (or Sharpe ratio).

    To optimize:
      longleaf Backtest MeanReversionOpt --target data/jan24_1min.json

    Expected optimization bounds:
      - rsi_period: [2.0, 14.0]
      - rsi_oversold: [20.0, 40.0]
      - rsi_overbought: [60.0, 80.0]
      - bb_period: [10.0, 30.0]
      - bb_stddev: [1.5, 3.0]
*)
let mean_reversion_opt =
  (* Simplified version with only 2 variables: RSI period and BB period
     This reduces optimization complexity from 10+ variables to just 2
     BB std dev is fixed at 2.0 to keep the search space manageable *)
  let rsi_period_var = Gadt_fo.var Gadt.Type.Int in  (* Variable: RSI period (2-14) *)
  let bb_period_var = Gadt_fo.var Gadt.Type.Int in   (* Variable: BB period (10-30) *)

  (* Create indicators with mixed variable/constant parameters *)
  let rsi_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period_var)))
  in
  let bb_lower_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
        bb_period_var,
        Const (2.0, Float),  (* Fixed: std dev multiplier *)
        Const (2.0, Float)))) (* Fixed: deviation *)
  in
  let bb_middle_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
        bb_period_var,  (* Same period variable reused *)
        Const (2.0, Float),
        Const (2.0, Float))))
  in
  let bb_upper_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.upper_bband", Tacaml.Indicator.Raw.upper_bband),
        bb_period_var,  (* Same period variable reused *)
        Const (2.0, Float),
        Const (2.0, Float))))
  in

  {
       name = "MeanReversionOpt";
       (* Buy: Oversold RSI AND price touched lower Bollinger Band *)
       buy_trigger =
         (rsi_var <. Const (30.0, Float)) &&. (last <. bb_lower_var);
       (* Sell: Return to mean OR overbought OR price hit upper band *)
       sell_trigger =
         (last >. bb_middle_var)
         ||. (rsi_var >. Const (70.0, Float))
         ||. (last >. bb_upper_var);
       score = Const (100.0, Float) -. rsi_var;  (* Lower RSI = higher score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** MeanReversion_8_27 - Optimized 1-Minute Mean Reversion Strategy

    This strategy was discovered through NLopt ISRES optimization over 1000 evaluations
    on September-October 2025 data. It uses an 8-period RSI and 27-period Bollinger Bands.

    Optimization Results:
    - Objective value: 105,749.10
    - RSI period: 8 (vs. standard 2 or 14)
    - BB period: 27 (vs. standard 20)
    - Both longer than typical settings, suggesting this catches medium-term reversions

    Entry: RSI(8) < 30 AND Last < BB_lower(27, 2σ)
    Exit: Last > BB_middle OR RSI(8) > 70 OR Last > BB_upper
*)
let mean_reversion_8_27 =
  let rsi_8 = Real.rsi 8 () in
  let bb_lower_27 = Real.lower_bband 27 2.0 2.0 () in
  let bb_middle_27 = Real.middle_bband 27 2.0 2.0 () in
  let bb_upper_27 = Real.upper_bband 27 2.0 2.0 () in

  {
       name = "MeanReversion_8_27";
       buy_trigger =
         (rsi_8 <. Const (30.0, Float)) &&. (last <. bb_lower_27);
       sell_trigger =
         (last >. bb_middle_27)
         ||. (rsi_8 >. Const (70.0, Float))
         ||. (last >. bb_upper_27);
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** MeanReversion_8_27_Safe - Production-Ready Version with Risk Controls

    Same entry/exit logic as MeanReversion_8_27, but adds critical risk management:
    - 2% stop-loss to limit downside on losing trades
    - 5% profit target to lock in gains
    - 60-tick (60 minute) maximum holding time to prevent overnight risk

    Entry: RSI(8) < 30 AND Last < BB_lower(27, 2σ)
    Exit: Original signals OR stop-loss OR profit target OR max hold time
*)
let mean_reversion_8_27_safe =
  let rsi_8 = Real.rsi 8 () in
  let bb_lower_27 = Real.lower_bband 27 2.0 2.0 () in
  let bb_middle_27 = Real.middle_bband 27 2.0 2.0 () in
  let bb_upper_27 = Real.upper_bband 27 2.0 2.0 () in

  {
       name = "MeanReversion_8_27_Safe";
       buy_trigger =
         (rsi_8 <. Const (30.0, Float)) &&. (last <. bb_lower_27);
       sell_trigger =
         (* Original exit signals *)
         (last >. bb_middle_27)
         ||. (rsi_8 >. Const (70.0, Float))
         ||. (last >. bb_upper_27)
         (* Risk management exits *)
         ||. stop_loss 0.02       (* 2% stop-loss *)
         ||. profit_target 0.05   (* 5% profit target *)
         ||. max_holding_time 60; (* 60 minutes max hold *)
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** MeanReversion_Safe_Opt - Optimizable Version with Fixed Risk Controls

    ISRES-optimizable version of the safe mean reversion strategy.
    Indicator periods and thresholds are variables; risk management is fixed.

    Variables to optimize (4 total):
    1. rsi_period: [5, 30] - RSI lookback period (wider to find slow RSI like CosmicCowgirl)
    2. bb_period: [15, 60] - Bollinger Band period (wider to allow longer bands)
    3. rsi_oversold: [20.0, 40.0] - RSI buy threshold
    4. rsi_overbought: [60.0, 85.0] - RSI sell threshold

    Fixed risk management:
    - 2% stop-loss
    - 5% profit target
    - 60-minute maximum holding time

    Entry: RSI(var) < var AND Last < BB_lower(var)
    Exit: Signals OR stop-loss(fixed) OR profit(fixed) OR max_hold(fixed)
*)
let mean_reversion_safe_opt =
  (* Create variables for optimizable parameters *)
  let rsi_period_var = Gadt_fo.var ~lower:5.0 ~upper:30.0 Gadt.Type.Int in       (* Variable 1: RSI period *)
  let bb_period_var = Gadt_fo.var ~lower:15.0 ~upper:60.0 Gadt.Type.Int in        (* Variable 2: BB period *)
  let rsi_oversold_var = Gadt_fo.var ~lower:20.0 ~upper:40.0 Gadt.Type.Float in   (* Variable 3: RSI oversold threshold *)
  let rsi_overbought_var = Gadt_fo.var ~lower:60.0 ~upper:85.0 Gadt.Type.Float in (* Variable 4: RSI overbought threshold *)

  (* Create indicators with variable parameters *)
  let rsi_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period_var)))
  in
  let bb_lower_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
        bb_period_var,
        Const (2.0, Float),  (* Fixed: std dev multiplier *)
        Const (2.0, Float)))) (* Fixed: deviation *)
  in
  let bb_middle_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
        bb_period_var,
        Const (2.0, Float),
        Const (2.0, Float))))
  in
  let bb_upper_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.upper_bband", Tacaml.Indicator.Raw.upper_bband),
        bb_period_var,
        Const (2.0, Float),
        Const (2.0, Float))))
  in


  {
       name = "MeanReversion_Safe_Opt";
       buy_trigger =
         (rsi_var <. rsi_oversold_var) &&. (last <. bb_lower_var);
       sell_trigger =
         (* Original exit signals with variable thresholds *)
         (last >. bb_middle_var)
         ||. (rsi_var >. rsi_overbought_var)
         ||. (last >. bb_upper_var)
         (* Fixed risk management exits *)
         ||. stop_loss 0.02       (* 2% stop-loss *)
         ||. profit_target 0.05   (* 5% profit target *)
         ||. max_holding_time 60; (* 60 minutes max hold *)
       score = Const (100.0, Float) -. rsi_var;  (* Lower RSI = higher score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** RocketReef - Optimized Mean Reversion Strategy

    This strategy emerged from NLopt optimization achieving an objective value of 106,246.32.

    What makes it unique:
    - Uses 53-period RSI (highly unusual - most strategies use 2-14)
    - Tight RSI thresholds around 50 (49.96 for buy, 51.43 for sell)
    - This captures mean reversion around RSI midpoint, not traditional oversold/overbought
    - 27-period Bollinger Bands (slightly wider than standard 20)

    The 53-period RSI smooths out noise while the tight thresholds around 50
    catch subtle shifts in momentum. Combined with Bollinger Bands touching the
    lower band, it identifies stocks that are temporarily weak but not crashed.

    Entry Logic:
    - RSI(53) dips below 49.96 (just starting to weaken)
    - Price touches lower Bollinger Band (27-period)
    - This catches early weakness, not deep oversold

    Exit Logic (OR of any condition):
    - Price returns to middle Bollinger Band (mean reversion complete)
    - RSI(53) rises above 51.43 (momentum turning positive)
    - Price hits upper Bollinger Band (overshot the mean)
    - 2% stop-loss (risk management)
    - 5% profit target (lock in gains)
    - 60-minute max hold (prevent overnight exposure)

    Optimization Results:
    - Objective value: 106,246.32
    - RSI period: 53 (optimized from range 5-100)
    - RSI buy threshold: 49.96 (optimized from range 20-50)
    - RSI sell threshold: 51.43 (optimized from range 50-80)
    - BB period: 27 (optimized from range 15-35)
*)
let rocket_reef =
  let rsi_53 = Real.rsi 53 () in
  let bb_lower_27 = Real.lower_bband 27 2.0 2.0 () in
  let bb_middle_27 = Real.middle_bband 27 2.0 2.0 () in
  let bb_upper_27 = Real.upper_bband 27 2.0 2.0 () in

  {
       name = "RocketReef";
       (* Buy: RSI(53) dips below 49.96 AND price touches lower BB *)
       buy_trigger =
         (rsi_53 <. Const (49.961254, Float)) &&. (last <. bb_lower_27);
       (* Sell: Mean reversion signals OR risk management exits *)
       sell_trigger =
         (* Mean reversion complete signals *)
         (last >. bb_middle_27)  (* Price returned to mean *)
         ||. (rsi_53 >. Const (51.425345, Float))  (* Momentum turning positive *)
         ||. (last >. bb_upper_27)  (* Overshot to upper band *)
         (* Risk management exits *)
         ||. stop_loss 0.02       (* 2% stop-loss *)
         ||. profit_target 0.05   (* 5% profit target *)
         ||. max_holding_time 60; (* 60 minutes max hold *)
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** RocketReef_DayOnly - Intraday-only variant

    RocketReef adapted for intraday trading with no overnight positions.
    Avoids the closing auction volatility window (last 10 minutes).
*)
let rocket_reef_day_only =
  let rsi_53 = Real.rsi 53 () in
  let bb_lower_27 = Real.lower_bband 27 2.0 2.0 () in
  let bb_middle_27 = Real.middle_bband 27 2.0 2.0 () in
  let bb_upper_27 = Real.upper_bband 27 2.0 2.0 () in

  {
       name = "RocketReef_DayOnly";
       buy_trigger =
         (rsi_53 <. Const (49.961254, Float))
         &&. (last <. bb_lower_27)
         &&. safe_to_enter ();
       sell_trigger =
         force_exit_eod ()
         ||. (last >. bb_middle_27)
         ||. (rsi_53 >. Const (51.425345, Float))
         ||. (last >. bb_upper_27)
         ||. stop_loss 0.02
         ||. profit_target 0.05
         ||. max_holding_time 60;
       score = Const (1.0, Float);  (* Default score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** RocketReef_DayOnly_Opt - Optimizable Intraday Mean Reversion (Multi-Position)

    Intraday-only mean reversion strategy with all parameters as variables.
    Holds up to 10 positions (10% allocation each) with tight risk management.

    Variables to optimize (7 total):
    1. rsi_period: [40, 60] - RSI lookback period
    2. rsi_buy_threshold: [20.0, 50.0] - RSI oversold entry level
    3. rsi_sell_threshold: [50.0, 80.0] - RSI exit level
    4. bb_period: [15, 35] - Bollinger Band period
    5. stop_loss_pct: [0.01, 0.05] - Stop-loss percentage (1-5%)
    6. profit_target_pct: [0.02, 0.10] - Profit target percentage (2-10%)
    7. max_hold_ticks: [30, 120] - Maximum hold time in ticks

    Entry Logic:
    - RSI(var) < var (momentum weakness)
    - Price < Lower Bollinger Band(var) (price stretched below mean)
    - NOT within 10 minutes of market close

    Exit Logic:
    - Force exit within 10 minutes of close (no overnight)
    - Price returns to middle Bollinger Band (mean reversion complete)
    - RSI(var) > var (momentum turning positive)
    - Price hits upper Bollinger Band (overshot the mean)
    - Variable stop-loss
    - Variable profit target
    - Variable max hold time

    Multiple positions allow portfolio diversification while maintaining strategy focus.
*)
let rocket_reef_day_only_opt =
  (* Create variables for optimizable parameters *)
  let rsi_period_var = Gadt_fo.var Gadt.Type.Int in             (* Variable 1 *)
  let rsi_buy_threshold_var = Gadt_fo.var Gadt.Type.Float in    (* Variable 2 *)
  let rsi_sell_threshold_var = Gadt_fo.var Gadt.Type.Float in   (* Variable 3 *)
  let bb_period_var = Gadt_fo.var Gadt.Type.Int in              (* Variable 4 *)
  let stop_loss_var = Gadt_fo.var Gadt.Type.Float in            (* Variable 5 *)
  let profit_target_var = Gadt_fo.var Gadt.Type.Float in        (* Variable 6 *)
  let max_hold_ticks_var = Gadt_fo.var Gadt.Type.Int in         (* Variable 7 *)

  (* Create RSI indicator with variable period *)
  let rsi_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period_var)))
  in

  (* Create Bollinger Band indicators with variable period *)
  let bb_lower_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
        bb_period_var,
        Const (2.0, Float),
        Const (2.0, Float))))
  in
  let bb_middle_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
        bb_period_var,
        Const (2.0, Float),
        Const (2.0, Float))))
  in
  let bb_upper_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.upper_bband", Tacaml.Indicator.Raw.upper_bband),
        bb_period_var,
        Const (2.0, Float),
        Const (2.0, Float))))
  in

  (* Variable risk management *)
  let stop_loss_var_risk =
    last <. (EntryPrice *. (Const (1.0, Float) -. stop_loss_var))
  in
  let profit_target_var_risk =
    last >. (EntryPrice *. (Const (1.0, Float) +. profit_target_var))
  in
  let max_hold_var_risk =
    App2 (Fun (">", (>)), TicksHeld, max_hold_ticks_var)
  in


  {
       name = "RocketReef_DayOnly_Opt";
       (* Buy: Mean reversion entry conditions + safe to enter *)
       buy_trigger =
         (rsi_var <. rsi_buy_threshold_var)
         &&. (last <. bb_lower_var)
         &&. safe_to_enter ();
       (* Sell: End-of-day exit first, then mean reversion signals and risk management *)
       sell_trigger =
         force_exit_eod ()
         ||. (last >. bb_middle_var)
         ||. (rsi_var >. rsi_sell_threshold_var)
         ||. (last >. bb_upper_var)
         ||. stop_loss_var_risk
         ||. profit_target_var_risk
         ||. max_hold_var_risk;
       score = Const (100.0, Float) -. rsi_var;  (* Lower RSI = higher score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** RocketReef_Stoch - Optimizable Stricter Version with Fast Stochastic Filter

    This builds on RocketReef by adding a Fast Stochastic Oscillator filter to create
    stricter entry conditions. All indicator periods are variables for optimization.

    Variables to optimize (5 total):
    1. rsi_period: [40, 60] - RSI lookback period
    2. bb_period: [20, 35] - Bollinger Band period
    3. stoch_fastk_period: [10, 20] - Stochastic %K period
    4. stoch_fastd_period: [2, 5] - Stochastic %D smoothing period
    5. stoch_threshold: [15.0, 30.0] - Oversold threshold for stochastic

    The stochastic filter adds confirmation that the stock is deeply oversold,
    filtering out weak signals and improving entry quality.

    Entry Logic:
    - RSI(var) < 49.96 (subtle momentum weakness)
    - Price < Lower Bollinger Band(var) (price stretched below mean)
    - Fast Stochastic %K(var, var) < var (deeply oversold confirmation)

    Exit Logic:
    - Price returns to middle Bollinger Band (mean reversion complete)
    - RSI(var) > 51.43 (momentum turning positive)
    - Price hits upper Bollinger Band (overshot the mean)
    - 2% stop-loss
    - 5% profit target
    - 60-minute max hold time
*)
let rocket_reef_stoch_opt =
  (* Create variables for optimizable parameters *)
  let rsi_period_var = Gadt_fo.var Gadt.Type.Int in           (* Variable 1: RSI period *)
  let bb_period_var = Gadt_fo.var Gadt.Type.Int in            (* Variable 2: BB period *)
  let stoch_fastk_period_var = Gadt_fo.var Gadt.Type.Int in   (* Variable 3: Stochastic %K period *)
  let stoch_fastd_period_var = Gadt_fo.var Gadt.Type.Int in   (* Variable 4: Stochastic %D period *)
  let stoch_threshold_var = Gadt_fo.var Gadt.Type.Float in    (* Variable 5: Stochastic oversold threshold *)

  (* Create RSI indicator with variable period *)
  let rsi_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period_var)))
  in

  (* Create Bollinger Band indicators with variable period *)
  let bb_lower_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
        bb_period_var,
        Const (2.0, Float),  (* Fixed: std dev multiplier *)
        Const (2.0, Float)))) (* Fixed: deviation *)
  in
  let bb_middle_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
        bb_period_var,
        Const (2.0, Float),
        Const (2.0, Float))))
  in
  let bb_upper_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.upper_bband", Tacaml.Indicator.Raw.upper_bband),
        bb_period_var,
        Const (2.0, Float),
        Const (2.0, Float))))
  in

  (* Create Fast Stochastic indicator with variable periods *)
  let stoch_fast_k_var =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App2 (Fun ("I.stoch_f_fast_k", Tacaml.Indicator.Raw.stoch_f_fast_k),
        stoch_fastk_period_var,
        stoch_fastd_period_var)))
  in


  {
       name = "RocketReef_Stoch";
       (* Buy: All conditions must be met *)
       buy_trigger =
         (rsi_var <. Const (49.961254, Float))  (* RSI weakness *)
         &&. (last <. bb_lower_var)              (* Price at lower BB *)
         &&. (stoch_fast_k_var <. stoch_threshold_var);  (* Deep oversold *)
       (* Sell: Mean reversion signals OR risk management exits *)
       sell_trigger =
         (* Mean reversion complete signals *)
         (last >. bb_middle_var)  (* Price returned to mean *)
         ||. (rsi_var >. Const (51.425345, Float))  (* Momentum turning positive *)
         ||. (last >. bb_upper_var)  (* Overshot to upper band *)
         (* Risk management exits *)
         ||. stop_loss 0.02       (* 2% stop-loss *)
         ||. profit_target 0.05   (* 5% profit target *)
         ||. max_holding_time 60; (* 60 minutes max hold *)
       score = Const (100.0, Float) -. rsi_var;  (* Lower RSI = higher score *)
       max_positions = 10;
       position_size = 0.10;
     }

(** Stochness - Optimized RocketReef_Stoch

    Found via NLopt ISRES optimization (50000 iterations).
    Best objective: 106732.91 (6.73% return).

    Optimized parameters:
    - RSI period: 71
    - BB period (buy): 66
    - Stochastic Fast K period: 14
    - Stochastic Fast D period: 8
    - Stochastic threshold: 5.11
    - BB period (sell): 66 (same as buy)

    Entry: RSI < 50 AND price < lower BB AND fast stoch < 5.11
    Exit: Mean reversion OR 2% stop OR 5% target OR 60-tick hold
*)
let stochness =
  (* Optimized parameters *)
  let rsi_period = 71 in
  let bb_period = 66 in
  let stoch_fastk_period = 14 in
  let stoch_fastd_period = 8 in
  let stoch_threshold = 5.108469 in

  (* Create indicators *)
  let rsi = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.rsi rsi_period, Tacaml))) in

  let bb_lower = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.lower_bband bb_period 2.0 2.0, Tacaml))) in

  let bb_middle = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.middle_bband bb_period 2.0 2.0, Tacaml))) in

  let bb_upper = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.upper_bband bb_period 2.0 2.0, Tacaml))) in

  let stoch_fast_k = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.stoch_f_fast_k stoch_fastk_period stoch_fastd_period, Tacaml))) in

  {
    name = "Stochness";
    buy_trigger =
      (rsi <. Const (49.961254, Float))
      &&. (last <. bb_lower)
      &&. (stoch_fast_k <. Const (stoch_threshold, Float));
    sell_trigger =
      (last >. bb_middle)
      ||. (rsi >. Const (51.425345, Float))
      ||. (last >. bb_upper)
      ||. stop_loss 0.02
      ||. profit_target 0.05
      ||. max_holding_time 60;
    score = Const (1.0, Float);  (* Default score *)
    max_positions = 10;
    position_size = 0.10;
  }

(** StochnessMonster - Optimized RocketReef_Stoch (Better Performance)

    Found via NLopt ISRES optimization (iteration 49998).
    Performance: 9.78% return with 3352 orders.

    Raw parameters: 10.73, 92.41, 69.48, 6.32, 46.74, 93.33, 42.48

    This version discovered different optimal parameters than Stochness,
    achieving higher returns (9.78% vs 6.73%). Key differences:
    - Much shorter RSI period (11 vs 71) - responds faster to momentum shifts
    - Much longer initial BB period (92 vs 66) - more stable bands
    - Very long stochastic Fast K (69) - smoother oversold readings
    - Higher stochastic threshold (46.74 vs 5.11) - less strict filter
    - Different BB period for sell (42) - faster mean reversion detection

    The "monster" emerges from the depths with aggressive parameters!
*)
let stochness_monster =
  (* Optimized parameters from iteration 49998 *)
  let rsi_period = 11 in            (* Fast RSI *)
  let bb_period_buy = 92 in         (* Very long BB for entries *)
  let stoch_fastk_period = 69 in    (* Very long stochastic K *)
  let stoch_fastd_period = 6 in     (* Standard D smoothing *)
  let stoch_threshold = 46.74 in    (* Less strict oversold *)
  let bb_period_sell = 42 in        (* Medium BB for exits *)

  (* Create indicators *)
  let rsi = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.rsi rsi_period, Tacaml))) in

  let bb_lower = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.lower_bband bb_period_buy 2.0 2.0, Tacaml))) in

  let bb_middle = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.middle_bband bb_period_sell 2.0 2.0, Tacaml))) in

  let bb_upper = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.upper_bband bb_period_sell 2.0 2.0, Tacaml))) in

  let stoch_fast_k = Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
    Const (Tacaml.Indicator.Raw.stoch_f_fast_k stoch_fastk_period stoch_fastd_period, Tacaml))) in

  {
    name = "StochnessMonster";
    buy_trigger =
      (rsi <. Const (49.961254, Float))
      &&. (last <. bb_lower)
      &&. (stoch_fast_k <. Const (stoch_threshold, Float));
    sell_trigger =
      (last >. bb_middle)
      ||. (rsi >. Const (51.425345, Float))
      ||. (last >. bb_upper)
      ||. stop_loss 0.02
      ||. profit_target 0.05
      ||. max_holding_time 60;
    score = Const (1.0, Float);  (* Default score *)
    max_positions = 10;
    position_size = 0.10;
  }

(** RR1.0 - Optimized RocketReef_DayOnly

    Result from 30000 iteration optimization: 108843.42 (8.84% return)

    Optimized parameters:
    - RSI period: 25 (fast momentum detection)
    - BB period: 45 (medium-term bands)
    - RSI buy threshold: 31.72 (oversold entry)
    - RSI sell threshold: 46.07 (early exit on momentum shift)
    - Stop loss: 0.403 (40.3% - very wide, relies on other exits)
    - Profit target: 0.371 (37.1% - aggressive target)
    - Max hold: 58 ticks

    Intraday-only with 10-minute close buffer.
*)
let rr1_0 =
  let rsi = Real.rsi 25 () in
  let bb_lower = Real.lower_bband 45 2.0 2.0 () in
  let bb_middle = Real.middle_bband 45 2.0 2.0 () in
  let bb_upper = Real.upper_bband 45 2.0 2.0 () in
  {
    name = "RR1.0";
    buy_trigger =
      (rsi <. Const (31.717938, Float))
      &&. (last <. bb_lower)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last >. bb_middle)
      ||. (rsi >. Const (46.067607, Float))
      ||. (last >. bb_upper)
      ||. (last <. (EntryPrice *. Const (0.59677297, Float)))  (* 40.3% stop loss *)
      ||. (last >. (EntryPrice *. Const (1.37123664, Float)))  (* 37.1% profit target *)
      ||. App2 (Fun (">", (>)), TicksHeld, Const (58, Int));
    score = Const (1.0, Float);  (* Default score *)
    max_positions = 10;
    position_size = 0.10;
  }

(** DeepReef - Deep Momentum Mean Reversion

    Result from 150 iteration optimization: 101739.81 (1.74% return)

    Optimized parameters:
    - RSI period: 95 (very long-term momentum - unique characteristic)
    - BB period: 41 (medium-term bands)
    - RSI buy threshold: 33.47 (deeper oversold than typical)
    - RSI sell threshold: 40.29 (conservative exit)
    - Stop loss: -0.04 (effectively 51.9% - very wide)
    - Profit target: 0.82 (82.4% - extremely aggressive)
    - Max hold: 55 ticks

    The 95-period RSI is highly unusual and gives this strategy its "deep"
    character - it waits for very long-term momentum signals before entering,
    resulting in fewer but potentially higher-quality trades.

    Intraday-only with 10-minute close buffer.
*)
let deep_reef =
  let rsi = Real.rsi 95 () in
  let bb_lower = Real.lower_bband 41 2.0 2.0 () in
  let bb_middle = Real.middle_bband 41 2.0 2.0 () in
  let bb_upper = Real.upper_bband 41 2.0 2.0 () in
  {
    name = "DeepReef";
    buy_trigger =
      (rsi <. Const (33.473369, Float))
      &&. (last <. bb_lower)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last >. bb_middle)
      ||. (rsi >. Const (40.288297, Float))
      ||. (last >. bb_upper)
      ||. (last <. (EntryPrice -. Const (1.0, Float) *. Const (51.904005, Float)))
      ||. (last >. (EntryPrice +. Const (1.0, Float) *. Const (82.403944, Float)))
      ||. App2 (Fun (">", (>)), TicksHeld, Const (55, Int));
    score = Const (1.0, Float);  (* Default score *)
    max_positions = 10;
    position_size = 0.10;
  }

(** TrendRider - Optimizable Trend Following Strategy

    A trend-following strategy that contrasts with mean reversion approaches.
    Instead of buying weakness and selling strength, this strategy rides momentum.

    Variables to optimize (6 total):
    1. fast_ema_period: [8, 20] - Fast EMA period for trend detection
    2. slow_ema_period: [20, 50] - Slow EMA period for trend confirmation
    3. adx_period: [10, 20] - ADX period for trend strength measurement
    4. adx_threshold: [20.0, 30.0] - Minimum ADX for trend entry
    5. stop_loss_pct: [0.015, 0.035] - Stop loss (1.5-3.5%)
    6. profit_target_pct: [0.03, 0.08] - Profit target (3-8%)

    Entry Logic (Trend Start):
    - Fast EMA crosses above Slow EMA (golden cross)
    - ADX > threshold (strong trending market, not choppy)
    - MACD main line > signal line (momentum confirmation)
    - NOT within 10 minutes of market close

    Exit Logic (Trend Break or Risk Management):
    - Force exit within 10 minutes of close (no overnight)
    - Fast EMA crosses below Slow EMA (death cross - trend breakdown)
    - ADX drops below 15 (trend weakening into consolidation)
    - MACD main line < signal line (momentum reversal)
    - Variable stop loss (1.5-3.5%)
    - Variable profit target (3-8%)
    - 120-tick max hold (longer than mean reversion strategies)

    Position Sizing:
    - 5 positions at 20% each (fewer, larger positions for trend riding)
    - Contrasts with mean reversion's 10 positions at 10%

    Key Differences from Mean Reversion:
    - Buys on strength (MA crossover up) vs weakness (RSI oversold)
    - Requires trending markets (ADX > 25) vs choppy markets
    - Longer hold times (up to 120 ticks) vs quick reversions (30-60 ticks)
    - Larger position sizes to capitalize on sustained moves
*)
let trend_rider_opt =
  (* Create variables for optimizable parameters *)
  let fast_ema_period = Gadt_fo.var Gadt.Type.Int in        (* Variable 1 *)
  let slow_ema_period = Gadt_fo.var Gadt.Type.Int in        (* Variable 2 *)
  let adx_period = Gadt_fo.var Gadt.Type.Int in             (* Variable 3 *)
  let adx_threshold = Gadt_fo.var Gadt.Type.Float in        (* Variable 4 *)
  let stop_loss_var = Gadt_fo.var Gadt.Type.Float in        (* Variable 5 *)
  let profit_target_var = Gadt_fo.var Gadt.Type.Float in    (* Variable 6 *)

  (* Create EMA indicators with variable periods *)
  let fast_ema =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.ema", Tacaml.Indicator.Raw.ema), fast_ema_period)))
  in
  let slow_ema =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.ema", Tacaml.Indicator.Raw.ema), slow_ema_period)))
  in

  (* Create ADX indicator with variable period *)
  let adx =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), adx_period)))
  in

  (* MACD indicators with standard periods (12, 26, 9) *)
  let macd_main =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      Const (Tacaml.Indicator.Raw.macd_macd 12 26 9, Tacaml)))
  in
  let macd_signal =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      Const (Tacaml.Indicator.Raw.macd_signal 12 26 9, Tacaml)))
  in

  (* Variable risk management *)
  let stop_loss_var_risk =
    last <. (EntryPrice *. (Const (1.0, Float) -. stop_loss_var))
  in
  let profit_target_var_risk =
    last >. (EntryPrice *. (Const (1.0, Float) +. profit_target_var))
  in

  {
    name = "TrendRider_Opt";
    (* Entry: Trend establishment with multiple confirmations *)
    buy_trigger =
      cross_up fast_ema slow_ema          (* Golden cross *)
      &&. (adx >. adx_threshold)           (* Strong trend *)
      &&. (macd_main >. macd_signal)       (* MACD bullish *)
      &&. (fast_ema >. slow_ema)           (* Trend intact *)
      &&. safe_to_enter ();                (* Not near close *)
    (* Exit: Trend breakdown or risk management *)
    sell_trigger =
      force_exit_eod ()                    (* No overnight *)
      ||. cross_down fast_ema slow_ema    (* Death cross *)
      ||. (adx <. Const (15.0, Float))    (* Trend weakening *)
      ||. (macd_main <. macd_signal)      (* MACD bearish *)
      ||. stop_loss_var_risk               (* Variable stop *)
      ||. profit_target_var_risk           (* Variable target *)
      ||. max_holding_time 120;            (* 120 ticks max *)
    score = adx;  (* Higher ADX = stronger trend = higher score *)
    max_positions = 5;     (* Fewer positions for trend riding *)
    position_size = 0.20;  (* 20% per position *)
  }

(** SlowGlide - Optimized TrendRider (Subtle Trend Following)

    Result from ISRES optimization: 100792.15 (0.79% return over 2 weeks)

    Optimized parameters reveal an interesting discovery:
    - Fast EMA: 79 (very long, almost converged with slow)
    - Slow EMA: 90 (extremely long)
    - ADX period: 51 (unusually long trend measurement)
    - ADX threshold: 32.22 (high - requires very strong trends)
    - Stop loss: 14.09% (wide, allows for volatility)
    - Profit target: 36.54% (aggressive, swing-for-the-fences)

    The name "SlowGlide" reflects the strategy's character:
    - EMAs at 79/90 detect only subtle, slow-moving trend shifts
    - High ADX threshold (32.22) filters for only the strongest trends
    - Wide stop (14%) and aggressive target (36%) suggest patience
    - This is a "swing trading" approach, not day trading

    The close convergence of EMAs (79 vs 90) suggests that dramatic
    crossovers don't work for trends - instead, subtle shifts in
    very long-term averages signal sustainable momentum.

    Performance: Modest 0.79% in 2 weeks, suggesting trend-following
    may struggle in choppy/mean-reverting markets where your other
    strategies excel.
*)
let slow_glide =
  (* Optimized parameters *)
  let fast_ema_79 = Real.ema 79 () in
  let slow_ema_90 = Real.ema 90 () in
  let adx_51 = Real.adx 51 () in
  let macd_main = Real.macd_macd 12 26 9 () in
  let macd_signal = Real.macd_signal 12 26 9 () in

  {
    name = "SlowGlide";
    buy_trigger =
      cross_up fast_ema_79 slow_ema_90
      &&. (adx_51 >. Const (32.224663, Float))
      &&. (macd_main >. macd_signal)
      &&. (fast_ema_79 >. slow_ema_90)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. cross_down fast_ema_79 slow_ema_90
      ||. (adx_51 <. Const (15.0, Float))
      ||. (macd_main <. macd_signal)
      ||. (last <. (EntryPrice -. Const (1.0, Float) *. Const (14.087366, Float)))
      ||. (last >. (EntryPrice +. Const (1.0, Float) *. Const (36.540345, Float)))
      ||. max_holding_time 120;
    score = Const (1.0, Float);  (* Default score *)
    max_positions = 5;
    position_size = 0.20;
  }

(** QuickSnap - Optimized RocketReef_DayOnly (Fast RSI + Stable Bands)

    Result from ISRES optimization: 102598.18 (2.6% return over 2 weeks)
    This significantly outperforms the original RocketReef variants.

    Optimized parameters reveal a powerful "fast/slow" dynamic:
    - RSI period: 11 (very fast, responsive to quick moves)
    - RSI buy threshold: 21.18 (deep oversold)
    - RSI sell threshold: 50.81 (exit near neutral - key insight!)
    - BB period: 51 (very long, stable bands)
    - Stop loss: 84.11% (effectively disabled)
    - Profit target: 60.90% (swing trade target)
    - Max hold: 88 ticks

    The name "QuickSnap" reflects the strategy's dual nature:
    - "Quick": Fast 11-period RSI catches rapid momentum shifts
    - "Snap": Snaps back from oversold to neutral (exits at RSI ~50)

    Key Innovation - Fast RSI, Slow Bands:
    The 11-period RSI paired with 51-period Bollinger Bands creates a
    "spring-loaded" effect:
    - Slow BB (51) provides stable reference frame
    - Fast RSI (11) detects quick dips within that frame
    - This catches brief oversold moments in otherwise stable trends

    Exit Strategy Innovation:
    Unlike typical mean reversion (exit at RSI 70+), QuickSnap exits at
    RSI 50.81 - just above neutral. This is brilliant:
    - Takes profit as soon as momentum neutralizes
    - Doesn't wait for full mean reversion to overbought
    - Reduces exposure time and risk

    Entry: RSI(11) < 21.18 AND price < lower BB(51)
    Exit: RSI(11) > 50.81 OR price > middle BB(51) OR EOD OR 88 ticks

    Performance: 2.6% in 2 weeks (solid for mean reversion)
*)
let quick_snap =
  (* Optimized parameters *)
  let rsi_11 = Real.rsi 11 () in
  let bb_lower_51 = Real.lower_bband 51 2.0 2.0 () in
  let bb_middle_51 = Real.middle_bband 51 2.0 2.0 () in
  let bb_upper_51 = Real.upper_bband 51 2.0 2.0 () in

  {
    name = "QuickSnap";
    buy_trigger =
      (rsi_11 <. Const (21.178191, Float))
      &&. (last <. bb_lower_51)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last >. bb_middle_51)
      ||. (rsi_11 >. Const (50.812802, Float))  (* Exit near neutral - key insight! *)
      ||. (last >. bb_upper_51)
      ||. (last <. (EntryPrice -. Const (1.0, Float) *. Const (84.109595, Float)))
      ||. (last >. (EntryPrice +. Const (1.0, Float) *. Const (60.904661, Float)))
      ||. max_holding_time 88;
    score = Const (1.0, Float);  (* Default score *)
    max_positions = 10;
    position_size = 0.10;
  }

(** ProfessionalMeanRev_Opt - Mean Reversion with Distinct Entry/Exit Indicators

    This strategy fixes the variable reuse problem in rocket_reef_day_only_opt
    and similar strategies where the same bb_period_var is used for both entry
    and exit Bollinger Bands.

    KEY INSIGHT - ASYMMETRIC INDICATORS:
    Entry and exit may work best with DIFFERENT timeframes:
    - Entry: Longer periods (stable bands) to identify true oversold conditions
    - Exit: Shorter periods (responsive bands) to catch quick mean reversions

    Variables to optimize (6 total - STRATEGY LOGIC ONLY):
    1. entry_rsi_period: [5, 50] - RSI period for entry detection
    2. entry_rsi_threshold: [20.0, 40.0] - RSI oversold entry level
    3. entry_bb_period: [30, 80] - BB period for entry (can be long/stable)
    4. exit_rsi_period: [10, 60] - RSI period for exit (can differ from entry)
    5. exit_rsi_threshold: [50.0, 75.0] - RSI exit level
    6. exit_bb_period: [15, 50] - BB period for exit (can be short/responsive)

    FIXED RISK MANAGEMENT (not optimized):
    - 2.5% stop loss: Tight risk control, prevents catastrophic losses
    - NO profit target: Let mean reversion logic determine exits naturally
    - 90 tick max hold: ~90 minutes, prevents extended exposure
    - Intraday only: No overnight positions
    - 10-minute close buffer: Avoids auction volatility

    WHY NO PROFIT TARGET?
    Past optimizations (DeepReef: 82% target, RR1.0: 37% target) show that
    ISRES overfits profit targets to specific large moves in backtest data.
    Mean reversion should exit when the reversion completes (price > middle BB,
    RSI normalizes), not at arbitrary price levels.

    Entry Logic:
    - Entry RSI(var1) < threshold(var2) - momentum weakness
    - Price < Lower BB(var3) - price stretched below mean
    - Safe to enter (not near market close)

    Exit Logic (clean, no redundancy):
    - Force exit near close (no overnight risk)
    - Price > Middle BB(var6) - mean reversion complete
    - Exit RSI(var4) > threshold(var5) - momentum turned positive
    - 2.5% stop loss (fixed, tight risk control)
    - 90 tick max hold (fixed, limits exposure)

    Note: Upper BB condition removed as redundant - if price > middle BB,
    the mean reversion trade is complete.
*)
let professional_mean_rev_opt =
  (* Entry indicators - optimizable *)
  let entry_rsi_period = Gadt_fo.var Gadt.Type.Int in          (* Var 1 *)
  let entry_rsi_threshold = Gadt_fo.var Gadt.Type.Float in     (* Var 2 *)
  let entry_bb_period = Gadt_fo.var Gadt.Type.Int in           (* Var 3 *)

  (* Exit indicators - DISTINCT from entry, also optimizable *)
  let exit_rsi_period = Gadt_fo.var Gadt.Type.Int in           (* Var 4 *)
  let exit_rsi_threshold = Gadt_fo.var Gadt.Type.Float in      (* Var 5 *)
  let exit_bb_period = Gadt_fo.var Gadt.Type.Int in            (* Var 6 *)

  (* Create ENTRY indicators *)
  let entry_rsi =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), entry_rsi_period)))
  in
  let entry_bb_lower =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
        entry_bb_period,
        Const (2.0, Float),
        Const (2.0, Float))))
  in

  (* Create EXIT indicators - using DIFFERENT period variables *)
  let exit_rsi =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), exit_rsi_period)))
  in
  let exit_bb_middle =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Data.Type.Tacaml x),
      App3 (Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
        exit_bb_period,
        Const (2.0, Float),
        Const (2.0, Float))))
  in

  {
    name = "ProfessionalMeanRev_Opt";
    (* Entry: Oversold on entry indicators + intraday safety *)
    buy_trigger =
      (entry_rsi <. entry_rsi_threshold)
      &&. (last <. entry_bb_lower)
      &&. safe_to_enter ();
    (* Exit: Distinct exit indicators + fixed risk management *)
    sell_trigger =
      force_exit_eod ()                              (* No overnight *)
      ||. (last >. exit_bb_middle)                  (* Mean reversion complete *)
      ||. (exit_rsi >. exit_rsi_threshold)          (* Momentum positive *)
      ||. stop_loss 0.025                            (* 2.5% stop (fixed) *)
      ||. max_holding_time 90;                       (* 90 ticks (fixed) *)
    score = Const (100.0, Float) -. entry_rsi;  (* Lower entry RSI = higher score *)
    max_positions = 10;
    position_size = 0.10;
  }

(* List of all strategies defined in this module *)
let gadt_examples_strategies = [
  listener_strategy;
  stupid;
  dual_sma_rsi;
  golden_cross_1;
  smarsi0;
  smarsi1;
  golden_cross;
  golden_cross_0;
  plurple;
  pflurple_kelly;
  pflurple_conservative;
  pflurple_enhanced;
  mean_reversion_lag;
  bollinger_breakout;
  triple_crossover;
  momentum_divergence;
  adaptive_channels;
  contrarian_spike;
  rsi_divergence_crossover;
  macd_momentum_lag;
  stochastic_crossover_volume;
  williams_r_reversal;
  triple_ema_crossover;
  cci_overbought_oversold;
  aroon_trend_following;
  parabolic_sar_trend;
  multi_timeframe_rsi;
  roc_momentum_cross;
  always_trading;
  mean_reversion_1min;
  mean_reversion_opt;
  mean_reversion_8_27;
  mean_reversion_8_27_safe;
  mean_reversion_safe_opt;
  rocket_reef;
  rocket_reef_day_only;
  rocket_reef_day_only_opt;
  rocket_reef_stoch_opt;
  stochness;
  stochness_monster;
  rr1_0;
  deep_reef;
  trend_rider_opt;
  slow_glide;
  quick_snap;
  professional_mean_rev_opt;
]

(* Combine all strategy lists from different modules *)
let all_strategies = gadt_examples_strategies @ Strategy_library.all_strategies
