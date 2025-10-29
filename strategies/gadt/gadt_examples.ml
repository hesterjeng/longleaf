(* open Gadt_fo.Variable *)
open Gadt
open Gadt_strategy

let all_strategies : Gadt_strategy.t List.Ref.t = List.Ref.create ()

let register x =
  List.Ref.push all_strategies x;
  x

(* Risk management helper functions - return bool Gadt.t expressions *)

(* Stop-loss: sell if current price is below entry price by stop_loss_pct (as decimal) *)
let stop_loss stop_loss_pct : bool Gadt.t =
  (* current_price < entry_price * (1 - stop_loss) *)
  let multiplier = Float.((-) 1.0 stop_loss_pct) in
  last <. (EntryPrice *. Const (multiplier, Float))

(* Profit target: sell if current price is above entry price by profit_target_pct (as decimal) *)
let profit_target profit_target_pct : bool Gadt.t =
  (* current_price > entry_price * (1 + profit_target) *)
  let multiplier = Float.((+) 1.0 profit_target_pct) in
  last >. (EntryPrice *. Const (multiplier, Float))

(* Max holding time: sell if held for more than max_ticks *)
let max_holding_time max_ticks : bool Gadt.t =
  (* Need to compare TicksHeld (int Gadt.t) with max_ticks - create custom comparison *)
  let max_ticks_expr = Const (max_ticks, Int) in
  (* Create a Fun that compares the two int values *)
  App2 (Fun (">", (>)), TicksHeld, max_ticks_expr)

(** A data collection listener strategy that never buys or sells *)
let listener_strategy =
  register
  @@ {
       name = "Listener";
       buy_trigger = Const (0.0, Float) <. Const (0.0, Float);
       (* Never buy *)
       sell_trigger = Const (0.0, Float) <. Const (0.0, Float);
       (* Never sell *)
       max_positions = 0;
       (* No positions allowed *)
       position_size = 0.0;
       (* No position sizing needed *)
     }

let stupid =
  register
  @@ {
       name = "Dumb strategy";
       buy_trigger =
         close
         >. Const (100.0, Float)
         &&. (Gadt_fo.Constant.rsi 18 () >. Const (40.0, Float));
       sell_trigger = close <. Const (100.0, Float);
       max_positions = 1;
       position_size = 1.0;
     }

let dual_sma_rsi =
  (* Dual SMA + RSI *)
  register
  @@ {
       name = "SEAKING";
       (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
       buy_trigger =
         Gadt_fo.Variable.sma () >. Gadt_fo.Variable.tema ()
         &&. (Gadt_fo.Variable.rsi () <. Const (30.0, Float));
       (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
       sell_trigger =
         Gadt_fo.Variable.sma () <. Gadt_fo.Variable.tema ()
         ||. (Gadt_fo.Variable.rsi () >. Const (70.0, Float));
       max_positions = 2;
       position_size = 0.5;
     }

module Real = Gadt_fo.Constant

let golden_cross_1 =
  (* Classic moving average crossover strategy *)
  register
  @@ {
       name = "GCONST";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up (Real.ema 50 ()) (Real.sma 57 ());
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down (Real.ema 60 ()) (Real.sma 58 ());
       max_positions = 1;
       position_size = 1.0;
     }

let smarsi0 =
  (* Dual SMA + RSI *)
  register
  @@
  let sma = Real.sma 10 () in
  let tema = Real.tema 8 () in
  let rsi = Real.rsi 4 () in
  {
    name = "SMARSI0";
    (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
    buy_trigger = sma >. tema &&. (rsi <. Const (30.0, Float));
    (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
    sell_trigger = sma <. tema ||. (rsi >. Const (70.0, Float));
    max_positions = 2;
    position_size = 0.5;
  }

let smarsi1 =
  (* Dual SMA + RSI *)
  register
  @@
  let sma = Real.sma 12 () in
  let tema = Real.tema 13 () in
  let rsi = Real.rsi 7 () in
  {
    name = "SMARSI1";
    (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
    buy_trigger = sma >. tema &&. (rsi <. Const (30.0, Float));
    (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
    sell_trigger = sma <. tema ||. (rsi >. Const (70.0, Float));
    max_positions = 2;
    position_size = 0.5;
  }

let golden_cross =
  (* Classic moving average crossover strategy *)
  register
  @@ {
       name = "GoldenCross";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger =
         cross_up (Gadt_fo.Variable.ema ()) (Gadt_fo.Variable.sma ());
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger =
         cross_down (Gadt_fo.Variable.ema ()) (Gadt_fo.Variable.sma ());
       max_positions = 1;
       position_size = 1.0;
     }

let golden_cross_0 =
  (* Classic moving average crossover strategy *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in
  register
  @@ {
       name = "GoldenCrossSa";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down ema sma;
       max_positions = 1;
       position_size = 1.0;
     }

let plurple =
  (* Classic moving average crossover strategy *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in
  register
  @@ {
       name = "Pflurple";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below SMA *)
       sell_trigger = cross_down ema sma;
       max_positions = 8;
       position_size = 0.5;
     }

let pflurple_kelly =
  (* Improved Pflurple using Kelly-inspired position sizing *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in
  register
  @@ {
       name = "PflurpleKelly";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down ema sma;
       max_positions = 16;
       (* More diversification *)
       position_size = 0.125;
       (* 12.5% per position = 200% max allocation *)
     }

let pflurple_conservative =
  (* Conservative version with proper risk management *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in
  register
  @@ {
       name = "PflurpleConservative";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down ema sma;
       max_positions = 20;
       (* Maximum diversification *)
       position_size = 0.08;
       (* 8% per position = 160% max allocation *)
     }

let pflurple_enhanced =
  (* Enhanced version with momentum filter *)
  let ema = Real.ema 58 () in
  let sma = Real.sma 36 () in
  register
  @@ {
       name = "PflurpleEnhanced";
       (* Buy when EMA crosses SMA AND momentum is positive *)
       buy_trigger = cross_up ema sma &&. (Real.mom 10 () >. Const (0.0, Float));
       (* Sell when EMA crosses below SMA OR momentum turns negative *)
       sell_trigger =
         cross_down ema sma ||. (Real.mom 10 () <. Const (-1.0, Float));
       max_positions = 12;
       position_size = 0.15;
       (* 15% per position = 180% max allocation *)
     }

let mean_reversion_lag =
  (* Mean reversion using lagged price comparison *)
  register
  @@ {
       name = "MeanReversionlag";
       (* Buy when current price is below lagged price by significant margin and RSI oversold *)
       buy_trigger =
         close <. lag close 5 &&. (Real.rsi 14 () <. Const (25.0, Float));
       (* Sell when current price recovers above lagged SMA or RSI overbought *)
       sell_trigger =
         close
         >. lag (Real.sma 20 ()) 3
         ||. (Real.rsi 14 () >. Const (75.0, Float));
       max_positions = 3;
       position_size = 0.33;
     }

let bollinger_breakout =
  (* Bollinger Band breakout with momentum confirmation *)
  register
  @@ {
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
       max_positions = 2;
       position_size = 0.6;
     }

let triple_crossover =
  (* Complex triple moving average system with volume confirmation *)
  register
  @@ {
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
       max_positions = 1;
       position_size = 0.8;
     }

let momentum_divergence =
  (* Momentum divergence with lagged comparison *)
  register
  @@ {
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
       max_positions = 2;
       position_size = 0.4;
     }

let adaptive_channels =
  (* Adaptive channel breakout using ATR for dynamic thresholds *)
  register
  @@ {
       name = "AdaptiveChannels";
       (* Buy when price breaks above SMA + ATR channel *)
       buy_trigger = close >. Real.sma 20 () +. Real.atr 14 ();
       (* Sell when price breaks below SMA - ATR channel *)
       sell_trigger = close <. Real.sma 20 () -. Real.atr 14 ();
       max_positions = 1;
       position_size = 1.0;
     }

let contrarian_spike =
  (* Contrarian strategy for price spikes with lag confirmation *)
  register
  @@ {
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
       max_positions = 4;
       position_size = 0.25;
     }

let rsi_divergence_crossover =
  (* RSI divergence with EMA crossover confirmation *)
  register
  @@ {
       name = "RSI(/.)Cross";
       (* Buy when RSI crosses above 30 and EMA crosses above SMA *)
       buy_trigger =
         cross_up (Real.rsi 14 ()) (Const (30.0, Float))
         &&. cross_up (Gadt_fo.Variable.ema ()) (Gadt_fo.Variable.sma ());
       (* Sell when RSI crosses below 70 or EMA crosses below SMA *)
       sell_trigger =
         cross_down (Real.rsi 14 ()) (Const (70.0, Float))
         ||. cross_down (Gadt_fo.Variable.ema ()) (Gadt_fo.Variable.sma ());
       max_positions = 2;
       position_size = 0.5;
     }

let macd_momentum_lag =
  (* MACD with momentum and lag confirmation *)
  register
  @@ {
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
       max_positions = 1;
       position_size = 0.8;
     }

let stochastic_crossover_volume =
  (* Stochastic crossover with volume confirmation *)
  register
  @@ {
       name = "StochCrossVol";
       (* Buy when slow %K crosses above slow %D and volume > 10-period average *)
       buy_trigger =
         cross_up (Real.stoch_slow_k 14 3 3 ()) (Real.stoch_slow_d 14 3 3 ())
         &&. (volume >. lag (Real.sma 10 ()) 10);
       (* Sell when slow %K crosses below slow %D or volume dries up *)
       sell_trigger =
         cross_down (Real.stoch_slow_k 14 3 3 ()) (Real.stoch_slow_d 14 3 3 ())
         ||. (volume <. lag volume 20 /. Const (2.0, Float));
       max_positions = 3;
       position_size = 0.33;
     }

let williams_r_reversal =
  (* Williams %R mean reversion with lag confirmation *)
  register
  @@ {
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
       max_positions = 2;
       position_size = 0.6;
     }

let triple_ema_crossover =
  (* Triple EMA system with different timeframes *)
  register
  @@ {
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
       max_positions = 1;
       position_size = 0.9;
     }

let cci_overbought_oversold =
  (* CCI overbought/oversold with momentum filter *)
  register
  @@ {
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
       max_positions = 2;
       position_size = 0.5;
     }

let aroon_trend_following =
  (* Aroon oscillator trend following with volume *)
  register
  @@ {
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
       max_positions = 1;
       position_size = 0.8;
     }

let parabolic_sar_trend =
  (* Parabolic SAR trend following with lag confirmation *)
  register
  @@ {
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
       max_positions = 1;
       position_size = 1.0;
     }

let multi_timeframe_rsi =
  (* Multi-timeframe RSI strategy using different lag periods *)
  register
  @@ {
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
       max_positions = 3;
       position_size = 0.4;
     }

let roc_momentum_cross =
  (* Rate of Change momentum crossover *)
  register
  @@ {
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
       max_positions = 2;
       position_size = 0.5;
     }

let always_trading =
  (* Maximum churn strategy - always generates buy and sell signals for all symbols *)
  register
  @@ {
       name = "AlwaysTrading";
       (* Always buy - price is always >= 0 *)
       buy_trigger = close >. Const (0.0, Float);
       (* Always sell - price is always >= 0 *)
       sell_trigger = close >. Const (0.0, Float);
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
  register
  @@ {
       name = "MeanReversion1Min";
       (* Buy: Oversold (RSI < 30) AND price touched lower Bollinger Band *)
       buy_trigger =
         (rsi_2 <. Const (30.0, Float)) &&. (last <. bb_lower);
       (* Sell: Return to mean OR overbought OR price hit upper band *)
       sell_trigger =
         (last >. bb_middle)  (* Price returned to mean *)
         ||. (rsi_2 >. Const (70.0, Float))  (* Overbought *)
         ||. (last >. bb_upper);  (* Overshot to upper band *)
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
  register
  @@ {
       name = "MeanReversionOpt";
       (* Buy: Oversold RSI AND price touched lower Bollinger Band *)
       buy_trigger =
         (rsi_var <. Const (30.0, Float)) &&. (last <. bb_lower_var);
       (* Sell: Return to mean OR overbought OR price hit upper band *)
       sell_trigger =
         (last >. bb_middle_var)
         ||. (rsi_var >. Const (70.0, Float))
         ||. (last >. bb_upper_var);
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
  register
  @@ {
       name = "MeanReversion_8_27";
       buy_trigger =
         (rsi_8 <. Const (30.0, Float)) &&. (last <. bb_lower_27);
       sell_trigger =
         (last >. bb_middle_27)
         ||. (rsi_8 >. Const (70.0, Float))
         ||. (last >. bb_upper_27);
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
  register
  @@ {
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
       max_positions = 10;
       position_size = 0.10;
     }

(** MeanReversion_Safe_Opt - Optimizable Version with Fixed Risk Controls

    ISRES-optimizable version of the safe mean reversion strategy.
    Indicator periods and thresholds are variables; risk management is fixed.

    Variables to optimize (4 total):
    1. rsi_period: [5, 15] - RSI lookback period
    2. bb_period: [15, 35] - Bollinger Band period
    3. rsi_oversold: [20.0, 35.0] - RSI buy threshold
    4. rsi_overbought: [65.0, 80.0] - RSI sell threshold

    Fixed risk management:
    - 2% stop-loss
    - 5% profit target
    - 60-minute maximum holding time

    Entry: RSI(var) < var AND Last < BB_lower(var)
    Exit: Signals OR stop-loss(fixed) OR profit(fixed) OR max_hold(fixed)
*)
let mean_reversion_safe_opt =
  (* Create variables for optimizable parameters *)
  let rsi_period_var = Gadt_fo.var Gadt.Type.Int in       (* Variable 1: RSI period *)
  let bb_period_var = Gadt_fo.var Gadt.Type.Int in        (* Variable 2: BB period *)
  let rsi_oversold_var = Gadt_fo.var Gadt.Type.Float in   (* Variable 3: RSI oversold threshold *)
  let rsi_overbought_var = Gadt_fo.var Gadt.Type.Float in (* Variable 4: RSI overbought threshold *)

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

  register
  @@ {
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
       max_positions = 10;
       position_size = 0.10;
     }

let all_strategies = !all_strategies
