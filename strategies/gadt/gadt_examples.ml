(* open Gadt_fo.Variable *)
open Gadt
open Gadt_strategy

let all_strategies : Gadt_strategy.t List.Ref.t = List.Ref.create ()

let register x =
  List.Ref.push all_strategies x;
  x

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

let all_strategies = !all_strategies
