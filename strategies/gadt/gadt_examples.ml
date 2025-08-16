open Gadt_fo.Variable
open Gadt

let all_strategies : Gadt.strategy List.Ref.t = List.Ref.create ()

let register x =
  List.Ref.push all_strategies x;
  x

(** A data collection listener strategy that never buys or sells *)
let listener_strategy =
  register
  @@ {
       name = "Listener";
       buy_trigger = Const false;
       (* Never buy *)
       sell_trigger = Const false;
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
         close >. Const 100.0 &&. (Gadt_fo.Constant.rsi 18 >. Const 40.0);
       sell_trigger = close <. Const 100.0;
       max_positions = 1;
       position_size = 1.0;
     }

let dual_sma_rsi =
  (* Dual SMA + RSI *)
  register
  @@ {
       name = "SMARSI";
       (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
       buy_trigger = sma >. tema &&. (rsi <. Const 30.0);
       (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
       sell_trigger = sma <. tema ||. (rsi >. Const 70.0);
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
       buy_trigger = cross_up (Real.ema 50) (Real.sma 57);
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down (Real.ema 60) (Real.sma 58);
       max_positions = 1;
       position_size = 1.0;
     }

let smarsi0 =
  (* Dual SMA + RSI *)
  register
  @@
  let sma = Real.sma 10 in
  let tema = Real.tema 8 in
  let rsi = Real.rsi 4 in
  {
    name = "SMARSI0";
    (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
    buy_trigger = sma >. tema &&. (rsi <. Const 30.0);
    (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
    sell_trigger = sma <. tema ||. (rsi >. Const 70.0);
    max_positions = 2;
    position_size = 0.5;
  }

let smarsi1 =
  (* Dual SMA + RSI *)
  register
  @@
  let sma = Real.sma 12 in
  let tema = Real.tema 13 in
  let rsi = Real.rsi 7 in
  {
    name = "SMARSI1";
    (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
    buy_trigger = sma >. tema &&. (rsi <. Const 30.0);
    (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
    sell_trigger = sma <. tema ||. (rsi >. Const 70.0);
    max_positions = 2;
    position_size = 0.5;
  }

let golden_cross =
  (* Classic moving average crossover strategy *)
  register
  @@ {
       name = "GoldenCross";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = cross_up ema sma;
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = cross_down ema sma;
       max_positions = 1;
       position_size = 1.0;
     }

let mean_reversion_lag =
  (* Mean reversion using lagged price comparison *)
  register
  @@ {
       name = "MeanReversionlag";
       (* Buy when current price is below lagged price by significant margin and RSI oversold *)
       buy_trigger = close <. lag close 5 &&. (rsi <. Const 25.0);
       (* Sell when current price recovers above lagged SMA or RSI overbought *)
       sell_trigger = close >. lag sma 3 ||. (rsi >. Const 75.0);
       max_positions = 3;
       position_size = 0.33;
     }

let bollinger_breakout =
  (* Bollinger Band breakout with momentum confirmation *)
  register
  @@ {
       name = "BollingerBreakout";
       (* Buy when price breaks above upper band and momentum is positive *)
       buy_trigger = close >. upper_bband &&. (mom >. Const 0.0);
       (* Sell when price falls back to middle band or momentum turns negative *)
       sell_trigger = close <. middle_bband ||. (mom <. Const (-1.0));
       max_positions = 2;
       position_size = 0.6;
     }

let triple_crossover =
  (* Complex triple moving average system with volume confirmation *)
  register
  @@ {
       name = "TripleCross";
       (* Buy when fast > medium > slow MA and volume above average *)
       buy_trigger = ema >. sma &&. (sma >. tema) &&. (volume >. lag volume 10);
       (* Sell when any MA order breaks or volume dries up *)
       sell_trigger = ema <. sma ||. (sma <. tema) ||. (volume <. lag volume 20);
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
         close <. lag close 5 &&. (mom >. lag mom 5) &&. (rsi <. Const 40.0);
       (* Sell when momentum peaks or price recovers strongly *)
       sell_trigger = mom <. lag mom 2 ||. (close >. lag close 10 +. Const 2.0);
       max_positions = 2;
       position_size = 0.4;
     }

let adaptive_channels =
  (* Adaptive channel breakout using ATR for dynamic thresholds *)
  register
  @@ {
       name = "AdaptiveChannels";
       (* Buy when price breaks above SMA + ATR channel *)
       buy_trigger = close >. sma +. atr;
       (* Sell when price breaks below SMA - ATR channel *)
       sell_trigger = close <. sma -. atr;
       max_positions = 1;
       position_size = 1.0;
     }

let contrarian_spike =
  (* Contrarian strategy for price spikes with lag confirmation *)
  register
  @@ {
       name = "ContrarianSpike";
       (* Buy when price spikes down significantly from recent average *)
       buy_trigger = close <. lag sma 3 *. Const 0.95 &&. (rsi <. Const 20.0);
       (* Sell when price recovers to normal levels or gets overbought *)
       sell_trigger = close >. lag sma 1 ||. (rsi >. Const 80.0);
       max_positions = 4;
       position_size = 0.25;
     }

let rsi_divergence_crossover =
  (* RSI divergence with EMA crossover confirmation *)
  register
  @@ {
       name = "RSI(/.)Cross";
       (* Buy when RSI crosses above 30 and EMA crosses above SMA *)
       buy_trigger = cross_up rsi (Const 30.0) &&. cross_up ema sma;
       (* Sell when RSI crosses below 70 or EMA crosses below SMA *)
       sell_trigger = cross_down rsi (Const 70.0) ||. cross_down ema sma;
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
         macd_macd >. macd_signal &&. (mom >. Const 0.0)
         &&. (close >. lag close 5);
       (* Sell when MACD < signal or price drops below 3-period lag SMA *)
       sell_trigger = macd_macd <. macd_signal ||. (close <. lag sma 3);
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
         cross_up stoch_slow_k stoch_slow_d &&. (volume >. lag sma 10);
       (* Sell when slow %K crosses below slow %D or volume dries up *)
       sell_trigger =
         cross_down stoch_slow_k stoch_slow_d
         ||. (volume <. lag volume 20 /. Const 2.0);
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
         willr <. Const (-80.0) &&. (close <. lag low 3) &&. (rsi <. Const 35.0);
       (* Sell when Williams %R overbought or price above lagged high *)
       sell_trigger = willr >. Const (-20.0) ||. (close >. lag high 2);
       max_positions = 2;
       position_size = 0.6;
     }

let triple_ema_crossover =
  (* Triple EMA system with different timeframes *)
  register
  @@ {
       name = "TripleEMA";
       (* Buy when fast EMA > medium EMA > slow EMA (all aligned) *)
       buy_trigger = ema >. tema &&. (tema >. dema) &&. cross_up ema tema;
       (* Sell when any EMA alignment breaks *)
       sell_trigger = ema <. tema ||. (tema <. dema) ||. cross_down ema tema;
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
         cci <. Const (-100.0) &&. cross_up mom (Const 0.0)
         &&. (close >. lag close 2);
       (* Sell when CCI overbought or momentum turning negative *)
       sell_trigger = cci >. Const 100.0 ||. cross_down mom (Const 0.0);
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
         aroon_up >. aroon_down &&. (aroon_osc >. Const 50.0)
         &&. (volume >. lag volume 5);
       (* Sell when Aroon Down > Aroon Up or oscillator < -50 *)
       sell_trigger = aroon_down >. aroon_up ||. (aroon_osc <. Const (-50.0));
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
         cross_up close sar &&. (close >. lag high 5) &&. (adx >. Const 25.0);
       (* Sell when price crosses below SAR *)
       sell_trigger = cross_down close sar ||. (adx <. Const 20.0);
       max_positions = 1;
       position_size = 1.0;
     }

let multi_timeframe_rsi =
  (* ( *. )ti-timeframe RSI strategy using different lag periods *)
  register
  @@ {
       name = "( *. )tiTimeframeRSI";
       (* Buy when short-term RSI oversold but long-term trend up *)
       buy_trigger =
         rsi <. Const 30.0
         &&. (lag rsi 10 >. lag rsi 20)
         &&. (close >. lag sma 20);
       (* Sell when RSI overbought or long-term trend turns down *)
       sell_trigger = rsi >. Const 70.0 ||. (lag rsi 5 <. lag rsi 15);
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
         cross_up roc (lag roc 5)
         &&. (roc >. Const 0.0)
         &&. (lag roc 5 >. Const 0.0);
       (* Sell when short ROC crosses below long ROC *)
       sell_trigger = cross_down roc (lag roc 5) ||. (roc <. Const (-2.0));
       max_positions = 2;
       position_size = 0.5;
     }

let all_strategies = !all_strategies
