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
       buy_trigger = Bool false;
       (* Never buy *)
       sell_trigger = Bool false;
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
         close >. Float 100.0 &&. (Gadt_fo.Constant.rsi 18 >. Float 40.0);
       sell_trigger = close <. Float 100.0;
       max_positions = 1;
       position_size = 1.0;
     }

let dual_sma_rsi =
  (* Dual SMA + RSI *)
  register
  @@ {
       name = "SMARSI";
       (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
       buy_trigger = sma >. tema &&. (rsi <. Float 30.0);
       (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
       sell_trigger = sma <. tema ||. (rsi >. Float 70.0);
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
       buy_trigger = CrossUp (Real.ema 50, Real.sma 57);
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = CrossDown (Real.ema 60, Real.sma 58);
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
    buy_trigger = sma >. tema &&. (rsi <. Float 30.0);
    (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
    sell_trigger = sma <. tema ||. (rsi >. Float 70.0);
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
    buy_trigger = sma >. tema &&. (rsi <. Float 30.0);
    (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
    sell_trigger = sma <. tema ||. (rsi >. Float 70.0);
    max_positions = 2;
    position_size = 0.5;
  }

let golden_cross =
  (* Classic moving average crossover strategy *)
  register
  @@ {
       name = "GoldenCross";
       (* Buy when fast EMA crosses above slow SMA *)
       buy_trigger = CrossUp (ema, sma);
       (* Sell when fast EMA crosses below slow SMA *)
       sell_trigger = CrossDown (ema, sma);
       max_positions = 1;
       position_size = 1.0;
     }

let mean_reversion_lag =
  (* Mean reversion using lagged price comparison *)
  register
  @@ {
       name = "MeanReversionLag";
       (* Buy when current price is below lagged price by significant margin and RSI oversold *)
       buy_trigger = close <. Lag (close, 5) &&. (rsi <. Float 25.0);
       (* Sell when current price recovers above lagged SMA or RSI overbought *)
       sell_trigger = close >. Lag (sma, 3) ||. (rsi >. Float 75.0);
       max_positions = 3;
       position_size = 0.33;
     }

let bollinger_breakout =
  (* Bollinger Band breakout with momentum confirmation *)
  register
  @@ {
       name = "BollingerBreakout";
       (* Buy when price breaks above upper band and momentum is positive *)
       buy_trigger = close >. upper_bband &&. (mom >. Float 0.0);
       (* Sell when price falls back to middle band or momentum turns negative *)
       sell_trigger = close <. middle_bband ||. (mom <. Float (-1.0));
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
         ema >. sma &&. (sma >. tema) &&. (volume >. Lag (volume, 10));
       (* Sell when any MA order breaks or volume dries up *)
       sell_trigger =
         ema <. sma ||. (sma <. tema) ||. (volume <. Lag (volume, 20));
       max_positions = 1;
       position_size = 0.8;
     }

let momentum_divergence =
  (* Momentum divergence with lagged comparison *)
  register
  @@ {
       name = "MomentumDiv";
       (* Buy when price makes new low but momentum is improving (divergence) *)
       buy_trigger =
         close
         <. Lag (close, 5)
         &&. (mom >. Lag (mom, 5))
         &&. (rsi <. Float 40.0);
       (* Sell when momentum peaks or price recovers strongly *)
       sell_trigger =
         mom <. Lag (mom, 2) ||. (close >. Add (Lag (close, 10), Float 2.0));
       max_positions = 2;
       position_size = 0.4;
     }

let adaptive_channels =
  (* Adaptive channel breakout using ATR for dynamic thresholds *)
  register
  @@ {
       name = "AdaptiveChannels";
       (* Buy when price breaks above SMA + ATR channel *)
       buy_trigger = close >. Add (sma, atr);
       (* Sell when price breaks below SMA - ATR channel *)
       sell_trigger = close <. Sub (sma, atr);
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
         close <. Mul (Lag (sma, 3), Float 0.95) &&. (rsi <. Float 20.0);
       (* Sell when price recovers to normal levels or gets overbought *)
       sell_trigger = close >. Lag (sma, 1) ||. (rsi >. Float 80.0);
       max_positions = 4;
       position_size = 0.25;
     }

let rsi_divergence_crossover =
  (* RSI divergence with EMA crossover confirmation *)
  register
  @@ {
       name = "RSIDivCross";
       (* Buy when RSI crosses above 30 and EMA crosses above SMA *)
       buy_trigger = CrossUp (rsi, Float 30.0) &&. CrossUp (ema, sma);
       (* Sell when RSI crosses below 70 or EMA crosses below SMA *)
       sell_trigger = CrossDown (rsi, Float 70.0) ||. CrossDown (ema, sma);
       max_positions = 2;
       position_size = 0.5;
     }

let macd_momentum_lag =
  (* MACD with momentum and lag confirmation *)
  register
  @@ {
       name = "MACDMomLag";
       (* Buy when MACD > signal, momentum positive, and price > 5-period lag *)
       buy_trigger =
         macd_macd >. macd_signal &&. (mom >. Float 0.0)
         &&. (close >. Lag (close, 5));
       (* Sell when MACD < signal or price drops below 3-period lag SMA *)
       sell_trigger = macd_macd <. macd_signal ||. (close <. Lag (sma, 3));
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
         CrossUp (stoch_slow_k, stoch_slow_d) &&. (volume >. Lag (sma, 10));
       (* Sell when slow %K crosses below slow %D or volume dries up *)
       sell_trigger =
         CrossDown (stoch_slow_k, stoch_slow_d)
         ||. (volume <. Div (Lag (volume, 20), Float 2.0));
       max_positions = 3;
       position_size = 0.33;
     }

let bollinger_squeeze_breakout =
  (* Bollinger Band squeeze breakout with ATR expansion *)
  register
  @@ {
       name = "BBSqueezeBreak";
       (* Buy when price breaks upper band and ATR is expanding *)
       buy_trigger =
         close >. upper_bband
         &&. (atr >. Lag (atr, 5))
         &&. (volume >. Mul (Lag (volume, 10), Float 1.5));
       (* Sell when price falls back into bands or ATR contracts *)
       sell_trigger = close <. middle_bband ||. (atr <. Lag (atr, 3));
       max_positions = 1;
       position_size = 1.0;
     }

let williams_r_reversal =
  (* Williams %R mean reversion with lag confirmation *)
  register
  @@ {
       name = "WillRReversal";
       (* Buy when Williams %R oversold and price below lagged low *)
       buy_trigger =
         willr <. Float (-80.0)
         &&. (close <. Lag (low, 3))
         &&. (rsi <. Float 35.0);
       (* Sell when Williams %R overbought or price above lagged high *)
       sell_trigger = willr >. Float (-20.0) ||. (close >. Lag (high, 2));
       max_positions = 2;
       position_size = 0.6;
     }

let triple_ema_crossover =
  (* Triple EMA system with different timeframes *)
  register
  @@ {
       name = "TripleEMA";
       (* Buy when fast EMA > medium EMA > slow EMA (all aligned) *)
       buy_trigger = ema >. tema &&. (tema >. dema) &&. CrossUp (ema, tema);
       (* Sell when any EMA alignment breaks *)
       sell_trigger = ema <. tema ||. (tema <. dema) ||. CrossDown (ema, tema);
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
         cci <. Float (-100.0)
         &&. CrossUp (mom, Float 0.0)
         &&. (close >. Lag (close, 2));
       (* Sell when CCI overbought or momentum turning negative *)
       sell_trigger = cci >. Float 100.0 ||. CrossDown (mom, Float 0.0);
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
         aroon_up >. aroon_down &&. (aroon_osc >. Float 50.0)
         &&. (volume >. Lag (volume, 5));
       (* Sell when Aroon Down > Aroon Up or oscillator < -50 *)
       sell_trigger = aroon_down >. aroon_up ||. (aroon_osc <. Float (-50.0));
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
         CrossUp (close, sar)
         &&. (close >. Lag (high, 5))
         &&. (adx >. Float 25.0);
       (* Sell when price crosses below SAR *)
       sell_trigger = CrossDown (close, sar) ||. (adx <. Float 20.0);
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
         rsi <. Float 30.0
         &&. (Lag (rsi, 10) >. Lag (rsi, 20))
         &&. (close >. Lag (sma, 20));
       (* Sell when RSI overbought or long-term trend turns down *)
       sell_trigger = rsi >. Float 70.0 ||. (Lag (rsi, 5) <. Lag (rsi, 15));
       max_positions = 3;
       position_size = 0.4;
     }

let volatility_breakout_atr =
  (* Volatility breakout using ATR bands *)
  register
  @@ {
       name = "VolBreakoutATR";
       (* Buy when price breaks above SMA + 2*ATR with expanding volatility *)
       buy_trigger =
         close
         >. Add (sma, Mul (atr, Float 2.0))
         &&. (atr >. Lag (atr, 10))
         &&. (volume >. Lag (volume, 5));
       (* Sell when price falls back inside ATR bands *)
       sell_trigger = close <. Add (sma, atr) ||. (atr <. Lag (atr, 5));
       max_positions = 2;
       position_size = 0.6;
     }

let roc_momentum_cross =
  (* Rate of Change momentum crossover *)
  register
  @@ {
       name = "ROCMomentumCross";
       (* Buy when short ROC crosses above long ROC and both positive *)
       buy_trigger =
         CrossUp (roc, Lag (roc, 5))
         &&. (roc >. Float 0.0)
         &&. (Lag (roc, 5) >. Float 0.0);
       (* Sell when short ROC crosses below long ROC *)
       sell_trigger = CrossDown (roc, Lag (roc, 5)) ||. (roc <. Float (-2.0));
       max_positions = 2;
       position_size = 0.5;
     }

let all_strategies = !all_strategies
