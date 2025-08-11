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

let all_strategies = !all_strategies
