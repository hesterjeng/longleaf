open Gadt

(* Strategy 1: Classic RSI Oversold/Overbought *)
let rsi_classic =
  {
    name = "RSI Classic";
    buy_trigger = rsi <. Float 30.0;
    sell_trigger = rsi >. Float 70.0;
    max_positions = 3;
    position_size = 0.33;
  }

(* Strategy 2: Bollinger Band Mean Reversion *)
let bollinger_mean_reversion =
  {
    name = "Bollinger Mean Reversion";
    buy_trigger = close <. bb_lower &&. (rsi <. Float 50.0);
    sell_trigger = close >. bb_upper ||. (close >. bb_middle);
    max_positions = 4;
    position_size = 0.25;
  }

(* Strategy 3: Stochastic Momentum *)
let stochastic_momentum =
  {
    name = "Stochastic Momentum";
    buy_trigger =
      stoch_k >. stoch_d &&. (stoch_k <. Float 20.0) &&. (adx >. Float 25.0);
    sell_trigger = stoch_k <. stoch_d ||. (stoch_k >. Float 80.0);
    max_positions = 5;
    position_size = 0.2;
  }

(* Strategy 4: Volume Breakout with Trend Filter *)
let volume_breakout =
  {
    name = "Volume Breakout";
    buy_trigger =
      close >. sma
      &&. (volume >. volume *. Float 2.0)
      &&. (atr >. atr *. Float 1.5);
    sell_trigger = close <. ema ||. (volume <. volume *. Float 0.5);
    max_positions = 3;
    position_size = 0.33;
  }

(* Strategy 5: Multi-Pattern Candlestick *)
let multi_candlestick =
  {
    name = "Multi Candlestick";
    buy_trigger =
      IntGT (hammer, Int 0)
      ||. IntGT (morning_star, Int 0)
      ||. IntGT (piercing, Int 0)
      &&. (rsi <. Float 45.0) &&. (close >. sma);
    sell_trigger =
      IntLT (shooting_star, Int 0)
      ||. IntLT (evening_star, Int 0)
      ||. IntLT (dark_cloud, Int 0)
      ||. (rsi >. Float 65.0)
      ||. (close <. close *. Float 0.95);
    max_positions = 6;
    position_size = 0.16;
  }

(* Strategy 6: Always Buy - Test system limits *)
let always_buy =
  {
    name = "Always Buy";
    buy_trigger = Bool true;
    sell_trigger = Bool false;
    (* Never sell *)
    max_positions = 10;
    position_size = 0.1;
  }

(* Strategy 7: Never Buy - Test system edge case *)
let never_buy =
  {
    name = "Never Buy";
    buy_trigger = Bool false;
    sell_trigger = Bool true;
    (* Always sell (but nothing to sell) *)
    max_positions = 1;
    position_size = 1.0;
  }

(* Strategy 8: Buy High Sell Low - Anti-strategy to test losses *)
let buy_high_sell_low =
  {
    name = "Buy High Sell Low";
    buy_trigger = rsi >. Float 80.0 &&. (close >. bb_upper);
    sell_trigger = rsi <. Float 20.0 &&. (close <. bb_lower);
    max_positions = 5;
    position_size = 0.2;
  }

(* Strategy 9: Extreme Position Sizing - Test large positions *)
let extreme_sizing =
  {
    name = "Extreme Sizing";
    buy_trigger = rsi <. Float 50.0;
    sell_trigger = rsi >. Float 50.0;
    max_positions = 1;
    position_size = 1.0;
    (* All-in on each trade *)
  }

(* Strategy 10: Micro Positions - Test tiny positions *)
let micro_positions =
  {
    name = "Micro Positions";
    buy_trigger = close >. sma;
    sell_trigger = close <. sma;
    max_positions = 100;
    position_size = 0.001;
    (* 0.1% per position *)
  }

(* Strategy 11: High Frequency - Test many trades *)
let high_frequency =
  {
    name = "High Frequency";
    buy_trigger = close >. close +. Float 0.01;
    (* Price moved up by $0.01 *)
    sell_trigger = close <. close -. Float 0.01;
    (* Price moved down by $0.01 *)
    max_positions = 20;
    position_size = 0.05;
  }

(* Strategy 12: Mathematical Stress Test - Complex expressions *)
let math_stress_test =
  {
    name = "Math Stress Test";
    buy_trigger =
      (rsi *. Float 0.5) +. (adx *. Float 0.3)
      >. Float 40.0
      &&. ((bb_upper -. bb_lower) /. bb_middle >. Float 0.02)
      &&. (close /. sma >. Float 1.01);
    sell_trigger =
      (rsi *. Float 0.7) -. (adx *. Float 0.2)
      <. Float 30.0
      ||. ((high -. low) /. close >. Float 0.05);
    max_positions = 8;
    position_size = 0.125;
  }

(* Strategy 13: Multi-Indicator Consensus - Test many conditions *)
let multi_indicator_consensus =
  {
    name = "Multi Indicator Consensus";
    buy_trigger =
      rsi >. Float 40.0 &&. (rsi <. Float 60.0) &&. (macd >. macd_signal)
      &&. (stoch_k >. stoch_d) &&. (adx >. Float 20.0) &&. (close >. sma)
      &&. (close >. ema)
      &&. (volume >. volume *. Float 1.1)
      &&. (atr >. atr *. Float 0.8);
    sell_trigger =
      rsi <. Float 35.0 ||. (rsi >. Float 65.0) ||. (macd <. macd_signal)
      ||. (stoch_k <. stoch_d) ||. (adx <. Float 15.0) ||. (close <. sma)
      ||. (volume <. volume *. Float 0.9);
    max_positions = 3;
    position_size = 0.33;
  }

(* Strategy 14: Pure Candlestick Patterns - Test all patterns *)
let pure_candlestick =
  {
    name = "Pure Candlestick";
    buy_trigger =
      IntGT (hammer, Int 0)
      ||. IntGT (morning_star, Int 0)
      ||. IntGT (piercing, Int 0)
      ||. IntGT (doji, Int 0);
    sell_trigger =
      IntLT (shooting_star, Int 0)
      ||. IntLT (evening_star, Int 0)
      ||. IntLT (dark_cloud, Int 0)
      ||. IntLT (hanging_man, Int 0);
    max_positions = 12;
    position_size = 0.08;
  }

(* Strategy 15: Contrarian Extreme - Buy on extreme fear *)
let contrarian_extreme =
  {
    name = "Contrarian Extreme";
    buy_trigger =
      rsi <. Float 10.0 &&. (willr <. Float (-95.0))
      &&. (close <. bb_lower -. ((bb_upper -. bb_lower) *. Float 0.5));
    sell_trigger =
      rsi >. Float 90.0 ||. (willr >. Float (-5.0))
      ||. (close >. bb_upper +. ((bb_upper -. bb_lower) *. Float 0.5));
    max_positions = 2;
    position_size = 0.5;
  }

(* Strategy 16: Extended Candlestick - More patterns, same structure *)
let extended_candlestick =
  {
    name = "Extended Candlestick";
    buy_trigger =
      IntGT (hammer, Int 0)
      ||. IntGT (morning_star, Int 0)
      ||. IntGT (piercing, Int 0)
      ||. IntGT (doji, Int 0)
      ||. IntGT (inverted_hammer, Int 0)
      ||. IntGT (dragonfly_doji, Int 0)
      ||. IntGT (three_white_soldiers, Int 0)
      ||. IntGT (belt_hold, Int 0)
      ||. IntGT (harami, Int 0);
    sell_trigger =
      IntLT (shooting_star, Int 0)
      ||. IntLT (evening_star, Int 0)
      ||. IntLT (dark_cloud, Int 0)
      ||. IntLT (hanging_man, Int 0)
      ||. IntLT (gravestone_doji, Int 0)
      ||. IntLT (three_black_crows, Int 0)
      ||. IntLT (abandoned_baby, Int 0)
      ||. IntLT (harami_cross, Int 0);
    max_positions = 12;
    position_size = 0.08;
  }

(* Strategy 17: Reordered Candlestick - Same patterns, different order *)
let reordered_candlestick =
  {
    name = "Reordered Candlestick";
    buy_trigger =
      IntGT (doji, Int 0)
      ||. IntGT (piercing, Int 0)
      ||. IntGT (hammer, Int 0)
      ||. IntGT (morning_star, Int 0);
    sell_trigger =
      IntLT (hanging_man, Int 0)
      ||. IntLT (dark_cloud, Int 0)
      ||. IntLT (evening_star, Int 0)
      ||. IntLT (shooting_star, Int 0);
    max_positions = 12;
    position_size = 0.08;
  }

(* Strategy 18: Minimal Candlestick - Just 2 patterns *)
let minimal_candlestick =
  {
    name = "Minimal Candlestick";
    buy_trigger = IntGT (hammer, Int 0) ||. IntGT (morning_star, Int 0);
    sell_trigger = IntLT (shooting_star, Int 0) ||. IntLT (evening_star, Int 0);
    max_positions = 12;
    position_size = 0.08;
  }

(* Strategy 19: Single Pattern Test - Just hammer/shooting star *)
let single_pattern =
  {
    name = "Single Pattern";
    buy_trigger = IntGT (hammer, Int 0);
    sell_trigger = IntLT (shooting_star, Int 0);
    max_positions = 12;
    position_size = 0.08;
  }

(* Strategy 20: Doji Only - Test doji patterns specifically *)
let doji_only =
  {
    name = "Doji Only";
    buy_trigger = IntGT (doji, Int 0) ||. IntGT (dragonfly_doji, Int 0);
    sell_trigger = IntLT (doji, Int 0) ||. IntLT (gravestone_doji, Int 0);
    max_positions = 12;
    position_size = 0.08;
  }

(* Strategy 21: Conservative Candlestick - Larger positions, fewer max *)
let conservative_candlestick =
  {
    name = "Conservative Candlestick";
    buy_trigger =
      IntGT (hammer, Int 0)
      ||. IntGT (morning_star, Int 0)
      ||. IntGT (piercing, Int 0)
      ||. IntGT (doji, Int 0);
    sell_trigger =
      IntLT (shooting_star, Int 0)
      ||. IntLT (evening_star, Int 0)
      ||. IntLT (dark_cloud, Int 0)
      ||. IntLT (hanging_man, Int 0);
    max_positions = 5;
    (* Fewer positions *)
    position_size = 0.15;
    (* Larger size per position *)
  }

(* Strategy 22: Aggressive Candlestick - Many small positions *)
let aggressive_candlestick =
  {
    name = "Aggressive Candlestick";
    buy_trigger =
      IntGT (hammer, Int 0)
      ||. IntGT (morning_star, Int 0)
      ||. IntGT (piercing, Int 0)
      ||. IntGT (doji, Int 0);
    sell_trigger =
      IntLT (shooting_star, Int 0)
      ||. IntLT (evening_star, Int 0)
      ||. IntLT (dark_cloud, Int 0)
      ||. IntLT (hanging_man, Int 0);
    max_positions = 25;
    (* More positions *)
    position_size = 0.03;
    (* Smaller size per position *)
  }

let moving_average_crossover =
  {
    name = "SMA/EMA Crossover";
    buy_trigger =
      cross_up sma ema (* Fast SMA crosses above slow EMA *)
      &&. (rsi >. Float 30.0) (* Not oversold *)
      &&. (volume >. volume_sma *. Float 1.1);
    sell_trigger =
      cross_down sma ema (* Fast SMA crosses below slow EMA *)
      ||. (rsi >. Float 80.0) (* Overbought *)
      ||. (close <. lag close 10 *. Float 0.95);
    (* 5% stop loss from 10 periods ago *)
    max_positions = 3;
    position_size = 0.33;
  }

(* List of all example strategies *)
let all_strategies =
  [
    rsi_classic;
    bollinger_mean_reversion;
    stochastic_momentum;
    volume_breakout;
    multi_candlestick;
    always_buy;
    never_buy;
    buy_high_sell_low;
    extreme_sizing;
    micro_positions;
    high_frequency;
    math_stress_test;
    multi_indicator_consensus;
    pure_candlestick;
    contrarian_extreme;
    extended_candlestick;
    reordered_candlestick;
    minimal_candlestick;
    single_pattern;
    doji_only;
    conservative_candlestick;
    aggressive_candlestick;
    moving_average_crossover;
  ]

(* Helper function to run any strategy by name *)
(* let run_by_name name options = *)
(*   match List.find_opt (fun s -> String.equal s.name name) all_strategies with *)
(*   | Some strategy -> Gadt.run options strategy *)
(*   | None -> Error.fatal ("Unknown GADT strategy: " ^ name) *)

(* (\* Test runner for all strategies *\) *)
(* let test_all options = *)
(*   List.fold_left (fun acc strategy -> *)
(*     let result = Gadt.run options strategy in *)
(*     (strategy.name, result) :: acc *)
(*   ) [] all_strategies *)
