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

(* List of all example strategies *)
let all_strategies =
  [
    rsi_classic;
    bollinger_mean_reversion;
    stochastic_momentum;
    volume_breakout;
    multi_candlestick;
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
