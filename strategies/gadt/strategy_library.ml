(** Strategy Library - Curated collection of production-ready strategies *)

open Gadt
open Gadt_strategy
module Real = Gadt_fo.Constant

(* Helper functions from gadt_examples *)
let stop_loss stop_loss_pct : bool Gadt.t =
  let multiplier = Float.(1.0 - stop_loss_pct) in
  last <. EntryPrice *. Const (multiplier, Float)

let profit_target profit_target_pct : bool Gadt.t =
  let multiplier = Float.(1.0 + profit_target_pct) in
  last >. EntryPrice *. Const (multiplier, Float)

let max_holding_time max_ticks : bool Gadt.t =
  let max_ticks_expr = Const (max_ticks, Int) in
  App2 (Fun (">", ( > )), TicksHeld, max_ticks_expr)

let safe_to_enter ?(close_buffer = 10.0) () : bool Gadt.t =
  is_open TickTime
  &&. App1 (Fun ("not", not), is_close TickTime (Const (close_buffer, Float)))

let force_exit_eod ?(close_buffer = 10.0) () : bool Gadt.t =
  is_close TickTime (Const (close_buffer, Float))

(** Estridatter_Fixed - Mean Reversion Strategy with Proper EOD Protections

    IMPORTANT: This is the FIXED version of estridatter that includes:
    - Position sizing fix (from template)
    - safe_to_enter() - prevents entering positions near market close
    - force_exit_eod() - exits all positions before market close
    - Removed 60-tick time limit - let the mean reversion signals work!

    Original optimization results (NOTE: These are from buggy version):
    - Final Cash: $154,801.28 (54.8% return in 3 months)
    - Total Trades: 6,130 (Win Rate: 61.04%)
    - WARNING: Results invalid due to position sizing bug + missing EOD
      protections

    Strategy Logic: Entry (Buy):
    - RSI(19) < 39.01 (oversold)
    - Price < Lower Bollinger Band(48, 2.0, 2.0) (stretched below mean)
    - Safe to enter (NOT within 10 min of market close)

    Exit (Sell) - any of:
    - Force exit EOD (within 10 min of market close)
    - Price > Middle Bollinger Band(48, 2.0, 2.0) (mean reversion complete)
    - RSI(19) > 74.01 (overbought)
    - Price > Upper Bollinger Band(48, 2.0, 2.0) (overshot the mean)
    - 2% stop loss (Price < EntryPrice * 0.98)
    - 5% take profit (Price > EntryPrice * 1.05)

    Risk Management:
    - Maximum 10 concurrent positions for diversification
    - 10% position size per position (FIXED - was buggy before)
    - Tight 2% stop loss to limit downside
    - 5% take profit to lock in gains
    - No overnight positions (force_exit_eod) *)
let estridatter_fixed =
  (* RSI and Bollinger Band indicators *)
  let rsi_19 = Real.rsi 19 () in
  let bb_lower_48 = Real.lower_bband 48 2.0 2.0 () in
  let bb_middle_48 = Real.middle_bband 48 2.0 2.0 () in

  (* Optimized threshold values *)
  let rsi_buy_threshold = 39.008806 in
  let rsi_sell_threshold = 74.008689 in

  (* Risk management parameters *)
  let stop_loss_pct = 0.02 in
  (* 2% stop loss *)
  let take_profit_pct = 0.05 in
  (* 5% take profit *)

  (* Pre-compute multipliers for GADT expressions *)
  let stop_loss_multiplier = Float.(1.0 - stop_loss_pct) in
  (* 0.98 *)
  let take_profit_multiplier = Float.(1.0 + take_profit_pct) in
  (* 1.05 *)

  {
    name = "Estridatter_Fixed";
    (* Entry: RSI < 39.01 AND Price < Lower BB(48) AND safe to enter *)
    buy_trigger =
      rsi_19
      <. Const (rsi_buy_threshold, Float)
      &&. (last <. bb_lower_48) &&. safe_to_enter ();
    (* ADDED: Don't enter near market close *)

    (* Exit: EOD protection + mean reversion signals *)
    sell_trigger =
      force_exit_eod () (* ADDED: Exit within 10 min of close *)
      (* Mean reversion complete: price returned to middle BB *)
      ||. (last >. bb_middle_48)
      (* Overbought: RSI too high *)
      ||. (rsi_19 >. Const (rsi_sell_threshold, Float))
      (* Stop loss: 2% below entry *)
      ||. (last <. EntryPrice *. Const (stop_loss_multiplier, Float))
      (* Take profit: 5% above entry *)
      ||. (last >. EntryPrice *. Const (take_profit_multiplier, Float));
    (* REMOVED: max_holding_time 60 - let the signals work! *)

    (* Score: Prioritize stocks with lower RSI (more oversold = higher score) *)
    score = Const (100.0, Float) -. rsi_19;
    (* Portfolio parameters *)
    max_positions = 10;
    position_size = 0.1;
    (* 10% per position *)
  }

(** Estridatter.1.0.0 - First Valid Optimized Version (Proper Bounds)

    Result from ISRES optimization with CORRECTED bounds:
    - Best objective value: 154,250.98
    - All parameters within valid ranges

    Optimized Parameters:
    - RSI(9): Very fast, reactive momentum indicator
    - RSI oversold: 33.60 (moderate oversold threshold)
    - RSI overbought: 75.41 (patient exit)
    - BB(58): Medium-term stable bands
    - BB std entry: 2.12 (slightly wide for entry)
    - BB std exit: 1.61 (tighter for earlier exits)

    Strategy Character: "Fast reaction, asymmetric exits"

    Key Innovation - Asymmetric BB Std:
    - Entry bands (2.12σ): Wider, waits for bigger dips
    - Exit bands (1.61σ): Narrower, exits on partial recovery
    - This captures: enter on strong dips, exit before full reversion

    The fast RSI(9) makes this very reactive to short-term momentum shifts,
    while the medium BB(58) provides stable context.

    Entry Logic:
    - RSI(9) < 33.60 (fast oversold signal)
    - Price < Lower BB(58, 2.12) (moderate dip)
    - Safe to enter (NOT within 10 min of close)

    Exit Logic (any of):
    - Force exit EOD (within 10 min of close)
    - Price > Middle BB(58, 1.61) (partial recovery - tighter bands)
    - RSI(9) > 75.41 (fast momentum reversal)
    - Price > Upper BB(58, 1.61) (strong recovery)
    - 2% stop loss
    - 5% take profit

    Risk Management:
    - 10 concurrent positions max
    - 10% position size per trade
    - No overnight positions (force_exit_eod + safe_to_enter) *)
let estridatter_1_0_0 =
  (* Fast RSI for reactive signals *)
  let rsi_9 = Real.rsi 9 () in

  (* Entry bands - slightly wider (2.12 std) *)
  let bb_lower_entry = Real.lower_bband 58 2.122944 2.122944 () in

  (* Exit bands - tighter (1.61 std) for earlier exits *)
  let bb_middle_exit = Real.middle_bband 58 1.612366 1.612366 () in

  {
    name = "Estridatter.1.0.0";
    (* Entry: Fast oversold on moderate dip *)
    buy_trigger =
      rsi_9
      <. Const (33.595346, Float)
      &&. (last <. bb_lower_entry) &&. safe_to_enter ();
    (* Exit: Partial recovery with tighter bands *)
    sell_trigger =
      force_exit_eod ()
      ||. (last >. bb_middle_exit) (* Tighter middle band *)
      ||. (rsi_9 >. Const (75.408070, Float)) (* Fast momentum shift *)
      ||. stop_loss 0.02 ||. profit_target 0.05;
    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi_9;
    max_positions = 10;
    position_size = 0.1;
  }

(** Estridatter_Var - Optimizable version with proper EOD protections

    This is the version to use for NEW optimization runs. All fixes applied +
    parameters as variables.

    Variables to optimize (6 total): 1. rsi_period: [5, 30] - RSI calculation
    period 2. rsi_oversold: [20.0, 45.0] - RSI oversold entry threshold 3.
    rsi_overbought: [60.0, 80.0] - RSI overbought exit threshold 4. bb_period:
    [20, 80] - Bollinger Band period 5. bb_std_entry: [1.5, 3.0] - BB std for
    entry (lower band) 6. bb_std_exit: [1.5, 3.0] - BB std for exit
    (middle/upper bands)

    Fixed risk controls (NOT optimized):
    - stop_loss: 2%
    - take_profit: 5% *)
let estridatter_var =
  (* Variables for optimization - WITH PROPER BOUNDS ENFORCED *)
  let rsi_period = Gadt_fo.var ~lower:5.0 ~upper:30.0 Gadt.Type.Int in
  let rsi_oversold = Gadt_fo.var ~lower:20.0 ~upper:45.0 Gadt.Type.Float in
  let rsi_overbought = Gadt_fo.var ~lower:60.0 ~upper:80.0 Gadt.Type.Float in
  let bb_period = Gadt_fo.var ~lower:20.0 ~upper:80.0 Gadt.Type.Int in
  let bb_std_entry = Gadt_fo.var ~lower:1.5 ~upper:3.0 Gadt.Type.Float in
  let bb_std_exit = Gadt_fo.var ~lower:1.5 ~upper:3.0 Gadt.Type.Float in

  (* Create RSI indicator with variable period *)
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period) ))
  in

  (* Create entry Bollinger Bands (lower band with variable std) *)
  let bb_lower_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_period,
               bb_std_entry,
               bb_std_entry ) ))
  in

  (* Create exit Bollinger Bands (middle with variable std) *)
  let bb_middle_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_period,
               bb_std_exit,
               bb_std_exit ) ))
  in

  {
    name = "Estridatter_Var";
    (* Entry: Oversold + below lower BB + safe to enter *)
    buy_trigger =
      rsi <. rsi_oversold &&. (last <. bb_lower_entry) &&. safe_to_enter ();
    (* Exit: EOD + mean reversion signals + risk controls *)
    sell_trigger =
      force_exit_eod ()
      ||. (last >. bb_middle_exit) (* Mean reversion complete *)
      ||. (rsi >. rsi_overbought) (* Overbought *)
      ||. stop_loss 0.02 (* Fixed 2% stop *)
      ||. profit_target 0.05;
    (* Fixed 5% target *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi;
    max_positions = 10;
    position_size = 0.1;
  }

(** Estridatter_Var_Wide - Wide Diversification Variant

    Same logic as Estridatter_Var but with:
    - 20 positions instead of 10 (more diversification)
    - 5% position size instead of 10% (same 100% total allocation)

    Use this variant when you want broader market coverage and lower
    concentration risk. *)
let estridatter_var_wide =
  (* Variables for optimization - WITH PROPER BOUNDS ENFORCED *)
  let rsi_period = Gadt_fo.var ~lower:5.0 ~upper:30.0 Gadt.Type.Int in
  let rsi_oversold = Gadt_fo.var ~lower:20.0 ~upper:45.0 Gadt.Type.Float in
  let rsi_overbought = Gadt_fo.var ~lower:60.0 ~upper:80.0 Gadt.Type.Float in
  let bb_period = Gadt_fo.var ~lower:20.0 ~upper:80.0 Gadt.Type.Int in
  let bb_std_entry = Gadt_fo.var ~lower:1.5 ~upper:3.0 Gadt.Type.Float in
  let bb_std_exit = Gadt_fo.var ~lower:1.5 ~upper:3.0 Gadt.Type.Float in

  (* Create RSI indicator with variable period *)
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period) ))
  in

  (* Create entry Bollinger Bands (lower band with variable std) *)
  let bb_lower_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_period,
               bb_std_entry,
               bb_std_entry ) ))
  in

  (* Create exit Bollinger Bands (middle with variable std) *)
  let bb_middle_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_period,
               bb_std_exit,
               bb_std_exit ) ))
  in

  {
    name = "Estridatter_Var_Wide";
    (* Entry: Oversold + below lower BB + safe to enter *)
    buy_trigger =
      rsi <. rsi_oversold &&. (last <. bb_lower_entry) &&. safe_to_enter ();
    (* Exit: EOD + mean reversion signals + risk controls *)
    sell_trigger =
      force_exit_eod ()
      ||. (last >. bb_middle_exit) (* Mean reversion complete *)
      ||. (rsi >. rsi_overbought) (* Overbought *)
      ||. stop_loss 0.02 (* Fixed 2% stop *)
      ||. profit_target 0.05;
    (* Fixed 5% target *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi;
    max_positions = 20;
    (* 2x positions *)
    position_size = 0.05;
    (* 0.5x size = same total allocation *)
  }

(** Estridatter_Var_Ultra_Wide - Ultra Wide Diversification Variant

    Same logic as Estridatter_Var but with:
    - 30 positions instead of 10 (maximum diversification)
    - 3.3% position size instead of 10% (same 100% total allocation)

    Use this variant when you want to test if selection bias is affecting
    results. With more positions, you're taking a larger sample of available
    opportunities. *)
let estridatter_var_ultra_wide =
  (* Variables for optimization - WITH PROPER BOUNDS ENFORCED *)
  let rsi_period = Gadt_fo.var ~lower:5.0 ~upper:30.0 Gadt.Type.Int in
  let rsi_oversold = Gadt_fo.var ~lower:20.0 ~upper:45.0 Gadt.Type.Float in
  let rsi_overbought = Gadt_fo.var ~lower:60.0 ~upper:80.0 Gadt.Type.Float in
  let bb_period = Gadt_fo.var ~lower:20.0 ~upper:80.0 Gadt.Type.Int in
  let bb_std_entry = Gadt_fo.var ~lower:1.5 ~upper:3.0 Gadt.Type.Float in
  let bb_std_exit = Gadt_fo.var ~lower:1.5 ~upper:3.0 Gadt.Type.Float in

  (* Create RSI indicator with variable period *)
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period) ))
  in

  (* Create entry Bollinger Bands (lower band with variable std) *)
  let bb_lower_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_period,
               bb_std_entry,
               bb_std_entry ) ))
  in

  (* Create exit Bollinger Bands (middle with variable std) *)
  let bb_middle_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_period,
               bb_std_exit,
               bb_std_exit ) ))
  in

  {
    name = "Estridatter_Var_Ultra_Wide";
    (* Entry: Oversold + below lower BB + safe to enter *)
    buy_trigger =
      rsi <. rsi_oversold &&. (last <. bb_lower_entry) &&. safe_to_enter ();
    (* Exit: EOD + mean reversion signals + risk controls *)
    sell_trigger =
      force_exit_eod ()
      ||. (last >. bb_middle_exit) (* Mean reversion complete *)
      ||. (rsi >. rsi_overbought) (* Overbought *)
      ||. stop_loss 0.02 (* Fixed 2% stop *)
      ||. profit_target 0.05;
    (* Fixed 5% target *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi;
    max_positions = 30;
    (* 3x positions *)
    position_size = 0.0333;
    (* ~3.3% each = 100% total allocation *)
  }

(** Estridatter.2.0.0 - Second Optimized Version (NLOPT_MAXEVAL_REACHED)

    Result from optimization run achieving:
    - Final value: $171,287.96 (71.3% return)
    - Win rate: 71.15% over 6,555 trades
    - Profit factor: 1.688
    - Sharpe ratio: 0.154

    Optimized parameters:
    - RSI(27) with thresholds 42.13 / 76.38
    - Bollinger Bands(24) with 1.8045 std for entry, 2.29 std for exit
    - 2% stop loss, 5% take profit
    - EOD protection (10 min buffer) *)
let estridatter_2_0_0 =
  (* RSI indicator - period 27 *)
  let rsi_27 = Real.rsi 27 () in

  (* Entry Bollinger Bands - period 24, std 1.8045 *)
  let bb_lower_entry = Real.lower_bband 24 1.804500 1.804500 () in

  (* Exit Bollinger Bands - period 24, std 2.29 *)
  let bb_middle_exit = Real.middle_bband 24 2.290597 2.290597 () in

  {
    name = "Estridatter.2.0.0";
    (* Entry: RSI(27) < 42.13 AND Price < Lower BB(24, 1.8) AND safe to enter *)
    buy_trigger =
      rsi_27
      <. Const (42.130937, Float)
      &&. (last <. bb_lower_entry)
      &&. safe_to_enter ~close_buffer:10.0 ();
    (* Exit: EOD OR mean reversion signals OR risk controls *)
    sell_trigger =
      force_exit_eod ~close_buffer:10.0 ()
      ||. (last >. bb_middle_exit) (* Mean reversion to middle band *)
      ||. (rsi_27 >. Const (76.381991, Float)) (* Overbought *)
      ||. stop_loss 0.02 (* 2% stop loss *)
      ||. profit_target 0.05;
    (* 5% take profit *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi_27;
    max_positions = 10;
    position_size = 0.1;
  }

(** Estridatter.3.0.0 - Third Optimized Version (Ultra-Wide Variant)

    Result from ISRES optimization on ultra-wide variant (30 positions):
    - Final value: $110,128.08 (10.1% return over 2 weeks)
    - Win rate: 79.84% over 1,637 trades
    - Profit factor: N/A
    - Sharpe ratio: 0.243

    Optimized parameters:
    - RSI(20) with thresholds 44.68 / 77.78
    - Bollinger Bands(71) with 1.60 std for entry, 2.62 std for exit
    - 2% stop loss, 5% take profit
    - EOD protection (10 min buffer)

    Strategy Character: High-frequency intraday mean reversion on 1-minute bars.
    Long-period indicators (71 bars) provide stable anchors. Enters on mild
    weakness (RSI 44.68), exits quickly on mean reversion (~71 minute avg hold).

    WARNING: Backtested on only 2 weeks of minute data. High frequency (163
    trades/day) with zero transaction costs. Likely overfit to this specific
    regime. Requires extensive out-of-sample validation before production use.
*)
let estridatter_3_0_0 =
  (* RSI indicator - period 20 *)
  let rsi_20 = Real.rsi 20 () in

  (* Entry Bollinger Bands - period 71, std 1.60 *)
  let bb_lower_entry = Real.lower_bband 71 1.604088 1.604088 () in

  (* Exit Bollinger Bands - period 71, std 2.62 *)
  let bb_middle_exit = Real.middle_bband 71 2.624549 2.624549 () in

  {
    name = "Estridatter.3.0.0";
    (* Entry: RSI(20) < 44.68 AND Price < Lower BB(71, 1.60) AND safe to enter *)
    buy_trigger =
      rsi_20
      <. Const (44.683406, Float)
      &&. (last <. bb_lower_entry)
      &&. safe_to_enter ~close_buffer:10.0 ();
    (* Exit: EOD OR mean reversion signals OR risk controls *)
    sell_trigger =
      force_exit_eod ~close_buffer:10.0 ()
      ||. (last >. bb_middle_exit) (* Mean reversion to middle band *)
      ||. (rsi_20 >. Const (77.781908, Float)) (* Overbought *)
      ||. stop_loss 0.02 (* 2% stop loss *)
      ||. profit_target 0.05;
    (* 5% take profit *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi_20;
    max_positions = 30;
    position_size = 0.0333;
    (* ~3.3% each for 30 positions *)
  }

(** Estridatter_3_0_0_Debug - Single-Position Debug Variant

    This is EXACTLY the same logic as Estridatter.3.0.0, but with only 1
    position at 100% allocation. Use this to debug: 1. What single stock is
    being selected 2. Whether the scoring/ranking is working correctly 3.
    Whether the entry/exit signals are firing as expected

    If this variant also enters alphabetically-first stocks (AAPL, etc.) rather
    than the most oversold (lowest RSI), then the scoring system is broken. *)
let estridatter_3_0_0_debug =
  (* RSI indicator - period 20 *)
  let rsi_20 = Real.rsi 20 () in

  (* Entry Bollinger Bands - period 71, std 1.60 *)
  let bb_lower_entry = Real.lower_bband 71 1.604088 1.604088 () in

  (* Exit Bollinger Bands - period 71, std 2.62 *)
  let bb_middle_exit = Real.middle_bband 71 2.624549 2.624549 () in

  {
    name = "Estridatter_3_0_0_Debug";
    (* Entry: RSI(20) < 44.68 AND Price < Lower BB(71, 1.60) AND safe to enter *)
    buy_trigger =
      rsi_20
      <. Const (44.683406, Float)
      &&. (last <. bb_lower_entry)
      &&. safe_to_enter ~close_buffer:10.0 ();
    (* Exit: EOD OR mean reversion signals OR risk controls *)
    sell_trigger =
      force_exit_eod ~close_buffer:10.0 ()
      ||. (last >. bb_middle_exit) (* Mean reversion to middle band *)
      ||. (rsi_20 >. Const (77.781908, Float)) (* Overbought *)
      ||. stop_loss 0.02 (* 2% stop loss *)
      ||. profit_target 0.05;
    (* 5% take profit *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi_20;
    max_positions = 1;
    (* DEBUG: Only 1 position *)
    position_size = 1.0;
    (* DEBUG: 100% allocation *)
  }

(** Estridatter.4.0.0 - Fourth Optimized Version (Ultra-High Frequency Variant)

    Result from ISRES optimization on ultra-wide variant (30 positions):
    - Final value: $112,778.04 (12.8% return over 2 weeks)
    - Win rate: 75.72% over 2,854 trades
    - Profit factor: 2.143
    - Sharpe ratio: 0.210

    Optimized parameters:
    - RSI(29) with thresholds 44.17 / 66.04 (exits much earlier!)
    - Bollinger Bands(26) with 1.67 std for entry, 2.64 std for exit
    - 2% stop loss, 5% take profit
    - EOD protection (10 min buffer)

    Strategy Character: Ultra-high-frequency mean reversion (285 trades/day!)
    using very reactive 26-period Bollinger Bands. Exits early when RSI reaches
    66 (vs 77.78 in 3.0.0). Trades 74% more frequently than 3.0.0 but with
    smaller edge per trade ($4.48 vs $6.19).

    Trade-offs vs Estridatter.3.0.0:
    + Higher raw returns (12.8% vs 10.1%)

    - Lower after costs (4.2% vs 5.2% net with 0.30% per trade)
    - Lower Sharpe (0.210 vs 0.243)
    - Lower win rate (75.72% vs 79.84%)
    - Much higher frequency (285 vs 164 trades/day)

    WARNING: Backtested on only 2 weeks of minute data. Extremely high frequency
    (285 trades/day) with zero transaction costs. Transaction costs will likely
    make this underperform 3.0.0. Requires extensive out-of-sample validation
    before production use. *)
let estridatter_4_0_0 =
  (* RSI indicator - period 29 *)
  let rsi_29 = Real.rsi 29 () in

  (* Entry Bollinger Bands - period 26, std 1.67 *)
  let bb_lower_entry = Real.lower_bband 26 1.674544 1.674544 () in

  (* Exit Bollinger Bands - period 26, std 2.64 *)
  let bb_middle_exit = Real.middle_bband 26 2.643326 2.643326 () in

  {
    name = "Estridatter.4.0.0";
    (* Entry: RSI(29) < 44.17 AND Price < Lower BB(26, 1.67) AND safe to enter *)
    buy_trigger =
      rsi_29
      <. Const (44.174938, Float)
      &&. (last <. bb_lower_entry)
      &&. safe_to_enter ~close_buffer:10.0 ();
    (* Exit: EOD OR mean reversion signals OR risk controls *)
    sell_trigger =
      force_exit_eod ~close_buffer:10.0 ()
      ||. (last >. bb_middle_exit) (* Mean reversion to middle band *)
      ||. (rsi_29 >. Const (66.036040, Float))
          (* Overbought - exits MUCH earlier *)
      ||. stop_loss 0.02 (* 2% stop loss *)
      ||. profit_target 0.05;
    (* 5% take profit *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi_29;
    max_positions = 30;
    position_size = 0.0333;
    (* ~3.3% each for 30 positions *)
  }

(**
  Estridatter.5.0.0 - Fifth Optimized Version (Extreme Oversold Entry)

  Result from ISRES optimization on Var_Wide variant (20 positions):
  - Final value: $105,472.10 (5.47% return over 2.7 months)
  - Annualized: ~24% return
  - Win rate: 65.00% over 460 trades
  - Profit factor: 1.781
  - Sharpe ratio: 0.215

  Optimized parameters:
  - RSI(29) with thresholds 20.17 / 74.10
  - Bollinger Bands(46) with 1.80 std for entry, 2.47 std for exit
  - 2% stop loss, 5% take profit
  - EOD protection (10 min buffer)
  - 20 positions @ 5% each

  Strategy Character:
  "Patient hunter - waits for extreme oversold, exits on standard reversion"

  Key Innovation - Most Aggressive Entry:
  The RSI < 20.17 entry threshold is the LOWEST of all estridatter variants,
  making this the most patient entry strategy. It waits for truly extreme
  oversold conditions, not just mild dips. This filters out false signals
  and ensures entry on significant market dislocations.

  The medium-term indicators (RSI 29, BB 46) provide stable context while
  the extreme oversold threshold ensures only the best opportunities are taken.

  Entry Logic:
  - RSI(29) < 20.17 (EXTREME oversold - most aggressive entry)
  - Price < Lower BB(46, 1.80) (below mean with moderate band)
  - Safe to enter (NOT within 10 min of close)

  Exit Logic:
  - Force exit EOD (within 10 min of close)
  - Price > Middle BB(46, 2.47) (mean reversion complete)
  - RSI(29) > 74.10 (overbought - standard exit)
  - 2% stop loss
  - 5% take profit

  Risk Management:
  - 20 concurrent positions (wide diversification)
  - 5% position size per trade
  - No overnight positions (force_exit_eod + safe_to_enter)

  Performance Notes:
  Tested on Sept-Nov 2025 data (2.7 months, 23,069 minute bars).
  24% annualized return with moderate Sharpe (0.215). The extreme oversold
  entry (20.17) successfully filters noise and captures high-probability
  mean reversion setups. 460 trades = ~170/month shows good activity without
  overtrading.
*)
let estridatter_5_0_0 =
  (* RSI indicator - period 29 *)
  let rsi_29 = Real.rsi 29 () in

  (* Entry Bollinger Bands - period 46, std 1.798 *)
  let bb_lower_entry = Real.lower_bband 46 1.797883 1.797883 () in

  (* Exit Bollinger Bands - period 46, std 2.474 *)
  let bb_middle_exit = Real.middle_bband 46 2.474220 2.474220 () in

  {
    name = "Estridatter.5.0.0";
    (* Entry: RSI(29) < 20.17 AND Price < Lower BB(46, 1.80) AND safe to enter *)
    buy_trigger =
      rsi_29
      <. Const (20.173400, Float)
      &&. (last <. bb_lower_entry)
      &&. safe_to_enter ~close_buffer:10.0 ();
    (* Exit: EOD OR mean reversion signals OR risk controls *)
    sell_trigger =
      force_exit_eod ~close_buffer:10.0 ()
      ||. (last >. bb_middle_exit) (* Mean reversion to middle band *)
      ||. (rsi_29 >. Const (74.101443, Float)) (* Overbought *)
      ||. stop_loss 0.02 (* 2% stop loss *)
      ||. profit_target 0.05;
    (* 5% take profit *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi_29;
    max_positions = 20;
    (* Wide diversification *)
    position_size = 0.05;
    (* 5% each for 20 positions *)
  }

(** CosmicCowgirl_Var - Minimal 4-Parameter Mean Reversion Strategy

    Designed for efficient optimization with reduced parameter space:
    - Only 4 variables to optimize (vs 6-8 in other strategies)
    - Ideal for 300-500 ISRES iterations on large datasets
    - Enforces natural constraints (shared periods, symmetric thresholds)

    The 4 Optimizable Parameters: 1. shared_period: [10, 100] - Used for BOTH
    RSI and Bollinger Bands
    - Enforces consistency: oversold detection matches band timeframe
    - WIDENED: Previously [10, 50], optimizer was hitting upper bound 2.
      rsi_threshold: [15, 40] - Symmetric entry/exit
    - Buy when RSI < threshold
    - Sell when RSI > (100 - threshold) 3. bb_std: [1.0, 4.0] - Single Bollinger
      Band width
    - Entry: price < lower_band(shared_period, bb_std)
    - Exit: price > middle_band(shared_period, bb_std)
    - WIDENED: Previously [1.5, 2.5], optimizer wanted wider bands 4. stop_loss:
      [0.01, 0.05] - Risk control (1-5%)
    - Take profit auto-derived as stop_loss × 2.5 (2.5:1 ratio)
    - WIDENED: Previously [0.01, 0.03], optimizer hitting upper bound

    Strategy Logic: Entry (Buy):
    - RSI(shared_period) < rsi_threshold (oversold)
    - Price < Lower BB(shared_period, bb_std) (stretched below mean)
    - Safe to enter (NOT within 10 min of market close)

    Exit (Sell) - any of:
    - Force exit EOD (within 10 min of close)
    - Price > Middle BB(shared_period, bb_std) (mean reversion complete)
    - RSI(shared_period) > (100 - rsi_threshold) (overbought, symmetric)
    - Stop loss (variable 1-3%)
    - Take profit (2.5× stop loss for fixed risk/reward)

    Benefits of 4-Parameter Design:
    - Faster optimization (fewer dimensions to search)
    - Lower overfitting risk (simpler model)
    - Natural constraints enforce strategy coherence
    - Still captures core mean reversion logic *)
let cosmiccowgirl_var =
  (* Variable 1: Shared period for both RSI and Bollinger Bands *)
  let shared_period = Gadt_fo.var ~lower:10.0 ~upper:100.0 Gadt.Type.Int in

  (* Variable 2: RSI threshold (buy < X, sell > 100-X for symmetry) *)
  let rsi_threshold = Gadt_fo.var ~lower:15.0 ~upper:40.0 Gadt.Type.Float in

  (* Variable 3: Single Bollinger Band std dev (used for both entry and exit) *)
  let bb_std = Gadt_fo.var ~lower:1.0 ~upper:4.0 Gadt.Type.Float in

  (* Variable 4: Stop loss (take profit derived as 2.5× this) *)
  let stop_loss_pct = Gadt_fo.var ~lower:0.01 ~upper:0.05 Gadt.Type.Float in

  (* Derived: Symmetric RSI sell threshold *)
  let rsi_sell_threshold = Const (100.0, Float) -. rsi_threshold in

  (* Derived: Take profit = 2.5× stop loss for consistent risk/reward *)
  let take_profit_pct = stop_loss_pct *. Const (2.5, Float) in

  (* Create RSI indicator using shared period *)
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), shared_period) ))
  in

  (* Create entry Bollinger Band (lower band using shared period and std) *)
  let bb_lower_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               shared_period,
               bb_std,
               bb_std ) ))
  in

  (* Create exit Bollinger Band (middle band using shared period and std) *)
  let bb_middle_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               shared_period,
               bb_std,
               bb_std ) ))
  in

  (* Compute stop loss and take profit multipliers *)
  let stop_loss_mult = Const (1.0, Float) -. stop_loss_pct in
  (* 1 - stop_loss *)
  let take_profit_mult = Const (1.0, Float) +. take_profit_pct in
  (* 1 + take_profit *)

  {
    name = "CosmicCowgirl_Var";
    (* Entry: Oversold RSI + below lower BB + safe to enter *)
    buy_trigger =
      rsi <. rsi_threshold &&. (last <. bb_lower_entry)
      &&. safe_to_enter ~close_buffer:10.0 ();
    (* Exit: EOD + mean reversion signals + risk controls *)
    sell_trigger =
      force_exit_eod ~close_buffer:10.0 ()
      ||. (last >. bb_middle_exit) (* Mean reversion to middle *)
      ||. (rsi >. rsi_sell_threshold) (* Symmetric overbought *)
      ||. (last <. EntryPrice *. stop_loss_mult) (* Variable stop loss *)
      ||. (last >. EntryPrice *. take_profit_mult);
    (* 2.5× stop loss *)

    (* Score: More oversold = higher priority *)
    score = Const (100.0, Float) -. rsi;
    max_positions = 20;
    (* Wide diversification like estridatter *)
    position_size = 0.05;
    (* 5% each *)
  }

(* Keep original estridatter for comparison/reference *)
let estridatter =
  (* RSI and Bollinger Band indicators *)
  let rsi_19 = Real.rsi 19 () in
  let bb_lower_48 = Real.lower_bband 48 2.0 2.0 () in
  let bb_middle_48 = Real.middle_bband 48 2.0 2.0 () in

  (* Optimized threshold values *)
  let rsi_buy_threshold = 39.008806 in
  let rsi_sell_threshold = 74.008689 in

  (* Risk management parameters *)
  let stop_loss_pct = 0.02 in
  (* 2% stop loss *)
  let take_profit_pct = 0.05 in
  (* 5% take profit *)
  let max_hold_ticks = 60 in

  (* Pre-compute multipliers for GADT expressions *)
  let stop_loss_multiplier = Float.(1.0 - stop_loss_pct) in
  (* 0.98 *)
  let take_profit_multiplier = Float.(1.0 + take_profit_pct) in
  (* 1.05 *)

  {
    name = "Estridatter";
    (* Entry: RSI < 39.01 AND Price < Lower BB(48) *)
    buy_trigger =
      rsi_19 <. Const (rsi_buy_threshold, Float) &&. (last <. bb_lower_48);
    (* Exit: Any of the following conditions *)
    sell_trigger =
      (* Mean reversion complete: price returned to middle BB *)
      last >. bb_middle_48
      (* Overbought: RSI too high *)
      ||. (rsi_19 >. Const (rsi_sell_threshold, Float))
      (* Stop loss: 2% below entry *)
      ||. (last <. EntryPrice *. Const (stop_loss_multiplier, Float))
      (* Take profit: 5% above entry *)
      ||. (last >. EntryPrice *. Const (take_profit_multiplier, Float))
      (* Time-based exit: held too long *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (max_hold_ticks, Int));
    (* Score: Prioritize stocks with lower RSI (more oversold = higher score) *)
    score = Const (100.0, Float) -. rsi_19;
    (* Portfolio parameters *)
    max_positions = 10;
    position_size = 0.1;
    (* 10% per position *)
  }

(** CosmicCowgirl_25_52_90 - Optimized ProfessionalMeanRev (Patient Exit
    Strategy)

    Result from ISRES optimization: 101775.92 (1.78% return over 2 weeks)

    Optimized parameters reveal a "patient exit" pattern:
    - Entry RSI: 25 (medium-fast for catching dips)
    - Entry RSI threshold: 33.64 (moderately oversold)
    - Entry BB: 52 (long, stable bands for reliable entry signals)
    - Exit RSI: 90 (EXTREMELY long - waits for sustained trend changes)
    - Exit RSI threshold: 79.02 (very overbought - patient exit)
    - Exit BB: 84 (very long, stable exit detection)

    The Strategy's Character: "Enter conservatively, exit patiently" - the
    cosmic cowgirl doesn't rush.

    The 90-period RSI for exits is the key innovation. Most mean reversion
    strategies use short RSI periods (2-14) for quick exits. This strategy
    discovered that waiting for a 90-period RSI to reach 79 captures larger
    moves while filtering out noise.

    Entry Logic (Stable, 52-period bands):
    - RSI(25) dips below 33.64 (moderate oversold)
    - Price touches lower BB(52) (stable reference)
    - Safe to enter (not near market close)

    Exit Logic (Patient, 84-period bands):
    - Force exit near close (no overnight)
    - Price > middle BB(84) (long-term mean reversion)
    - RSI(90) > 79.02 (sustained momentum reversal - patient!)
    - 2.5% stop loss (tight risk control)
    - 90 tick max hold

    The asymmetric periods (52 for entry, 84 for exit) show that the optimizer
    discovered entry and exit work best at different timeframes - validating the
    design of ProfessionalMeanRev_Opt with distinct variables. *)
let cosmic_cowgirl_25_52_90 =
  (* Entry indicators - stable, medium-term *)
  let entry_rsi_25 = Real.rsi 25 () in
  let entry_bb_lower_52 = Real.lower_bband 52 2.0 2.0 () in

  (* Exit indicators - very long-term, patient *)
  let exit_rsi_90 = Real.rsi 90 () in
  let exit_bb_middle_84 = Real.middle_bband 84 2.0 2.0 () in

  {
    name = "CosmicCowgirl_25_52_90";
    (* Entry: Moderate oversold on stable indicators *)
    buy_trigger =
      entry_rsi_25
      <. Const (33.635982, Float)
      &&. (last <. entry_bb_lower_52)
      &&. safe_to_enter ();
    (* Exit: Patient, waiting for sustained reversal *)
    sell_trigger =
      force_exit_eod ()
      ||. (last >. exit_bb_middle_84) (* Long-term mean reversion *)
      ||. (exit_rsi_90 >. Const (79.021591, Float)) (* Very patient exit *)
      ||. stop_loss 0.025 (* 2.5% stop *)
      ||. max_holding_time 90;
    (* 90 ticks *)
    score = Const (1.0, Float);
    (* Default score *)
    max_positions = 10;
    position_size = 0.10;
  }

(** SharpEntry_PatientExit_50_16_98 - Optimized ProfessionalMeanRev (Reactive
    Entry, Patient Exit)

    Result from ISRES optimization: 102,515.58 (2.52% return over 2 weeks)

    Optimized parameters reveal a "reactive entry / patient exit" pattern:
    - Entry RSI: 50 (medium, catches early weakness not extreme oversold)
    - Entry RSI threshold: 39.68 (moderate oversold)
    - Entry BB: 16 (VERY SHORT - most reactive among all strategies!)
    - Exit RSI: 98 (EXTREMELY long - waits for sustained trend changes)
    - Exit RSI threshold: 90.31 (very overbought)
    - Exit BB: 60 (medium-term exit detection)

    The Strategy's Character: "Enter aggressively on sharp dips, exit patiently
    on sustained reversals"

    Key Innovation - Shortest Entry BB (16): The 16-period Bollinger Band for
    entry is among the shortest in all strategies. This creates extremely
    reactive bands that catch brief, sharp price drops that longer-period bands
    would smooth over. When combined with medium RSI (50), it enters on early
    weakness, not waiting for extreme oversold conditions.

    The 98-period RSI for exits provides extreme patience - it waits for a truly
    sustained momentum reversal, not just short-term bounces. This asymmetry
    (reactive entry, patient exit) aims to catch quick dips and ride them fully.

    Entry Logic (Reactive, 16-period bands):
    - RSI(50) dips below 39.68 (early weakness detection)
    - Price touches lower BB(16) (catches sharp, brief dips)
    - Safe to enter (not near market close)

    Exit Logic (Patient, 60-period bands):
    - Force exit near close (no overnight)
    - Price > middle BB(60) (medium-term mean reversion)
    - RSI(98) > 90.31 (extremely patient - waits for sustained reversal)
    - 2.5% stop loss (tight risk control)
    - 90 tick max hold

    Performance: 2.52% in 2 weeks - second-best among strategies with realistic
    risk management, just 0.08% behind QuickSnap. The short entry BB (16) vs
    QuickSnap's long entry BB (51) represents a fundamentally different approach
    to catching mean reversion opportunities. *)
let sharp_entry_patient_exit =
  (* Entry indicators - very reactive *)
  let entry_rsi_50 = Real.rsi 50 () in
  let entry_bb_lower_16 = Real.lower_bband 16 2.0 2.0 () in

  (* Exit indicators - very patient *)
  let exit_rsi_98 = Real.rsi 98 () in
  let exit_bb_middle_60 = Real.middle_bband 60 2.0 2.0 () in

  {
    name = "SharpEntry_PatientExit_50_16_98";
    (* Entry: Reactive to sharp dips *)
    buy_trigger =
      entry_rsi_50
      <. Const (39.682656, Float)
      &&. (last <. entry_bb_lower_16)
      &&. safe_to_enter ();
    (* Exit: Patient, waiting for sustained reversal *)
    sell_trigger =
      force_exit_eod ()
      ||. (last >. exit_bb_middle_60)
      ||. (exit_rsi_98 >. Const (90.309677, Float))
      ||. stop_loss 0.025 ||. max_holding_time 90;
    score = Const (1.0, Float);
    (* Default score *)
    max_positions = 10;
    position_size = 0.10;
  }

(** PatientEntry_77_46 - Optimized ProfessionalMeanRev (Glacial Entry, Medium
    Exit)

    Result from ISRES optimization: 102,977.71 (2.56% return - best so far!)

    Optimized parameters reveal an inverse "patient entry" pattern:
    - Entry RSI: 77 (EXTREMELY long - smoothest entry signal)
    - Entry RSI threshold: 39.72 (moderate oversold)
    - Entry BB: 97 (VERY LONG - most stable bands)
    - Exit RSI: 46 (medium-speed for exits)
    - Exit RSI threshold: 74.27 (overbought)
    - Exit BB: 80 (long but faster than entry)

    The Strategy's Character: "Wait for glacially smooth, deeply confirmed dips.
    Exit on medium-term strength."

    Key Innovation - Inverse of CosmicCowgirl: CosmicCowgirl used fast entry
    (RSI 25) with patient exit (RSI 90). PatientEntry does the opposite: patient
    entry (RSI 77) with medium exit (RSI 46).

    The 77-period RSI for entry is the longest among all optimized strategies.
    This filters out ALL noise - only entering on very confirmed, smooth
    oversold conditions. Combined with 97-period BBs, it catches only the
    deepest, most stable dips.

    Entry Logic (Glacial, 97-period bands):
    - RSI(77) dips below 39.72 (very smooth oversold signal)
    - Price touches lower BB(97) (extremely stable reference)
    - Safe to enter (not near market close)

    Exit Logic (Medium-speed, 80-period bands):
    - Force exit near close (no overnight)
    - Price > middle BB(80) (long-term mean reversion)
    - RSI(46) > 74.27 (medium-term momentum reversal)
    - 2.5% stop loss (tight risk control)
    - 90 tick max hold

    Performance Edge: Slightly outperforms all other variants (102,977 vs
    102,515 for SharpEntry). The glacially smooth entry appears to reduce false
    signals and improve entry quality, even though it means fewer trades
    overall.

    Philosophy: "The patient hunter waits for the perfect shot, not just any
    shot." *)
let patient_entry_77_46 =
  (* Entry indicators - glacially smooth *)
  let entry_rsi_77 = Real.rsi 77 () in
  let entry_bb_lower_97 = Real.lower_bband 97 2.0 2.0 () in

  (* Exit indicators - medium-term *)
  let exit_rsi_46 = Real.rsi 46 () in
  let exit_bb_middle_80 = Real.middle_bband 80 2.0 2.0 () in

  {
    name = "PatientEntry_77_46";
    (* Entry: Wait for extremely smooth, confirmed dips *)
    buy_trigger =
      entry_rsi_77
      <. Const (39.724033, Float)
      &&. (last <. entry_bb_lower_97)
      &&. safe_to_enter ();
    (* Exit: Medium-speed mean reversion *)
    sell_trigger =
      force_exit_eod ()
      ||. (last >. exit_bb_middle_80)
      ||. (exit_rsi_46 >. Const (74.271530, Float))
      ||. stop_loss 0.025 ||. max_holding_time 90;
    score = Const (100.0, Float) -. entry_rsi_77;
    (* Lower RSI = higher score *)
    max_positions = 10;
    position_size = 0.10;
  }

(** VolatilityBreakout_Opt - Catch Volatile Bursts with Volume Confirmation

    THESIS: Market grows in bursts concentrated in few stocks. Volatility
    expansion
    + volume surge identifies these "winner" stocks before major moves.

    This strategy combines: 1. ATR (Average True Range) to measure volatility
    expansion 2. Volume surge detection to confirm institutional interest 3. RSI
    to avoid chasing overbought rallies 4. Bollinger Bands to gauge price
    extension

    Variables to optimize (6 total): 1. atr_period: [10, 25] - ATR lookback for
    volatility measurement 2. atr_threshold: [1.2, 2.5] - How much ATR expansion
    required (multiplier) 3. volume_surge: [1.3, 2.5] - Volume multiplier vs
    recent average 4. rsi_period: [10, 30] - RSI period for momentum filter 5.
    rsi_max: [60.0, 85.0] - Maximum RSI for entry (avoid chasing) 6. bb_period:
    [15, 40] - Bollinger Band period for context

    Entry Logic - "Volatility + Volume = Winner Stock":
    - ATR(var1) > var2 * ATR_lagged (volatility expanding)
    - Volume > var3 * Volume_lagged (volume surge - institutions entering)
    - RSI(var4) < var5 (not yet overbought - room to run)
    - Price > Middle BB (in uptrend, not catching falling knives)
    - Safe to enter (not near close)

    Exit Logic - "Volatility Fades or Risk Limits Hit":
    - Force exit EOD (no overnight risk in skittish market)
    - ATR contracts (volatility fading - move exhausted)
    - Volume dries up (institutions exiting)
    - 2.5% stop loss (tight risk control)
    - 8% profit target (realistic for volatile bursts)
    - 90 tick max hold

    Why this works for your thesis:
    - Catches concentrated growth in "few winner stocks" via volume + volatility
    - Enters during expansion, exits when burst fades
    - Avoids dead money in non-volatile stocks *)
let volatility_breakout_opt =
  (* Variables for optimization *)
  let atr_period = Gadt_fo.var Gadt.Type.Int in
  (* Var 1 *)
  let atr_threshold = Gadt_fo.var Gadt.Type.Float in
  (* Var 2 *)
  let volume_surge = Gadt_fo.var Gadt.Type.Float in
  (* Var 3 *)
  let rsi_period = Gadt_fo.var Gadt.Type.Int in
  (* Var 4 *)
  let rsi_max = Gadt_fo.var Gadt.Type.Float in
  (* Var 5 *)
  let bb_period = Gadt_fo.var Gadt.Type.Int in
  (* Var 6 *)

  (* Create ATR indicator *)
  let atr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.atr", Tacaml.Indicator.Raw.atr), atr_period) ))
  in
  let atr_lagged = lag atr 10 in
  (* Compare to 10-period ago *)

  (* Create RSI indicator *)
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period) ))
  in

  (* Create Bollinger Band middle for trend context *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_period,
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Volume comparisons *)
  let volume_lagged = lag volume 10 in

  {
    name = "VolatilityBreakout_Opt";
    (* Entry: Volatility expansion + volume surge + room to run *)
    buy_trigger =
      atr
      >. atr_lagged *. atr_threshold (* Volatility expanding *)
      &&. (volume >. volume_lagged *. volume_surge) (* Volume surge *)
      &&. (rsi <. rsi_max) (* Not overbought *)
      &&. (last >. bb_middle) (* In uptrend *)
      &&. safe_to_enter ();
    (* Exit: Volatility/volume fade or risk limits *)
    sell_trigger =
      force_exit_eod ()
      ||. (atr <. atr_lagged) (* Volatility contracting *)
      ||. (volume <. volume_lagged *. Const (0.7, Float)) (* Volume dying *)
      ||. stop_loss 0.025 (* 2.5% stop *)
      ||. profit_target 0.08 (* 8% target *)
      ||. max_holding_time 90;
    score = atr *. volume;
    (* Higher volatility * volume = higher score *)
    max_positions = 8;
    (* Concentrate on fewer "winner" stocks *)
    position_size = 0.125;
    (* 12.5% per position *)
  }

(** MomentumVolume_Opt - Pure Momentum with Volume Confirmation

    THESIS: Growth is concentrated in historically performing stocks. Buy
    strength with volume confirmation (institutions piling in), exit on momentum
    fade.

    This is NOT mean reversion - this buys rising stocks and rides momentum.

    Variables to optimize (6 total): 1. fast_ema_period: [8, 20] - Fast EMA for
    momentum 2. slow_ema_period: [20, 50] - Slow EMA for trend 3. rsi_period:
    [10, 25] - RSI period 4. rsi_min: [50.0, 70.0] - Minimum RSI (buy strength,
    not weakness) 5. volume_surge: [1.2, 2.0] - Volume surge multiplier 6.
    adx_period: [10, 20] - ADX for trend strength

    Entry Logic - "Buy the Winners":
    - Fast EMA > Slow EMA (uptrend established)
    - RSI(var3) > var4 (buying strength, not dips!)
    - Volume > var5 * lagged volume (institutions entering)
    - ADX(var6) > 25 (strong trend, not choppy)
    - Safe to enter

    Exit Logic - "Momentum Fades":
    - Force exit EOD
    - Fast EMA < Slow EMA (trend breaking)
    - RSI drops below 45 (momentum fading)
    - Volume dries up
    - 3% stop loss
    - 10% profit target
    - 120 tick max hold (longer for momentum rides)

    Why this works for your thesis:
    - Identifies "historically performing stocks" via EMA crossover + strong
      volume
    - Buys concentrated winners, not diversified mediocrity
    - Momentum + volume = institutional accumulation in growth leaders *)
let momentum_volume_opt =
  (* Variables *)
  let fast_ema_period = Gadt_fo.var Gadt.Type.Int in
  (* Var 1 *)
  let slow_ema_period = Gadt_fo.var Gadt.Type.Int in
  (* Var 2 *)
  let rsi_period = Gadt_fo.var Gadt.Type.Int in
  (* Var 3 *)
  let rsi_min = Gadt_fo.var Gadt.Type.Float in
  (* Var 4 *)
  let volume_surge = Gadt_fo.var Gadt.Type.Float in
  (* Var 5 *)
  let adx_period = Gadt_fo.var Gadt.Type.Int in
  (* Var 6 *)

  (* Create EMAs *)
  let fast_ema =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.ema", Tacaml.Indicator.Raw.ema), fast_ema_period) ))
  in
  let slow_ema =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.ema", Tacaml.Indicator.Raw.ema), slow_ema_period) ))
  in

  (* Create RSI *)
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period) ))
  in

  (* Create ADX *)
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), adx_period) ))
  in

  let volume_lagged = lag volume 10 in

  {
    name = "MomentumVolume_Opt";
    (* Entry: Buy strength with volume confirmation *)
    buy_trigger =
      fast_ema >. slow_ema (* Uptrend *)
      &&. (rsi >. rsi_min) (* Strength, not weakness! *)
      &&. (volume >. volume_lagged *. volume_surge) (* Volume surge *)
      &&. (adx >. Const (25.0, Float)) (* Strong trend *)
      &&. safe_to_enter ();
    (* Exit: Momentum fades *)
    sell_trigger =
      force_exit_eod ()
      ||. (fast_ema <. slow_ema) (* Trend breaks *)
      ||. (rsi <. Const (45.0, Float)) (* Momentum fading *)
      ||. (volume <. volume_lagged *. Const (0.6, Float))
      ||. stop_loss 0.03 (* 3% stop *)
      ||. profit_target 0.10 (* 10% target *)
      ||. max_holding_time 120;
    score = rsi *. (volume /. volume_lagged);
    (* Higher RSI * volume surge = higher score *)
    max_positions = 6;
    (* Concentrate in winners *)
    position_size = 0.167;
    (* ~16.7% per position *)
  }

(** VolatileDip_Opt - Mean Reversion in Volatile Winners (FIXED v2)

    THESIS: Even "winner stocks" have intraday dips. Buy dips in volatile,
    high-volume stocks (the winners), expecting quick mean reversion.

    This combines mean reversion ENTRY with momentum/volatility FILTERS.

    FIXES from v1:
    - Changed from "ATR expansion" to "absolute ATR threshold" (avoid extreme
      multipliers)
    - Changed from "volume surge" to "above average volume" (more stable)
    - Changed from EMA(20) to variable SMA for trend (more generous,
      optimizable)
    - Separates stock selection (filters) from entry timing (signals)

    Variables to optimize (7 total): 1. rsi_period: [5, 20] - RSI for dip
    detection 2. rsi_oversold: [20.0, 40.0] - Oversold threshold 3. bb_period:
    [10, 30] - BB for reactive entry 4. atr_period: [10, 25] - ATR calculation
    period 5. atr_threshold: [0.3, 2.5] - Absolute minimum ATR (not multiplier!)
    6. volume_multiplier: [1.0, 2.5] - Volume vs 20-lag baseline (capped at
    2.5x) 7. trend_sma_period: [30, 80] - Longer-term trend filter

    Entry Logic - "Dip in a Volatile, High-Volume Winner": FILTERS (identify
    "winner stocks"):
    - ATR(var4) > var5 (absolute threshold - stock is volatile enough)
    - Volume > lag(volume, 20) * var6 (above baseline volume, capped multiplier)
    - Price > SMA(var7) (longer-term uptrend context - generous)

    ENTRY SIGNALS (mean reversion):
    - RSI(var1) < var2 (temporary dip)
    - Price < lower BB(var3) (oversold short-term)
    - Safe to enter

    Exit Logic - "Quick Mean Reversion":
    - Force exit EOD
    - Price > middle BB (dip recovered)
    - RSI > 65 (momentum normalized)
    - 2% stop loss (tight, this should bounce quickly)
    - 6% profit target
    - 60 tick max hold (quick reversions)

    Why this works for your thesis:
    - Filters for volatile, high-volume stocks (the "winners")
    - Buys temporary dips in these winners
    - Expects quick mean reversion in fundamentally strong stocks
    - Doesn't waste capital on dead/low-volatility stocks
    - Fixed geometry: no contradiction between uptrend filter and oversold entry
*)
let volatile_dip_opt =
  (* Variables with explicit bounds for optimizer *)
  let rsi_period = Gadt_fo.var ~lower:5.0 ~upper:20.0 Gadt.Type.Int in
  let rsi_oversold = Gadt_fo.var ~lower:20.0 ~upper:40.0 Gadt.Type.Float in
  let bb_period = Gadt_fo.var ~lower:10.0 ~upper:30.0 Gadt.Type.Int in
  let atr_period = Gadt_fo.var ~lower:10.0 ~upper:25.0 Gadt.Type.Int in
  let atr_threshold = Gadt_fo.var ~lower:0.3 ~upper:2.5 Gadt.Type.Float in
  (* Absolute threshold, not multiplier! *)
  let volume_multiplier = Gadt_fo.var ~lower:1.0 ~upper:2.5 Gadt.Type.Float in
  (* Capped at 2.5x *)
  let trend_sma_period = Gadt_fo.var ~lower:30.0 ~upper:80.0 Gadt.Type.Int in

  (* Create RSI *)
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period) ))
  in

  (* Create Bollinger Bands *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_period,
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_period,
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Create ATR for volatility filter - absolute threshold, not expansion *)
  let atr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.atr", Tacaml.Indicator.Raw.atr), atr_period) ))
  in

  (* Create variable-period SMA for trend filter (more generous than EMA(20)) *)
  let trend_sma =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", Tacaml.Indicator.Raw.sma), trend_sma_period) ))
  in

  (* Volume: Use simple lagged comparison (stable baseline)
     We compare current volume to volume 20 periods ago, multiplied by threshold.
     This is simpler than trying to compute SMA of volume. *)
  let volume_baseline = lag volume 20 in

  {
    name = "VolatileDip_Opt";
    (* Entry: Dip in a volatile, high-volume winner *)
    buy_trigger =
      (* FILTERS: Identify "winner stocks" *)
      atr >. atr_threshold (* Absolute volatility threshold *)
      &&. (volume >. volume_baseline *. volume_multiplier)
          (* Above baseline volume (capped) *)
      &&. (last >. trend_sma) (* Longer-term uptrend (generous) *)
      (* ENTRY SIGNALS: Mean reversion *)
      &&. (rsi <. rsi_oversold) (* Temporary dip *)
      &&. (last <. bb_lower) (* Oversold short-term *)
      &&. safe_to_enter ();
    (* Exit: Quick mean reversion *)
    sell_trigger =
      force_exit_eod ()
      ||. (last >. bb_middle) (* Recovered to mean *)
      ||. (rsi >. Const (65.0, Float)) (* Momentum normalized *)
      ||. stop_loss 0.02 (* 2% stop *)
      ||. profit_target 0.06 (* 6% target *)
      ||. max_holding_time 60;
    (* Quick exit *)
    score = (Const (100.0, Float) -. rsi) *. atr;
    (* Lower RSI * higher volatility = higher score *)
    max_positions = 10;
    position_size = 0.10;
  }

(* Export all strategies defined in this module *)
let all_strategies =
  [
    estridatter_5_0_0;
    (* Fifth optimized - 24% annualized, extreme oversold entry (RSI<20.17) *)
    estridatter_4_0_0;
    (* Fourth optimized - 12.8% over 2 weeks, 285 trades/day, worse after costs *)
    estridatter_3_0_0;
    (* Third optimized - 10.1% over 2 weeks, 79.84% win rate, better risk-adjusted *)
    estridatter_3_0_0_debug;
    (* DEBUG: Single-position variant of 3.0.0 *)
    estridatter_2_0_0;
    (* PRODUCTION: Second optimized version - 71.3% return, 71% win rate *)
    estridatter_1_0_0;
    (* PRODUCTION: First valid optimized version (proper bounds) *)
    estridatter_var;
    (* Optimizable version with all fixes *)
    estridatter_var_wide;
    (* Wide diversification variant - 20 positions @ 5% each *)
    estridatter_var_ultra_wide;
    (* Ultra wide variant - 30 positions @ 3.3% each *)
    estridatter_fixed;
    (* Fixed version with hardcoded params from old optimization *)
    estridatter;
    (* Keep original buggy version for comparison *)
    cosmiccowgirl_var;
    (* 4-parameter minimal mean reversion - efficient optimization *)
    cosmic_cowgirl_25_52_90;
    sharp_entry_patient_exit;
    patient_entry_77_46;
    volatility_breakout_opt;
    momentum_volume_opt;
    volatile_dip_opt;
  ]
