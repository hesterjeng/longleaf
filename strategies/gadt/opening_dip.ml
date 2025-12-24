(* Opening Dip Buy Strategies - Locked versions from ISRES optimization

   Trained on q3q4-2024-1min.json with 500 iterations.

   Strategy concept: Buy stocks that have dipped ~1% from open with
   oversold MFI at exactly 15 minutes after open (9:45 AM ET),
   then ride the mean reversion recovery toward EOD.

   Key constraint: Only buy during 1-minute window at 9:45 AM.
   This limits to max 5 entries/day = low execution drag.

   All three variants showed statistically significant edge (p < 0.002).
*)

open Gadt
open Gadt_strategy
module I = Tacaml.Indicator.Raw

(* Helper functions - same pattern as strategy_library.ml *)
let stop_loss stop_loss_pct : bool Gadt.t =
  let multiplier = Float.(1.0 - stop_loss_pct) in
  last <. EntryPrice *. Const (multiplier, Float)

let profit_target profit_target_pct : bool Gadt.t =
  let multiplier = Float.(1.0 + profit_target_pct) in
  last >. EntryPrice *. Const (multiplier, Float)

let max_holding_time max_ticks : bool Gadt.t =
  App2 (Fun (">", ( > )), TicksHeld, Const (max_ticks, Int))

(* Opening_Dip_A: Shortest hold, widest gap tolerance

   Training results (q3q4-2024):
   - Return: 13.2% ($113,189)
   - Win Rate: 58.63% (163W / 115L)
   - Trades: 278
   - Sharpe: 0.181
   - P-value: 0.0013
   - Profit Factor: 1.59 *)
let opening_dip_a : Gadt_strategy.t =
  let open Type in

  (* Locked parameters from optimization run 1 *)
  let mfi_period = 100 in
  let mfi_threshold = 41.76 in
  let max_gap = 4.22 in
  let max_change = -0.91 in
  let stop_pct = 0.0216 in  (* 2.16% as decimal *)
  let profit_pct = 0.0296 in  (* 2.96% as decimal *)
  let max_hold = 197 in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (mfi_period, Int)) ))
  in

  (* Entry: ONLY at minute 15, targeting WEAKNESS *)
  let in_buy_window = buy_window_at 15.0 in
  let oversold = mfi <. Const (mfi_threshold, Float) in
  let gap_ok = GapPct <. Const (max_gap, Float) in
  let dipped = DayChangePct <. Const (max_change, Float) in
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. oversold &&. gap_ok &&. dipped in

  (* Exit: Stop, profit, max hold, or EOD *)
  let sell_trigger =
    stop_loss stop_pct
    ||. profit_target profit_pct
    ||. max_holding_time max_hold
    ||. force_exit_eod ()
  in

  (* Score: Most oversold + biggest dip = highest priority *)
  let score = Const (100.0, Float) -. mfi -. DayChangePct *. Const (10.0, Float) in

  {
    name = "Opening_Dip_A";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 5;
    position_size = 0.20;
  }

(* Opening_Dip_B: Longest hold (327 ticks), widest stop (3.55%)

   Training results (q3q4-2024):
   - Return: 13.8% ($113,844)
   - Win Rate: 60.00% (132W / 88L)
   - Trades: 220
   - Sharpe: 0.200
   - P-value: 0.0015
   - Profit Factor: 1.68 *)
let opening_dip_b : Gadt_strategy.t =
  let open Type in

  (* Locked parameters from optimization run 2 *)
  let mfi_period = 96 in
  let mfi_threshold = 41.52 in
  let max_gap = 3.90 in
  let max_change = -1.06 in
  let stop_pct = 0.0355 in  (* 3.55% as decimal *)
  let profit_pct = 0.0259 in  (* 2.59% as decimal *)
  let max_hold = 327 in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (mfi_period, Int)) ))
  in

  (* Entry: ONLY at minute 15, targeting WEAKNESS *)
  let in_buy_window = buy_window_at 15.0 in
  let oversold = mfi <. Const (mfi_threshold, Float) in
  let gap_ok = GapPct <. Const (max_gap, Float) in
  let dipped = DayChangePct <. Const (max_change, Float) in
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. oversold &&. gap_ok &&. dipped in

  (* Exit: Stop, profit, max hold, or EOD *)
  let sell_trigger =
    stop_loss stop_pct
    ||. profit_target profit_pct
    ||. max_holding_time max_hold
    ||. force_exit_eod ()
  in

  (* Score: Most oversold + biggest dip = highest priority *)
  let score = Const (100.0, Float) -. mfi -. DayChangePct *. Const (10.0, Float) in

  {
    name = "Opening_Dip_B";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 5;
    position_size = 0.20;
  }

(* Opening_Dip_C: Medium hold, tightest dip requirement (-1.08%)

   Training results (q3q4-2024):
   - Return: 12.0% ($111,987)
   - Win Rate: 60.49% (124W / 81L)
   - Trades: 205
   - Sharpe: 0.209
   - P-value: 0.0014
   - Profit Factor: 1.69 *)
let opening_dip_c : Gadt_strategy.t =
  let open Type in

  (* Locked parameters from optimization run 3 *)
  let mfi_period = 102 in
  let mfi_threshold = 41.30 in
  let max_gap = 3.08 in
  let max_change = -1.08 in
  let stop_pct = 0.0272 in  (* 2.72% as decimal *)
  let profit_pct = 0.0238 in  (* 2.38% as decimal *)
  let max_hold = 262 in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (mfi_period, Int)) ))
  in

  (* Entry: ONLY at minute 15, targeting WEAKNESS *)
  let in_buy_window = buy_window_at 15.0 in
  let oversold = mfi <. Const (mfi_threshold, Float) in
  let gap_ok = GapPct <. Const (max_gap, Float) in
  let dipped = DayChangePct <. Const (max_change, Float) in
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. oversold &&. gap_ok &&. dipped in

  (* Exit: Stop, profit, max hold, or EOD *)
  let sell_trigger =
    stop_loss stop_pct
    ||. profit_target profit_pct
    ||. max_holding_time max_hold
    ||. force_exit_eod ()
  in

  (* Score: Most oversold + biggest dip = highest priority *)
  let score = Const (100.0, Float) -. mfi -. DayChangePct *. Const (10.0, Float) in

  {
    name = "Opening_Dip_C";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 5;
    position_size = 0.20;
  }

(* ============================================================
   V2 Opening Dip Buy Strategies - Extended buy window (15-45 min)

   These use a 30-minute buy window instead of exact minute 15.
   Each trained on different periods with ISRES optimization.
   ============================================================ *)

(* Opening_Dip_V2_A: Trained on q3q4-2025

   Training results:
   - Return: 25.1% ($125,068)
   - Win Rate: 58.44% (90W / 64L)
   - Trades: 154
   - Sharpe: 0.255
   - P-value: 0.0008
   - Profit Factor: 1.976 *)
let opening_dip_v2_a : Gadt_strategy.t =
  let open Type in

  (* Locked parameters from q3q4-2025 optimization *)
  let mfi_period = 145 in
  let mfi_threshold = 51.43 in
  let max_gap = -1.77 in      (* Gap DOWN required *)
  let max_change = 1.53 in
  let stop_pct = 0.0454 in    (* 4.54% *)
  let profit_pct = 0.0502 in  (* 5.02% *)
  let max_hold = 170 in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (mfi_period, Int)) ))
  in

  (* Entry: 15-45 minute window, targeting gap-down weakness *)
  let in_buy_window =
    (MinutesSinceOpen >=. Const (15.0, Float)) &&.
    (MinutesSinceOpen <. Const (45.0, Float))
  in
  let oversold = mfi <. Const (mfi_threshold, Float) in
  let gap_ok = GapPct <. Const (max_gap, Float) in
  let change_ok = DayChangePct <. Const (max_change, Float) in
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. oversold &&. gap_ok &&. change_ok in

  let sell_trigger =
    stop_loss stop_pct
    ||. profit_target profit_pct
    ||. max_holding_time max_hold
    ||. force_exit_eod ()
  in

  let score = Const (100.0, Float) -. mfi -. DayChangePct *. Const (10.0, Float) in

  {
    name = "Opening_Dip_V2_A";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 5;
    position_size = 0.20;
  }

(* Opening_Dip_V2_B: Trained on q1q2-2025

   Training results:
   - Return: 33.9% ($133,938)
   - Win Rate: 62.53% (227W / 136L)
   - Trades: 363
   - Sharpe: 0.162
   - P-value: 0.0011
   - Profit Factor: 1.582 *)
let opening_dip_v2_b : Gadt_strategy.t =
  let open Type in

  (* Locked parameters from q1q2-2025 optimization *)
  let mfi_period = 13 in
  let mfi_threshold = 22.48 in
  let max_gap = 4.43 in
  let max_change = -0.45 in   (* Must be dipping *)
  let stop_pct = 0.0477 in    (* 4.77% *)
  let profit_pct = 0.0601 in  (* 6.01% *)
  let max_hold = 257 in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (mfi_period, Int)) ))
  in

  (* Entry: 15-45 minute window *)
  let in_buy_window =
    (MinutesSinceOpen >=. Const (15.0, Float)) &&.
    (MinutesSinceOpen <. Const (45.0, Float))
  in
  let oversold = mfi <. Const (mfi_threshold, Float) in
  let gap_ok = GapPct <. Const (max_gap, Float) in
  let change_ok = DayChangePct <. Const (max_change, Float) in
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. oversold &&. gap_ok &&. change_ok in

  let sell_trigger =
    stop_loss stop_pct
    ||. profit_target profit_pct
    ||. max_holding_time max_hold
    ||. force_exit_eod ()
  in

  let score = Const (100.0, Float) -. mfi -. DayChangePct *. Const (10.0, Float) in

  {
    name = "Opening_Dip_V2_B";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 5;
    position_size = 0.20;
  }

(* Opening_Dip_V2_C: Trained on q3q4-2024

   Training results:
   - Return: 11.9% ($111,936)
   - Win Rate: 67.27% (37W / 18L)
   - Trades: 55
   - Sharpe: 0.386
   - P-value: 0.0025
   - Profit Factor: 2.754 *)
let opening_dip_v2_c : Gadt_strategy.t =
  let open Type in

  (* Locked parameters from q3q4-2024 optimization *)
  let mfi_period = 91 in
  let mfi_threshold = 49.80 in
  let max_gap = -3.45 in      (* Gap DOWN required *)
  let max_change = 0.87 in
  let stop_pct = 0.0495 in    (* 4.95% *)
  let profit_pct = 0.0594 in  (* 5.94% *)
  let max_hold = 204 in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (mfi_period, Int)) ))
  in

  (* Entry: 15-45 minute window, targeting gap-down weakness *)
  let in_buy_window =
    (MinutesSinceOpen >=. Const (15.0, Float)) &&.
    (MinutesSinceOpen <. Const (45.0, Float))
  in
  let oversold = mfi <. Const (mfi_threshold, Float) in
  let gap_ok = GapPct <. Const (max_gap, Float) in
  let change_ok = DayChangePct <. Const (max_change, Float) in
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. oversold &&. gap_ok &&. change_ok in

  let sell_trigger =
    stop_loss stop_pct
    ||. profit_target profit_pct
    ||. max_holding_time max_hold
    ||. force_exit_eod ()
  in

  let score = Const (100.0, Float) -. mfi -. DayChangePct *. Const (10.0, Float) in

  {
    name = "Opening_Dip_V2_C";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 5;
    position_size = 0.20;
  }

(* ============================================================
   V3 Opening Dip Buy - Multi-angle entry filters (Optimizable)

   Goes beyond stacking oversold indicators by incorporating:
   1. Trend context (ADX) - Favor range-bound markets for mean reversion
   2. Intraday price location - Buy near day's low, not dead cat bounces
   3. Prior momentum - Yesterday's strength indicates quality stock
   4. Oversold detection (MFI) - Core signal

   Each filter captures genuinely different market information.
   ============================================================ *)

let opening_dip_v3_opt : Gadt_strategy.t =
  let open Gadt in
  let open Type in

  (* === OPTIMIZABLE PARAMETERS === *)

  (* Core oversold detection *)
  let mfi_period_var = Gadt_fo.var ~lower:10.0 ~upper:200.0 Int in
  let mfi_threshold_var = Gadt_fo.var ~lower:15.0 ~upper:55.0 Float in

  (* Gap and day change filters (from V2) *)
  let max_gap_var = Gadt_fo.var ~lower:(-5.0) ~upper:5.0 Float in
  let max_change_var = Gadt_fo.var ~lower:(-3.0) ~upper:3.0 Float in

  (* NEW: Trend context - ADX below threshold means range-bound *)
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:100.0 Int in
  let adx_threshold_var = Gadt_fo.var ~lower:15.0 ~upper:40.0 Float in

  (* NEW: Intraday price location - how close to day's low (as %) *)
  let near_low_pct_var = Gadt_fo.var ~lower:0.1 ~upper:3.0 Float in

  (* NEW: Previous day strength - close above X% of day's range *)
  let prev_strength_var = Gadt_fo.var ~lower:0.3 ~upper:0.7 Float in

  (* Exit parameters *)
  let stop_pct_var = Gadt_fo.var ~lower:1.0 ~upper:6.0 Float in
  let profit_pct_var = Gadt_fo.var ~lower:1.0 ~upper:10.0 Float in
  let max_hold_var = Gadt_fo.var ~lower:60.0 ~upper:350.0 Int in

  (* === INDICATORS === *)

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), mfi_period_var) ))
  in

  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.adx", I.adx), adx_period_var) ))
  in

  (* === BUY CONDITIONS === *)

  (* Time window: 15-45 minutes after open *)
  let in_buy_window =
    (MinutesSinceOpen >=. Const (15.0, Float)) &&.
    (MinutesSinceOpen <. Const (45.0, Float))
  in

  (* 1. Oversold: MFI below threshold *)
  let oversold = mfi <. mfi_threshold_var in

  (* 2. Gap filter: Not gapping too extreme *)
  let gap_ok = GapPct <. max_gap_var in

  (* 3. Day change filter: Down from open *)
  let change_ok = DayChangePct <. max_change_var in

  (* 4. NEW: Range-bound market - ADX below threshold *)
  (* Low ADX = no strong trend = mean reversion more likely to work *)
  let range_bound = adx <. adx_threshold_var in

  (* 5. NEW: Near day's low - buying close to the bottom *)
  (* (Last - DayLow) / DayLow * 100 < threshold *)
  let dist_from_low = (last -. DayLow) /. DayLow *. Const (100.0, Float) in
  let near_low = dist_from_low <. near_low_pct_var in

  (* 6. NEW: Previous day strength - yesterday closed in upper portion of range *)
  (* PrevDayClose > PrevDayLow + (PrevDayHigh - PrevDayLow) * threshold *)
  let prev_range = PrevDayHigh -. PrevDayLow in
  let strength_level = PrevDayLow +. prev_range *. prev_strength_var in
  let prev_strong = PrevDayClose >. strength_level in

  let no_position = not_ HasPosition in

  let buy_trigger =
    no_position &&. in_buy_window &&.
    oversold &&. gap_ok &&. change_ok &&.
    range_bound &&. near_low &&. prev_strong
  in

  (* === SELL CONDITIONS === *)

  let stop_mult = Const (1.0, Float) -. stop_pct_var /. Const (100.0, Float) in
  let profit_mult = Const (1.0, Float) +. profit_pct_var /. Const (100.0, Float) in
  let stop_hit = last <. EntryPrice *. stop_mult in
  let target_hit = last >. EntryPrice *. profit_mult in
  let max_hold_exit = App2 (Fun (">", ( > )), TicksHeld, max_hold_var) in
  let eod = Gadt_strategy.force_exit_eod () in

  let sell_trigger = stop_hit ||. target_hit ||. max_hold_exit ||. eod in

  (* Score: Most oversold + biggest dip + closest to low = highest priority *)
  let score =
    Const (100.0, Float) -.
    mfi -.
    DayChangePct *. Const (5.0, Float) -.
    dist_from_low *. Const (10.0, Float)
  in

  {
    name = "Opening_Dip_V3_Opt";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 2;
    position_size = 0.50;
  }

(* ============================================================
   V4 Opening Dip Buy - Minimal filters (Optimizable)

   Stripped down to core signal only:
   - Time window (15-45 min after open)
   - MFI oversold

   Let ISRES find optimal exit parameters.
   ============================================================ *)

let opening_dip_v4_opt : Gadt_strategy.t =
  let open Gadt in
  let open Type in

  (* === OPTIMIZABLE PARAMETERS === *)

  (* Core oversold detection *)
  let mfi_period_var = Gadt_fo.var ~lower:10.0 ~upper:200.0 Int in
  let mfi_threshold_var = Gadt_fo.var ~lower:15.0 ~upper:55.0 Float in

  (* Exit parameters *)
  let stop_pct_var = Gadt_fo.var ~lower:1.0 ~upper:6.0 Float in
  let profit_pct_var = Gadt_fo.var ~lower:1.0 ~upper:10.0 Float in
  let max_hold_var = Gadt_fo.var ~lower:60.0 ~upper:350.0 Int in

  (* === INDICATORS === *)

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), mfi_period_var) ))
  in

  (* === BUY CONDITIONS === *)

  (* Time window: 15-45 minutes after open *)
  let in_buy_window =
    (MinutesSinceOpen >=. Const (15.0, Float)) &&.
    (MinutesSinceOpen <. Const (45.0, Float))
  in

  (* Oversold: MFI below threshold *)
  let oversold = mfi <. mfi_threshold_var in

  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. oversold in

  (* === SELL CONDITIONS === *)

  let stop_mult = Const (1.0, Float) -. stop_pct_var /. Const (100.0, Float) in
  let profit_mult = Const (1.0, Float) +. profit_pct_var /. Const (100.0, Float) in
  let stop_hit = last <. EntryPrice *. stop_mult in
  let target_hit = last >. EntryPrice *. profit_mult in
  let max_hold_exit = App2 (Fun (">", ( > )), TicksHeld, max_hold_var) in
  let eod = Gadt_strategy.force_exit_eod () in

  let sell_trigger = stop_hit ||. target_hit ||. max_hold_exit ||. eod in

  (* Score: Most oversold = highest priority *)
  let score = Const (100.0, Float) -. mfi in

  {
    name = "Opening_Dip_V4_Opt";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 2;
    position_size = 0.50;
  }

(* Opening_Dip_V4: Locked baseline version

   Simple oversold-in-morning-window strategy with sensible defaults.
   Use this as a baseline before optimization. *)
let opening_dip_v4 : Gadt_strategy.t =
  let open Type in

  (* Locked parameters - sensible defaults for minute bars *)
  let mfi_period = 15 in       (* Current session only, no yesterday bleed *)
  let mfi_threshold = 30.0 in  (* Classic oversold level *)
  let min_gap = 0.3 in         (* Minimum gap up % - shows overnight strength *)
  let stop_pct = 0.025 in      (* 2.5% stop *)
  let profit_pct = 0.20 in     (* 20% - effectively never triggers, let winners run *)
  let max_hold = 390 in        (* Hold until EOD *)

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (mfi_period, Int)) ))
  in

  (* Entry: 15-45 minute window, gapped UP but now oversold *)
  let in_buy_window =
    (MinutesSinceOpen >=. Const (15.0, Float)) &&.
    (MinutesSinceOpen <. Const (45.0, Float))
  in
  let gapped_up = GapPct >. Const (min_gap, Float) in
  let oversold = mfi <. Const (mfi_threshold, Float) in
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. gapped_up &&. oversold in

  let sell_trigger =
    stop_loss stop_pct
    ||. profit_target profit_pct
    ||. max_holding_time max_hold
    ||. force_exit_eod ()
  in

  let score = Const (100.0, Float) -. mfi in

  {
    name = "Opening_Dip_V4";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 2;
    position_size = 0.50;
  }

(* Export all strategies *)
let all_strategies =
  [
    opening_dip_a;
    opening_dip_b;
    opening_dip_c;
    opening_dip_v2_a;
    opening_dip_v2_b;
    opening_dip_v2_c;
    opening_dip_v3_opt;
    opening_dip_v4_opt;
    opening_dip_v4;
  ]
