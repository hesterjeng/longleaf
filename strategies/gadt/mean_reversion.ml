(** Mean Reversion Strategies

    This module contains mean reversion strategies, particularly those based on
    the Nature Boy family which use MFI, Bollinger Bands, and NATR filtering. *)

open Gadt
open Gadt_strategy
module Data = Longleaf_bars.Data

(** Nature_Boy_V3_Opt - Optimizable Mean Reversion Strategy

    Based on Nature Boy V2, with all constants replaced by optimization
    variables. This allows NLopt to explore the full parameter space.

    Variables to optimize (14 total): 1. mfi_period: [50, 300] - MFI lookback
    period 2. natr_lo_period: [5, 50] - Short NATR period (stale data detection)
    3. natr_hi_period: [50, 200] - Long NATR period (regime detection) 4.
    bb_entry_period: [100, 400] - Entry Bollinger Band period 5. bb_entry_std:
    [1.0, 3.0] - Entry BB standard deviation 6. bb_exit_period: [100, 400] -
    Exit Bollinger Band period 7. mfi_oversold: [20.0, 50.0] - MFI oversold
    threshold for entry 8. mfi_exit: [40.0, 70.0] - MFI exit threshold 9.
    natr_lo_threshold: [0.05, 1.0] - Min NATR (stale data filter) 10.
    natr_hi_threshold: [1.0, 6.0] - Max NATR (regime filter) 11. min_hold:
    [10, 100] - Minimum holding period in ticks 12. stop_loss_mult: [0.85, 0.95]
    \- Stop loss as multiplier of entry price 13. profit_target_mult:
    [1.05, 1.20] - Profit target as multiplier of entry 14. max_hold: [100, 500]
    \- Maximum holding period in ticks

    Base strategy: Nature Boy V2 (locked values):
    - mfi_period = 190, mfi_oversold = 36.59
    - natr_lo_period = 17, natr_lo_threshold = 0.15
    - natr_hi_period = 141, natr_hi_threshold = 3.83
    - bb_entry_period = 248, bb_entry_std = 1.61
    - bb_exit_period = 275
    - mfi_exit = 52.05
    - min_hold = 36, max_hold = 258
    - stop_loss = 9.07%, profit_target = 10.35%

    NOTE: Use starting index of at least 400 (-i 400) to allow longest indicator
    periods to warm up. *)
let nature_boy_v3_opt =
  (* Variable 1: MFI period *)
  let mfi_period_var = Gadt_fo.var ~lower:50.0 ~upper:300.0 Type.Int in
  (* Variable 2: Short NATR period *)
  let natr_lo_period_var = Gadt_fo.var ~lower:5.0 ~upper:50.0 Type.Int in
  (* Variable 3: Long NATR period *)
  let natr_hi_period_var = Gadt_fo.var ~lower:50.0 ~upper:200.0 Type.Int in
  (* Variable 4: Entry BB period *)
  let bb_entry_period_var = Gadt_fo.var ~lower:100.0 ~upper:400.0 Type.Int in
  (* Variable 5: Entry BB std dev *)
  let bb_entry_std_var = Gadt_fo.var ~lower:1.0 ~upper:3.0 Type.Float in
  (* Variable 6: Exit BB period *)
  let bb_exit_period_var = Gadt_fo.var ~lower:100.0 ~upper:400.0 Type.Int in
  (* Variable 7: MFI oversold threshold *)
  let mfi_oversold_var = Gadt_fo.var ~lower:20.0 ~upper:50.0 Type.Float in
  (* Variable 8: MFI exit threshold *)
  let mfi_exit_var = Gadt_fo.var ~lower:40.0 ~upper:70.0 Type.Float in
  (* Variable 9: NATR lower threshold *)
  let natr_lo_threshold_var = Gadt_fo.var ~lower:0.05 ~upper:1.0 Type.Float in
  (* Variable 10: NATR upper threshold *)
  let natr_hi_threshold_var = Gadt_fo.var ~lower:1.0 ~upper:6.0 Type.Float in
  (* Variable 11: Minimum hold ticks *)
  let min_hold_var = Gadt_fo.var ~lower:10.0 ~upper:100.0 Type.Int in
  (* Variable 12: Stop loss multiplier *)
  let stop_loss_mult_var = Gadt_fo.var ~lower:0.85 ~upper:0.95 Type.Float in
  (* Variable 13: Profit target multiplier *)
  let profit_target_mult_var = Gadt_fo.var ~lower:1.05 ~upper:1.20 Type.Float in
  (* Variable 14: Maximum hold ticks *)
  let max_hold_var = Gadt_fo.var ~lower:100.0 ~upper:500.0 Type.Int in

  (* Entry MFI indicator with variable period *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period_var) ))
  in

  (* NATR for lower bound - short period for stale data detection *)
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), natr_lo_period_var)
         ))
  in

  (* NATR for upper bound - longer period for regime detection *)
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), natr_hi_period_var)
         ))
  in

  (* Entry Bollinger Band - variable period and std dev *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_entry_period_var,
               bb_entry_std_var,
               bb_entry_std_var ) ))
  in

  (* Exit Bollinger Band middle - variable period *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_exit_period_var,
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Recovery filter - price recovering from recent low *)
  let recovering = last >. lag last 1 in

  (* Min hold gate - variable minimum holding period *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, min_hold_var) in

  (* Gated exit signals - variable thresholds *)
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle ||. (mfi >. mfi_exit_var)
        ||. (last >. EntryPrice *. profit_target_mult_var))
  in

  {
    name = "Nature_Boy_V3_Opt";
    buy_trigger =
      mfi <. mfi_oversold_var &&. (last <. bb_lower)
      &&. (natr_lo >. natr_lo_threshold_var) (* Stale data filter *)
      &&. (natr_hi <. natr_hi_threshold_var) (* Regime filter *)
      &&. recovering &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult_var)
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold_var);
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** MR0_Opt - Lean Mean Reversion Strategy (6 variables)

    Streamlined version of Nature Boy with structurally-justified fixed parameters
    and only essential optimization variables. Designed for S&P 100 stocks.

    Research basis:
    - S&P 100 spreads: ~3.7 bps (negligible)
    - Intraday mean reversion half-life: 1-4 hours for liquid large-caps
    - Daily volatility: 1-2% typical for S&P 100 constituents

    Fixed parameters (derived from first principles):
    - bb_std = 2.0 (standard Bollinger Band)
    - stop_loss = 8% (generous for oversold entries)
    - profit_target = 5% (typical intraday mean reversion)
    - min_hold = 30 bars (30 min - cover spread, allow reversion time)
    - max_hold = 240 bars (4 hours - mean reversion window)

    Optimization variables (6 total):
    1. mfi_period: [50, 200] - MFI lookback (1-3 hours on 1-min bars)
    2. mfi_oversold: [20, 45] - Entry threshold
    3. mfi_exit: [45, 65] - Exit threshold
    4. bb_period: [100, 300] - Bollinger Band period (~1.5-5 hours)
    5. natr_period: [30, 150] - Volatility regime filter period
    6. natr_max: [1.5, 5.0] - Max NATR (avoid high volatility regimes)

    Sources:
    - S&P 100 spreads: https://www.nasdaq.com/articles/sampling-sp-500-minimize-spreads
    - Mean reversion timescales: https://arxiv.org/html/2501.16772v1

    NOTE: Use starting index of at least 300 (-i 300) for indicator warmup. *)
let mr0_opt =
  (* Variable 1: MFI period - core momentum signal *)
  let mfi_period_var = Gadt_fo.var ~lower:50.0 ~upper:200.0 Type.Int in
  (* Variable 2: MFI oversold threshold *)
  let mfi_oversold_var = Gadt_fo.var ~lower:20.0 ~upper:45.0 Type.Float in
  (* Variable 3: MFI exit threshold *)
  let mfi_exit_var = Gadt_fo.var ~lower:45.0 ~upper:65.0 Type.Float in
  (* Variable 4: Bollinger Band period - single period for entry and exit *)
  let bb_period_var = Gadt_fo.var ~lower:100.0 ~upper:300.0 Type.Int in
  (* Variable 5: NATR period - volatility regime detection *)
  let natr_period_var = Gadt_fo.var ~lower:30.0 ~upper:150.0 Type.Int in
  (* Variable 6: NATR max threshold - avoid high volatility *)
  let natr_max_var = Gadt_fo.var ~lower:1.5 ~upper:5.0 Type.Float in

  (* Fixed parameters - structurally justified *)
  let bb_std = Const (2.0, Float) in
  let stop_loss_mult = Const (0.92, Float) in (* 8% stop *)
  let profit_target_mult = Const (1.05, Float) in (* 5% target *)
  let min_hold = Const (30, Int) in (* 30 minutes *)
  let max_hold = Const (240, Int) in (* 4 hours *)

  (* MFI indicator *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period_var) ))
  in

  (* NATR for volatility regime filter *)
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), natr_period_var) ))
  in

  (* Bollinger Bands - single period for both entry and exit *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_period_var,
               bb_std,
               bb_std ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_period_var,
               bb_std,
               bb_std ) ))
  in

  (* Recovery filter - price recovering from recent low *)
  let recovering = last >. lag last 1 in

  (* Min hold gate *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, min_hold) in

  (* Exit conditions *)
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. mfi_exit_var)
        ||. (last >. EntryPrice *. profit_target_mult))
  in

  {
    name = "MR0_Opt";
    buy_trigger =
      mfi <. mfi_oversold_var
      &&. (last <. bb_lower)
      &&. (natr <. natr_max_var) (* Avoid high volatility regimes *)
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(* Export all strategies *)
let all_strategies = [ nature_boy_v3_opt; mr0_opt ]
