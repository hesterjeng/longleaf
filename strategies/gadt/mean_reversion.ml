(** Mean Reversion Strategies

    This module contains mean reversion strategies, particularly those based on
    the Nature Boy family which use MFI, Bollinger Bands, and NATR filtering.

    == RESEARCH FINDINGS (Dec 2025) ==

    After extensive ISRES optimization and battery testing across 2023-2025 data
    with realistic execution costs (2 bps/side spread), we found:

    1. WITHOUT SLIPPAGE: Mean reversion shows strong edge (25-45% returns,
       statistically significant p<0.001, 60%+ win rates)

    2. WITH SLIPPAGE: Edge largely disappears or becomes regime-dependent.
       Most strategies that showed 40%+ returns in training periods produced
       -10% to +5% in out-of-sample periods.

    3. BEST RESULT: NB_V3_2 achieved 83.3% consistency (5/6 periods positive)
       with 4.58% avg return and 8.39% std dev - but this roughly matches
       passive SPY buy-and-hold, not worth the complexity.

    4. KEY INSIGHT: Tight volatility filtering (NATR_hi < 1.5) is critical for
       regime robustness. Strategies without this filter showed 2x variance.

    5. CONCLUSION: Simple intraday mean reversion on S&P 100 is a competed-away
       edge. Whatever signal exists gets eaten by execution costs. The remaining
       edge (~9% annualized) doesn't justify the effort over passive indexing.

    6. FAILURE MODES:
       - High-frequency MR (6000+ trades/period): destroyed by execution costs
       - Regime-fit strategies: +45% in training, -15% out-of-sample
       - Even "robust" strategies barely match SPY

    == BATTERY TEST RESULTS (quarterly_2023_2025) ==

    ID_MR_AT_1 (Simple SMA + MFI, trained q3q4-2025):
      q1q2-2023: +6.87%  | q3q4-2023: -3.44%  | q1q2-2024: -8.87%
      q3q4-2024: -4.49%  | q1q2-2025: -14.58% | q3q4-2025: +44.81% (training)
      Avg: 3.38%, Std: 19.62%, Consistency: 33.3%
      VERDICT: Severe overfitting to training period

    NB_V3_0 (Nature Boy V3, NATR_hi < 4.17):
      q1q2-2023: +1.37%  | q3q4-2023: +2.37%  | q1q2-2024: -12.95%
      q3q4-2024: -3.16%  | q1q2-2025: +0.19%  | q3q4-2025: +20.09% (training)
      Avg: 1.32%, Std: 9.82%, Consistency: 66.7%

    NB_V3_1 (Nature Boy V3, shorter MFI period 70):
      q1q2-2023: -3.16%  | q3q4-2023: -2.29%  | q1q2-2024: -12.85%
      q3q4-2024: -3.21%  | q1q2-2025: -0.43%  | q3q4-2025: +21.49% (training)
      Avg: -0.07%, Std: 10.43%, Consistency: 33.3%
      VERDICT: Shorter indicators overfit more

    NB_V3_2 (Nature Boy V3, tight NATR_hi < 1.41):
      q1q2-2023: +6.56%  | q3q4-2023: +1.95%  | q1q2-2024: -8.26%
      q3q4-2024: -0.25%  | q1q2-2025: +8.47%  | q3q4-2025: +18.99% (training)
      Avg: 4.58%, Std: 8.39%, Consistency: 83.3%
      VERDICT: BEST - tight volatility filter provides robustness

    Nature Boy V2 (original, pre-optimization):
      q1q2-2023: +1.57%  | q3q4-2023: -9.67%  | q1q2-2024: +3.70%
      q3q4-2024: -3.95%  | q1q2-2025: +0.10%  | q3q4-2025: +9.08%
      Avg: 0.14%, Std: 5.88%, Consistency: 66.7%
      VERDICT: Stable but negligible returns

    == KEY TAKEAWAYS ==

    - Volatility filtering (NATR_hi) is the most important parameter
    - Shorter indicator periods = more overfitting
    - Even best strategy (NB_V3_2) only achieves ~9% annualized
    - SPY returned 24% in 2023, 23% in 2024 - MR can't compete

    Strategies preserved here for reference. See momentum.ml for trend-following
    approaches. *)


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
    [10, 100] - Minimum holding period in ticks 12. stop_loss_mult: [0.95, 0.99]
    \- Stop loss 1-5% 13. profit_target_mult: [1.01, 1.10] - Profit target 1-10%
    14. max_hold: [100, 500] - Maximum holding period in ticks

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
  (* Variable 12: Stop loss multiplier - 1-5% stop loss *)
  let stop_loss_mult_var = Gadt_fo.var ~lower:0.95 ~upper:0.99 Type.Float in
  (* Variable 13: Profit target multiplier - 1-10% profit target (2x max stop) *)
  let profit_target_mult_var = Gadt_fo.var ~lower:1.01 ~upper:1.10 Type.Float in
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

(** MRNB_3_A - Nature Boy V3 Winner A (locked)

    Trained on q3q4-2025. Objective: 125,249.73
    192 trades, 60.94% win rate, Sharpe 0.310, p < 0.0001 *)
let mrnb_3_a =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (235, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (9, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (89, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (359, Int),
               Const (1.636, Float),
               Const (1.636, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (331, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (72, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (59.09, Float))
        ||. (last >. EntryPrice *. Const (1.068, Float)))
  in
  {
    name = "MRNB_3_A";
    buy_trigger =
      mfi <. Const (49.70, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.492, Float))
      &&. (natr_hi <. Const (3.261, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.868, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (171, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** MRNB_3_B - Nature Boy V3 Winner B (locked)

    Trained on q3q4-2025. Objective: 122,756.98
    249 trades, 60.64% win rate, Sharpe 0.213, p < 0.0004 *)
let mrnb_3_b =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (213, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (7, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (186, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (228, Int),
               Const (1.662, Float),
               Const (1.662, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (333, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (64, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (65.77, Float))
        ||. (last >. EntryPrice *. Const (1.150, Float)))
  in
  {
    name = "MRNB_3_B";
    buy_trigger =
      mfi <. Const (49.11, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.488, Float))
      &&. (natr_hi <. Const (2.779, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.871, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (396, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** MRNB_3_C - Nature Boy V3 Winner C (locked)

    Trained on q3q4-2025. Objective: 123,321.08
    214 trades, 64.49% win rate, Sharpe 0.246, p < 0.0002 *)
let mrnb_3_c =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (230, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (18, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (179, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (315, Int),
               Const (2.610, Float),
               Const (2.610, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (287, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (48, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (60.55, Float))
        ||. (last >. EntryPrice *. Const (1.078, Float)))
  in
  {
    name = "MRNB_3_C";
    buy_trigger =
      mfi <. Const (48.98, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.347, Float))
      &&. (natr_hi <. Const (5.649, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.872, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (441, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** MR_Basic - Baseline Mean Reversion (no optimization)

    Textbook mean reversion with sensible defaults for 1-minute bars.
    No ISRES training - just reasonable parameters from first principles.

    Purpose: Baseline comparison for optimized strategies. If ISRES-trained
    strategies can't beat this, the optimization is finding noise, not signal.

    Parameters (all fixed, chosen from common practice):
    - MFI period: 90 (1.5 hours - smoothed momentum)
    - MFI oversold: 30 (standard oversold threshold)
    - MFI exit: 50 (neutral - mean reverted)
    - BB period: 120 (2 hours)
    - BB std: 2.0 (standard)
    - NATR period: 60 (1 hour)
    - NATR max: 3.0 (avoid high volatility)
    - Stop loss: 5%
    - Profit target: 4%
    - Min hold: 20 bars (20 minutes - avoid noise)
    - Max hold: 180 bars (3 hours - intraday window)

    NOTE: Use starting index of at least 150 (-i 150) for indicator warmup. *)
let mr_basic =
  (* Fixed parameters - textbook values *)
  let mfi_period = Const (90, Int) in
  let mfi_oversold = Const (30.0, Float) in
  let mfi_exit = Const (50.0, Float) in
  let bb_period = Const (120, Int) in
  let bb_std = Const (2.0, Float) in
  let natr_period = Const (60, Int) in
  let natr_max = Const (3.0, Float) in
  let stop_loss_mult = Const (0.95, Float) in
  let profit_target_mult = Const (1.04, Float) in
  let min_hold = Const (20, Int) in
  let max_hold = Const (180, Int) in

  (* MFI indicator *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period) ))
  in

  (* NATR for volatility filter *)
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), natr_period) ))
  in

  (* Bollinger Bands *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  (* Simple recovery filter *)
  let recovering = last >. lag last 1 in

  (* Min hold gate *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, min_hold) in

  (* Exit conditions *)
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. mfi_exit)
        ||. (last >. EntryPrice *. profit_target_mult))
  in

  {
    name = "MR_Basic";
    buy_trigger =
      mfi <. mfi_oversold
      &&. (last <. bb_lower)
      &&. (natr <. natr_max)
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

(** Intraday_Momentum_10AM - Simple Intraday Momentum Strategy

    Tests the hypothesis: "morning winners keep winning through the day"

    Entry (10:00-10:15 AM only):
    - 30-45 minutes after market open
    - Price > price 30 bars ago (positive momentum)

    Score:
    - Return over last 30 bars (higher return = higher priority)

    Exit:
    - End of day (force_exit_eod)
    - Stop loss: 2% below entry
    - Momentum reversal: price drops below 5-bar ago low

    NOTE: Use starting index of at least 50 (-i 50) for lag warmup. *)
let intraday_momentum_10am =
  (* Entry window: 30-45 minutes after open (10:00-10:15 AM) *)
  let minutes_since = minutes_since_open TickTime in
  let in_entry_window =
    (minutes_since >=. Const (30.0, Float))
    &&. (minutes_since <=. Const (45.0, Float))
  in

  (* Momentum: price is higher than 30 bars ago *)
  let price_30_ago = lag last 30 in
  let momentum_positive = last >. price_30_ago in

  (* Return calculation for scoring *)
  let return_30 = (last -. price_30_ago) /. price_30_ago in

  (* Short-term reversal detection: price below recent low *)
  let recent_low = lag low 5 in
  let momentum_reversal = last <. recent_low in

  (* Stop loss: 2% *)
  let stop_loss_mult = Const (0.98, Float) in

  {
    name = "Intraday_Momentum_10AM";
    buy_trigger =
      in_entry_window
      &&. momentum_positive
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. momentum_reversal;
    score = return_30 *. Const (100.0, Float);
    max_positions = 5;
    position_size = 0.20;
  }

(** Intraday_MR_10AM - Inverse of Momentum: Buy Morning Losers

    Tests the hypothesis: "morning losers bounce back through the day"
    (Mean reversion at 10 AM instead of momentum continuation)

    Entry (10:00-10:15 AM only):
    - 30-45 minutes after market open
    - Price < price 30 bars ago (negative momentum = oversold)

    Score:
    - Negative return over last 30 bars (bigger drop = higher priority)

    Exit:
    - End of day (force_exit_eod)
    - Stop loss: 2% below entry
    - Recovery target: price rises above 5-bar ago high

    NOTE: Use starting index of at least 50 (-i 50) for lag warmup. *)
let intraday_mr_10am =
  (* Entry window: 30-45 minutes after open (10:00-10:15 AM) *)
  let minutes_since = minutes_since_open TickTime in
  let in_entry_window =
    (minutes_since >=. Const (30.0, Float))
    &&. (minutes_since <=. Const (45.0, Float))
  in

  (* Mean reversion: price is LOWER than 30 bars ago (oversold) *)
  let price_30_ago = lag last 30 in
  let momentum_negative = last <. price_30_ago in

  (* Return calculation for scoring - more negative = higher score *)
  let return_30 = (last -. price_30_ago) /. price_30_ago in

  (* Recovery detection: price above recent high = take profit *)
  let recent_high = lag high 5 in
  let recovery_exit = last >. recent_high in

  (* Stop loss: 2% *)
  let stop_loss_mult = Const (0.98, Float) in

  {
    name = "Intraday_MR_10AM";
    buy_trigger =
      in_entry_window
      &&. momentum_negative
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. recovery_exit;
    (* Invert score: more negative return = higher priority (bigger dip = better opportunity) *)
    score = Const (0.0, Float) -. (return_30 *. Const (100.0, Float));
    max_positions = 5;
    position_size = 0.20;
  }

(** Intraday_MR_Anytime - Mean Reversion Without Time Restriction

    Same as Intraday_MR_10AM but can enter anytime during the day.
    Tests whether the "buy losers" signal exists throughout the day
    or is specific to the 10 AM window.

    Entry (anytime during market hours):
    - Price < price 30 bars ago (down over last 30 min)

    Score:
    - Bigger drop = higher priority

    Exit:
    - End of day
    - Stop loss: 2% below entry
    - Recovery: price rises above 5-bar ago high

    NOTE: Use starting index of at least 50 (-i 50) for lag warmup. *)
let intraday_mr_anytime =
  (* No time restriction - just need safe_to_enter *)

  (* Mean reversion: price is LOWER than 30 bars ago *)
  let price_30_ago = lag last 30 in
  let momentum_negative = last <. price_30_ago in

  (* Return calculation for scoring *)
  let return_30 = (last -. price_30_ago) /. price_30_ago in

  (* Recovery detection: price above recent high = take profit *)
  let recent_high = lag high 5 in
  let recovery_exit = last >. recent_high in

  (* Stop loss: 2% *)
  let stop_loss_mult = Const (0.98, Float) in

  {
    name = "Intraday_MR_Anytime";
    buy_trigger =
      momentum_negative
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. recovery_exit;
    score = Const (0.0, Float) -. (return_30 *. Const (100.0, Float));
    max_positions = 5;
    position_size = 0.20;
  }

(** Intraday_MR_Anytime_2 - Mean Reversion with SMA and Volume Spike

    Improved version of Anytime with filters:
    - Price at least 0.3% below SMA(30) (significant drop, not noise)
    - Volume > 2x lagged volume (volume spike confirms selling)
    - max_positions = 3, position_size = 0.33 (full capital utilization)

    Entry (anytime during market hours):
    - Price >= 0.3% below SMA(30)
    - Volume >= 2x volume from 20 bars ago

    Score:
    - Bigger drop below SMA = higher priority

    Exit:
    - End of day
    - Stop loss: 2% below entry
    - Recovery: price rises above 5-bar ago high

    NOTE: Use starting index of at least 50 (-i 50) for lag warmup. *)
let intraday_mr_anytime_2 =
  (* SMA(30) for smoother mean reversion signal *)
  let sma_30 = Gadt_fo.Constant.sma 30 () in

  (* Percentage below SMA *)
  let pct_below_sma = (sma_30 -. last) /. sma_30 in

  (* Mean reversion: price at least 0.3% below SMA *)
  let significantly_below_sma = pct_below_sma >. Const (0.003, Float) in

  (* Volume spike: current volume > 2x volume 20 bars ago *)
  let volume_spike = volume >. lag volume 20 *. Const (2.0, Float) in

  (* Recovery detection: price above recent high = take profit *)
  let recent_high = lag high 5 in
  let recovery_exit = last >. recent_high in

  (* Stop loss: 2% *)
  let stop_loss_mult = Const (0.98, Float) in

  {
    name = "Intraday_MR_Anytime_2";
    buy_trigger =
      significantly_below_sma
      &&. volume_spike
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. recovery_exit;
    (* Bigger drop below SMA = higher score *)
    score = pct_below_sma *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** Intraday_MR_5min - Mean Reversion with 5-Minute Minimum Hold

    Based on Intraday_MR_Anytime but with a 5-minute minimum hold time.
    This filters out noise trades and retains only significant moves.

    Rationale for 5-minute hold:
    - Execution: ~30s for order submission + fill + confirmation
    - Market impact: need time for our entry to not move exit price
    - Mean reversion: real reversions take time to develop
    - Reduces trade count dramatically while keeping high-conviction trades

    Entry (anytime during market hours):
    - Price < price 30 bars ago (down over last 30 min)

    Exit:
    - End of day (always)
    - Stop loss: 2% (gated by min hold - gives trade time to work)
    - Recovery: price > 5-bar ago high (gated by min hold)
    - OR held for 20+ minutes (max hold for intraday)

    NOTE: Use starting index of at least 50 (-i 50) for lag warmup. *)
let intraday_mr_5min =
  (* Minimum hold: 5 bars = 5 minutes on 1-min data *)
  let min_hold = Const (5, Int) in
  let max_hold = Const (60, Int) in (* 1 hour max *)

  (* Mean reversion: price is LOWER than 30 bars ago *)
  let price_30_ago = lag last 30 in
  let momentum_negative = last <. price_30_ago in

  (* Return calculation for scoring *)
  let return_30 = (last -. price_30_ago) /. price_30_ago in

  (* Recovery detection: price above recent high = take profit *)
  let recent_high = lag high 5 in
  let recovery_exit = last >. recent_high in

  (* Stop loss: 2% *)
  let stop_loss_mult = Const (0.98, Float) in

  (* Gate exits by minimum hold time *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, min_hold) in
  let gated_exits =
    past_min_hold &&. (
      recovery_exit
      ||. (last <. EntryPrice *. stop_loss_mult)
    )
  in

  {
    name = "Intraday_MR_5min";
    buy_trigger =
      momentum_negative
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    score = Const (0.0, Float) -. (return_30 *. Const (100.0, Float));
    max_positions = 5;
    position_size = 0.20;
  }

(** Intraday_MR_15min - Mean Reversion with 15-Minute Minimum Hold

    More conservative variant with 15-minute hold.
    Trades even less frequently, targeting only substantial reversions.

    This hold time better matches:
    - Typical intraday mean reversion half-life (15-60 min for large caps)
    - Realistic execution window for retail traders
    - Sufficient time for the trade thesis to play out

    Exit:
    - End of day (always)
    - Stop loss: 3% (wider to give trade room)
    - Profit target: 1.5% (gated by min hold)
    - Recovery: price > 5-bar ago high (gated by min hold)
    - Max hold: 2 hours

    NOTE: Use starting index of at least 50 (-i 50) for lag warmup. *)
let intraday_mr_15min =
  (* Minimum hold: 15 bars = 15 minutes *)
  let min_hold = Const (15, Int) in
  let max_hold = Const (120, Int) in (* 2 hours max *)

  (* Mean reversion: price is LOWER than 30 bars ago *)
  let price_30_ago = lag last 30 in
  let momentum_negative = last <. price_30_ago in

  (* Return calculation for scoring *)
  let return_30 = (last -. price_30_ago) /. price_30_ago in

  (* Recovery detection *)
  let recent_high = lag high 5 in
  let recovery_exit = last >. recent_high in

  (* Profit target: 1.5% *)
  let profit_target_mult = Const (1.015, Float) in
  let profit_target_hit = last >. EntryPrice *. profit_target_mult in

  (* Stop loss: 3% (wider for longer hold) *)
  let stop_loss_mult = Const (0.97, Float) in

  (* Gate exits by minimum hold time *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, min_hold) in
  let gated_exits =
    past_min_hold &&. (
      recovery_exit
      ||. profit_target_hit
      ||. (last <. EntryPrice *. stop_loss_mult)
    )
  in

  {
    name = "Intraday_MR_15min";
    buy_trigger =
      momentum_negative
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    score = Const (0.0, Float) -. (return_30 *. Const (100.0, Float));
    max_positions = 5;
    position_size = 0.20;
  }

(** Intraday_MR_Anytime_Opt - DEPRECATED, use ID_MR_AT_OPT_0 instead *)
let intraday_mr_anytime_opt =
  (* Keeping minimal version for backwards compatibility *)
  let sma = Gadt_fo.Constant.sma 30 () in
  let pct_below_sma = (sma -. last) /. sma in
  {
    name = "Intraday_MR_Anytime_Opt";
    buy_trigger = pct_below_sma >. Const (0.003, Float) &&. safe_to_enter ();
    sell_trigger = force_exit_eod () ||. (last <. EntryPrice *. Const (0.97, Float));
    score = pct_below_sma *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** ID_MR_AT_OPT_0 - Intraday Mean Reversion with MFI-based volume confirmation

    All key parameters exposed as optimization variables for ISRES tuning.
    Uses MFI (Money Flow Index) for volume confirmation instead of raw volume comparison.

    Variables (8 total):
    1. sma_period: [10, 200] - SMA lookback for mean calculation
    2. min_drop: [0.001, 0.02] - Minimum % below SMA to enter (0.1% to 2%)
    3. mfi_period: [10, 200] - MFI lookback period
    4. mfi_entry: [20, 45] - MFI below this to enter (oversold + volume)
    5. mfi_exit: [50, 75] - MFI above this to exit (recovered)
    6. stop_loss: [0.01, 0.05] - Stop loss percentage (1% to 5%)
    7. profit_target: [0.005, 0.03] - Profit target percentage (0.5% to 3%)
    8. max_hold: [30, 240] - Maximum hold time in bars

    Entry:
    - Price >= min_drop below SMA(sma_period)
    - MFI < mfi_entry (oversold with volume confirmation)

    Exit:
    - EOD, stop loss, profit target, MFI > mfi_exit, or max hold

    NOTE: Use starting index of at least 250 (-i 250) for indicator warmup. *)
let id_mr_at_opt_0 =
  (* Variable 1: SMA period for mean calculation *)
  let sma_period_var = Gadt_fo.var ~lower:10.0 ~upper:200.0 Type.Int in

  (* Variable 2: Minimum drop below SMA to enter *)
  let min_drop_var = Gadt_fo.var ~lower:0.001 ~upper:0.02 Type.Float in

  (* Variable 3: MFI period *)
  let mfi_period_var = Gadt_fo.var ~lower:10.0 ~upper:200.0 Type.Int in

  (* Variable 4: MFI entry threshold (below = oversold) *)
  let mfi_entry_var = Gadt_fo.var ~lower:20.0 ~upper:45.0 Type.Float in

  (* Variable 5: MFI exit threshold (above = recovered) *)
  let mfi_exit_var = Gadt_fo.var ~lower:50.0 ~upper:75.0 Type.Float in

  (* Variable 6: Stop loss percentage *)
  let stop_loss_var = Gadt_fo.var ~lower:0.01 ~upper:0.05 Type.Float in

  (* Variable 7: Profit target percentage *)
  let profit_target_var = Gadt_fo.var ~lower:0.005 ~upper:0.03 Type.Float in

  (* Variable 8: Maximum hold time *)
  let max_hold_var = Gadt_fo.var ~lower:30.0 ~upper:240.0 Type.Int in

  (* SMA indicator with variable period *)
  let sma =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", Tacaml.Indicator.Raw.sma), sma_period_var) ))
  in

  (* MFI indicator with variable period *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period_var) ))
  in

  (* Percentage below SMA *)
  let pct_below_sma = (sma -. last) /. sma in

  (* Entry conditions *)
  let below_sma_threshold = pct_below_sma >. min_drop_var in
  let mfi_oversold = mfi <. mfi_entry_var in

  (* Exit conditions *)
  let mfi_recovered = mfi >. mfi_exit_var in
  let stop_loss_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_target_mult = Const (1.0, Float) +. profit_target_var in

  {
    name = "ID_MR_AT_OPT_0";
    buy_trigger =
      below_sma_threshold
      &&. mfi_oversold
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. mfi_recovered
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold_var);
    (* Bigger drop below SMA = higher score *)
    score = pct_below_sma *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** ID_MR_AT_0 - ISRES-trained Mean Reversion with MFI (locked)

    Trained on ID_MR_AT_OPT_0, 4000 iterations with 2 bps/side spread cost.
    SURVIVES SLIPPAGE - first strategy to show edge after execution costs.

    Objective: $138,884.45 (38.9% return)
    372 trades, 61.56% win rate, Sharpe 0.196, profit factor 1.648
    Expectancy: $112.09/trade, p=0.0001

    Key insight: ~2% drop below 139-bar SMA filters out marginal trades.
    Trade count reduced from 6000+ to ~370, preserving quality signals.

    NOTE: Use starting index of at least 250 (-i 250) for indicator warmup. *)
let id_mr_at_0 =
  let sma =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", Tacaml.Indicator.Raw.sma), Const (139, Int)) ))
  in
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (81, Int)) ))
  in
  let pct_below_sma = (sma -. last) /. sma in
  let below_sma_threshold = pct_below_sma >. Const (0.019044, Float) in
  let mfi_oversold = mfi <. Const (36.661, Float) in
  let mfi_recovered = mfi >. Const (65.796, Float) in
  let stop_loss_mult = Const (0.9639, Float) in (* 1 - 0.036117 *)
  let profit_target_mult = Const (1.0245, Float) in (* 1 + 0.024548 *)
  {
    name = "ID_MR_AT_0";
    buy_trigger =
      below_sma_threshold
      &&. mfi_oversold
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. mfi_recovered
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (178, Int));
    score = pct_below_sma *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** ID_MR_AT_1 - ISRES-trained Mean Reversion with MFI (locked)

    Trained on ID_MR_AT_OPT_0, 4000 iterations with 2 bps/side spread cost.
    BEST of 3 runs - highest absolute return.

    Objective: $143,277.26 (43.3% return)
    368 trades, 60.33% win rate, Sharpe 0.195, profit factor 1.645
    Expectancy: $125.52/trade, p=0.0001

    Slightly wider MFI exit (74.4) allows more profit capture.

    NOTE: Use starting index of at least 250 (-i 250) for indicator warmup. *)
let id_mr_at_1 =
  let sma =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", Tacaml.Indicator.Raw.sma), Const (142, Int)) ))
  in
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (93, Int)) ))
  in
  let pct_below_sma = (sma -. last) /. sma in
  let below_sma_threshold = pct_below_sma >. Const (0.019485, Float) in
  let mfi_oversold = mfi <. Const (38.356, Float) in
  let mfi_recovered = mfi >. Const (74.426, Float) in
  let stop_loss_mult = Const (0.9656, Float) in (* 1 - 0.034398 *)
  let profit_target_mult = Const (1.0286, Float) in (* 1 + 0.028618 *)
  {
    name = "ID_MR_AT_1";
    buy_trigger =
      below_sma_threshold
      &&. mfi_oversold
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. mfi_recovered
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (163, Int));
    score = pct_below_sma *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** ID_MR_AT_2 - ISRES-trained Mean Reversion with MFI (locked)

    Trained on ID_MR_AT_OPT_0, 4000 iterations with 2 bps/side spread cost.
    Best profit factor (1.668) and win rate (62.22%).

    Objective: $141,343.56 (41.3% return)
    405 trades, 62.22% win rate, Sharpe 0.196, profit factor 1.668
    Expectancy: $109.91/trade, p=0.0000

    Longest SMA period (155) and max_hold (195) - most patient variant.

    NOTE: Use starting index of at least 250 (-i 250) for indicator warmup. *)
let id_mr_at_2 =
  let sma =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", Tacaml.Indicator.Raw.sma), Const (155, Int)) ))
  in
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (86, Int)) ))
  in
  let pct_below_sma = (sma -. last) /. sma in
  let below_sma_threshold = pct_below_sma >. Const (0.018192, Float) in
  let mfi_oversold = mfi <. Const (35.991, Float) in
  let mfi_recovered = mfi >. Const (62.279, Float) in
  let stop_loss_mult = Const (0.9596, Float) in (* 1 - 0.040387 *)
  let profit_target_mult = Const (1.0232, Float) in (* 1 + 0.023224 *)
  {
    name = "ID_MR_AT_2";
    buy_trigger =
      below_sma_threshold
      &&. mfi_oversold
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. mfi_recovered
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (195, Int));
    score = pct_below_sma *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** IDMR_A_ISRES_0 - ISRES-trained Intraday Mean Reversion

    Trained on 6 months data, 4k iterations.
    Objective: 138,040.65
    6969 trades, 64.04% win rate, $7.46 expectancy, p=0.0001

    NOTE: Shows statistically significant edge WITHOUT slippage, but
    ~$7/trade edge is destroyed by ~$6-13/trade execution costs.
    At 2 bps slippage: -16% avg return. Not viable for retail execution.

    Locked parameters:
    - SMA period: 15
    - Min drop: 0.301% below SMA
    - Volume mult: 1.327x
    - Stop loss: 4.13%
    - Profit target: 0.80%
    - Max hold: 33 bars *)
let idmr_a_isres_0 =
  (* SMA(15) for mean calculation *)
  let sma = Gadt_fo.Constant.sma 15 () in

  (* Percentage below SMA *)
  let pct_below_sma = (sma -. last) /. sma in

  (* Entry: price at least 0.301% below SMA *)
  let below_sma_threshold = pct_below_sma >. Const (0.00301, Float) in

  (* Volume spike: 1.327x lagged volume *)
  let volume_spike = volume >. lag volume 20 *. Const (1.327, Float) in

  (* Stop loss: 4.13% and profit target: 0.80% *)
  let stop_loss_mult = Const (0.9587, Float) in
  let profit_target_mult = Const (1.008, Float) in

  (* Recovery exit: price above recent high *)
  let recent_high = lag high 5 in
  let recovery_exit = last >. recent_high in

  (* Max hold: 33 bars *)
  let max_hold = Const (33, Int) in

  {
    name = "IDMR_A_ISRES_0";
    buy_trigger =
      below_sma_threshold
      &&. volume_spike
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. recovery_exit
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    score = pct_below_sma *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** IDMR_A_ISRES_1 - ISRES-trained with 2x trade penalty

    Trained on 6 months data, 4k iterations, 2x per-trade penalty.
    Objective: 121,762.97
    6737 trades, 64.26% win rate, $7.23 expectancy, p=0.0001

    NOTE: Shows statistically significant edge WITHOUT slippage, but
    ~$7/trade edge is destroyed by ~$6-13/trade execution costs.
    At 2 bps slippage: -16% avg return. Not viable for retail execution.

    Locked parameters:
    - SMA period: 18
    - Min drop: 0.332% below SMA
    - Volume mult: 1.339x
    - Stop loss: 2.57%
    - Profit target: 1.23%
    - Max hold: 163 bars *)
let idmr_a_isres_1 =
  (* SMA(18) for mean calculation *)
  let sma = Gadt_fo.Constant.sma 18 () in

  (* Percentage below SMA *)
  let pct_below_sma = (sma -. last) /. sma in

  (* Entry: price at least 0.332% below SMA *)
  let below_sma_threshold = pct_below_sma >. Const (0.00332, Float) in

  (* Volume spike: 1.339x lagged volume *)
  let volume_spike = volume >. lag volume 20 *. Const (1.339, Float) in

  (* Stop loss: 2.57% and profit target: 1.23% *)
  let stop_loss_mult = Const (0.9743, Float) in
  let profit_target_mult = Const (1.0123, Float) in

  (* Recovery exit: price above recent high *)
  let recent_high = lag high 5 in
  let recovery_exit = last >. recent_high in

  (* Max hold: 163 bars *)
  let max_hold = Const (163, Int) in

  {
    name = "IDMR_A_ISRES_1";
    buy_trigger =
      below_sma_threshold
      &&. volume_spike
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. recovery_exit
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    score = pct_below_sma *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** NB_V3_0 - Nature Boy V3 ISRES-trained (locked)

    Trained on q3q4-2025, 4000 iterations with 2 bps/side spread cost.

    Objective: $120,147.71 (20.1% return)
    215 trades, 65.58% win rate, Sharpe 0.230, profit factor 1.767
    Expectancy: $97.97/trade, p=0.0004

    NOTE: Use starting index of at least 400 (-i 400) for indicator warmup. *)
let nb_v3_0 =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (186, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (6, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (167, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (323, Int),
               Const (2.864, Float),
               Const (2.864, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (326, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (71, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (61.99, Float))
        ||. (last >. EntryPrice *. Const (1.063, Float)))
  in
  {
    name = "NB_V3_0";
    buy_trigger =
      mfi <. Const (47.93, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.509, Float))
      &&. (natr_hi <. Const (4.174, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.965, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (190, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** NB_V3_1 - Nature Boy V3 ISRES-trained (locked)

    Trained on q3q4-2025, 4000 iterations with 2 bps/side spread cost.
    BEST of 3 runs - highest Sharpe (0.288) and profit factor (2.071).

    Objective: $121,488.53 (21.5% return)
    188 trades, 65.43% win rate, Sharpe 0.288, profit factor 2.071
    Expectancy: $118.61/trade, p=0.0000

    Shorter MFI period (70) and tighter NATR_hi (58) than other variants.

    NOTE: Use starting index of at least 400 (-i 400) for indicator warmup. *)
let nb_v3_1 =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (70, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (9, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (58, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (303, Int),
               Const (2.750, Float),
               Const (2.750, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (235, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (57, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (57.89, Float))
        ||. (last >. EntryPrice *. Const (1.047, Float)))
  in
  {
    name = "NB_V3_1";
    buy_trigger =
      mfi <. Const (47.40, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.474, Float))
      &&. (natr_hi <. Const (3.975, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.951, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (382, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** NB_V3_2 - Nature Boy V3 ISRES-trained (locked)

    Trained on q3q4-2025, 4000 iterations with 2 bps/side spread cost.

    Objective: $118,931.75 (18.9% return)
    193 trades, 64.25% win rate, Sharpe 0.239, profit factor 1.777
    Expectancy: $102.36/trade, p=0.0005

    Tightest NATR_hi threshold (1.41) - most selective for low volatility.

    NOTE: Use starting index of at least 400 (-i 400) for indicator warmup. *)
let nb_v3_2 =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (170, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (5, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (127, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (348, Int),
               Const (2.426, Float),
               Const (2.426, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (323, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (10, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (67.99, Float))
        ||. (last >. EntryPrice *. Const (1.070, Float)))
  in
  {
    name = "NB_V3_2";
    buy_trigger =
      mfi <. Const (45.18, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.553, Float))
      &&. (natr_hi <. Const (1.411, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.969, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (233, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(* Export all strategies *)
let all_strategies =
  [
    nature_boy_v3_opt;
    mr0_opt;
    mrnb_3_a;
    mrnb_3_b;
    mrnb_3_c;
    mr_basic;
    intraday_momentum_10am;
    intraday_mr_10am;
    intraday_mr_anytime;
    intraday_mr_anytime_2;
    intraday_mr_anytime_opt;
    id_mr_at_opt_0;
    id_mr_at_0;
    id_mr_at_1;
    id_mr_at_2;
    nb_v3_0;
    nb_v3_1;
    nb_v3_2;
    idmr_a_isres_0;
    idmr_a_isres_1;
    intraday_mr_5min;
    intraday_mr_15min;
  ]
