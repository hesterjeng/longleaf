(** Tech Momentum Strategies

    Momentum and trend-following strategies designed for the tech-heavy
    universe during 2023-2025 bull markets. These strategies are the
    OPPOSITE of mean reversion - they buy strength and ride trends.

    Key differences from mean reversion:
    - Buy when indicators show STRENGTH, not weakness
    - High MFI (>50) = buying pressure = good
    - High ADX = strong trend = good
    - Price ABOVE moving averages = good
    - Ride winners, cut losers quickly

    Universe: tech (17 stocks)
    AAPL, MSFT, NVDA, AMZN, GOOGL, META, NFLX, TSLA, AMD, CRM,
    ADBE, INTU, ORCL, CSCO, INTC, QCOM, TXN

    Usage:
      longleaf Backtest Tech_Momentum_Opt data/q3q4-2025-1min.json -u tech -i 200
      longleaf Battery Tech_Momentum_Opt quarterly -u tech
*)

open Gadt
open Gadt_strategy
module Data = Longleaf_bars.Data
module I = Tacaml.Indicator.Raw

(** Tech_Momentum_Opt - Optimizable Tech Momentum Strategy

    HYPOTHESIS: In a bull market for tech, stocks that show strong
    momentum (high ROC, high ADX, price above SMA) tend to continue.
    We want to catch these moves early and ride them.

    Variables to optimize (10 total):

    Trend Detection:
    1. adx_period: [14, 200] - ADX lookback (~14 min to 3.3 hours)
    2. adx_threshold: [15, 45] - Minimum ADX to confirm trend
    3. sma_period: [20, 250] - SMA for trend direction (~20 min to 4 hours)

    Momentum Confirmation:
    4. roc_period: [5, 60] - Rate of change lookback
    5. roc_threshold: [0.05, 3.0] - Minimum ROC % for entry
    6. mfi_period: [14, 150] - MFI for volume confirmation
    7. mfi_threshold: [45, 80] - Minimum MFI (buying pressure)

    Risk Management:
    8. stop_loss: [0.5%, 8%] - Stop loss range
    9. profit_target: [1%, 15%] - Profit target range
    10. max_hold: [15, 350] - Max hold time (~15 min to 5.8 hours)

    Entry:
    - ADX > threshold (strong trend)
    - Price > SMA (uptrend)
    - ROC > threshold (positive momentum)
    - MFI > threshold (volume-confirmed buying)

    Exit:
    - EOD (no overnight)
    - Stop loss hit
    - Profit target hit
    - ADX drops below 15 (trend weakening)
    - Max hold time reached

    NOTE: Use starting index of at least 200 (-i 200) for indicator warmup.
    NOTE: Use with -u tech for the tech universe. *)
let tech_momentum_opt : Gadt_strategy.t =
  (* Trend Detection Variables - for 1-min bars, 200 = ~3.3 hours *)
  let adx_period_var = Gadt_fo.var ~lower:14.0 ~upper:200.0 Type.Int in
  let adx_threshold_var = Gadt_fo.var ~lower:15.0 ~upper:45.0 Type.Float in
  let sma_period_var = Gadt_fo.var ~lower:20.0 ~upper:250.0 Type.Int in

  (* Momentum Confirmation Variables *)
  let roc_period_var = Gadt_fo.var ~lower:5.0 ~upper:60.0 Type.Int in
  let roc_threshold_var = Gadt_fo.var ~lower:0.05 ~upper:3.0 Type.Float in
  let mfi_period_var = Gadt_fo.var ~lower:14.0 ~upper:150.0 Type.Int in
  let mfi_threshold_var = Gadt_fo.var ~lower:45.0 ~upper:80.0 Type.Float in

  (* Risk Management Variables *)
  let stop_loss_var = Gadt_fo.var ~lower:0.005 ~upper:0.08 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.01 ~upper:0.15 Type.Float in
  let max_hold_var = Gadt_fo.var ~lower:15.0 ~upper:350.0 Type.Int in

  (* Indicators *)
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", I.adx), adx_period_var) ))
  in

  let sma =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), sma_period_var) ))
  in

  let roc =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.roc", I.roc), roc_period_var) ))
  in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), mfi_period_var) ))
  in

  (* Entry Conditions - ALL must be true *)
  let strong_trend = adx >. adx_threshold_var in
  let uptrend = last >. sma in
  let positive_momentum = roc >. roc_threshold_var in
  let volume_confirmed = mfi >. mfi_threshold_var in

  (* Exit Conditions *)
  let stop_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_mult = Const (1.0, Float) +. profit_target_var in
  let trend_weakening = adx <. Const (15.0, Float) in

  {
    name = "Tech_Momentum_Opt";
    buy_trigger =
      strong_trend &&. uptrend &&. positive_momentum &&. volume_confirmed
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. trend_weakening
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold_var);
    (* Score: Higher ADX * ROC = stronger signal *)
    score = adx *. roc;
    max_positions = 1;
    position_size = 1.0;
  }

(** Tech_Breakout_Opt - Opening Range Breakout for Tech

    HYPOTHESIS: Tech stocks that break out of their opening range
    with volume tend to continue in that direction. This is a
    classic day trading pattern.

    Variables to optimize (8 total):

    Opening Range:
    1. range_minutes: [10, 90] - Minutes to establish opening range
    2. breakout_atr_mult: [0.2, 3.0] - ATR multiple above range high

    Trend Filter:
    3. adx_period: [10, 120] - ADX for trend context (up to 2 hours)
    4. adx_min: [10, 40] - Minimum ADX (not choppy)

    Volume Confirmation:
    5. mfi_period: [10, 100] - MFI period (up to ~1.7 hours)
    6. mfi_min: [40, 80] - Minimum MFI (buying pressure)

    Risk Management:
    7. stop_atr_mult: [0.3, 3.0] - ATR multiple for stop
    8. profit_atr_mult: [0.5, 6.0] - ATR multiple for target

    Entry:
    - Price breaks above opening high + ATR buffer
    - After opening range established (range_minutes)
    - ADX > min (trending, not choppy)
    - MFI > min (volume confirmed)

    Exit:
    - EOD
    - Stop: entry - ATR * mult
    - Target: entry + ATR * mult
    - Max hold: 4 hours (240 bars)

    NOTE: Use -i 100 for warmup. Use -u tech. *)
let tech_breakout_opt : Gadt_strategy.t =
  (* Opening Range Variables - for 1-min bars *)
  let range_minutes_var = Gadt_fo.var ~lower:10.0 ~upper:90.0 Type.Float in
  let breakout_atr_mult_var = Gadt_fo.var ~lower:0.2 ~upper:3.0 Type.Float in

  (* Trend Filter Variables *)
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:120.0 Type.Int in
  let adx_min_var = Gadt_fo.var ~lower:10.0 ~upper:40.0 Type.Float in

  (* Volume Variables *)
  let mfi_period_var = Gadt_fo.var ~lower:10.0 ~upper:100.0 Type.Int in
  let mfi_min_var = Gadt_fo.var ~lower:40.0 ~upper:80.0 Type.Float in

  (* Risk Variables *)
  let stop_atr_mult_var = Gadt_fo.var ~lower:0.3 ~upper:3.0 Type.Float in
  let profit_atr_mult_var = Gadt_fo.var ~lower:0.5 ~upper:6.0 Type.Float in

  (* Indicators *)
  let atr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.atr", I.atr), Const (14, Int)) ))
  in

  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", I.adx), adx_period_var) ))
  in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), mfi_period_var) ))
  in

  (* Entry Conditions *)
  let past_opening_range = MinutesSinceOpen >. range_minutes_var in
  let breakout_level = DayHigh +. atr *. breakout_atr_mult_var in
  let price_breakout = last >. breakout_level in
  let trend_ok = adx >. adx_min_var in
  let volume_ok = mfi >. mfi_min_var in

  (* Dynamic stops based on ATR *)
  let stop_level = EntryPrice -. atr *. stop_atr_mult_var in
  let target_level = EntryPrice +. atr *. profit_atr_mult_var in

  {
    name = "Tech_Breakout_Opt";
    buy_trigger =
      past_opening_range &&. price_breakout &&. trend_ok &&. volume_ok
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. stop_level)
      ||. (last >. target_level)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (240, Int));
    (* Score: How far above breakout level *)
    score = last -. breakout_level;
    max_positions = 1;
    position_size = 1.0;
  }

(** Tech_EMA_Cross_Opt - EMA Crossover with Momentum Filter

    HYPOTHESIS: When fast EMA crosses above slow EMA with strong
    momentum confirmation, the trend continues. Classic trend-following.

    Variables to optimize (8 total):

    EMA Crossover:
    1. fast_ema: [3, 60] - Fast EMA period (3-60 min)
    2. slow_ema: [20, 200] - Slow EMA period (20 min to 3.3 hours)

    Momentum Filter:
    3. roc_period: [3, 45] - ROC lookback
    4. roc_min: [0.0, 2.0] - Minimum ROC for entry

    Trend Strength:
    5. adx_period: [10, 100] - ADX period (up to ~1.7 hours)
    6. adx_min: [15, 45] - Minimum ADX

    Risk Management:
    7. stop_loss: [0.5%, 6%] - Stop loss range
    8. profit_target: [1%, 12%] - Profit target range

    Entry:
    - Fast EMA crosses above slow EMA
    - ROC > min (momentum confirmation)
    - ADX > min (trending market)

    Exit:
    - EOD
    - Fast EMA crosses below slow EMA (trend reversal)
    - Stop loss or profit target
    - ADX drops below 15
    - Max hold: 4 hours

    NOTE: Use -i 150 for warmup. Use -u tech. *)
let tech_ema_cross_opt : Gadt_strategy.t =
  (* EMA Variables - for 1-min bars *)
  let fast_ema_var = Gadt_fo.var ~lower:3.0 ~upper:60.0 Type.Int in
  let slow_ema_var = Gadt_fo.var ~lower:20.0 ~upper:200.0 Type.Int in

  (* Momentum Variables *)
  let roc_period_var = Gadt_fo.var ~lower:3.0 ~upper:45.0 Type.Int in
  let roc_min_var = Gadt_fo.var ~lower:0.0 ~upper:2.0 Type.Float in

  (* Trend Variables *)
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:100.0 Type.Int in
  let adx_min_var = Gadt_fo.var ~lower:15.0 ~upper:45.0 Type.Float in

  (* Risk Variables *)
  let stop_loss_var = Gadt_fo.var ~lower:0.005 ~upper:0.06 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.01 ~upper:0.12 Type.Float in

  (* Indicators *)
  let fast_ema =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.ema", I.ema), fast_ema_var) ))
  in

  let slow_ema =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.ema", I.ema), slow_ema_var) ))
  in

  let roc =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.roc", I.roc), roc_period_var) ))
  in

  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", I.adx), adx_period_var) ))
  in

  (* Entry Conditions *)
  let ema_cross_up = cross_up fast_ema slow_ema in
  let momentum_ok = roc >. roc_min_var in
  let trend_ok = adx >. adx_min_var in

  (* Exit Conditions *)
  let ema_cross_down = cross_down fast_ema slow_ema in
  let stop_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_mult = Const (1.0, Float) +. profit_target_var in

  {
    name = "Tech_EMA_Cross_Opt";
    buy_trigger =
      ema_cross_up &&. momentum_ok &&. trend_ok &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. ema_cross_down
      ||. (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. (adx <. Const (15.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (240, Int));
    (* Score: ADX * ROC - stronger trend and momentum = higher priority *)
    score = adx *. roc;
    max_positions = 1;
    position_size = 1.0;
  }

(** Tech_Gap_Go_Opt - Gap and Go Strategy

    HYPOTHESIS: Tech stocks that gap up at open and show continuation
    in the first hour tend to keep going. Classic gap-and-go pattern.

    Variables to optimize (7 total):

    Gap Filter:
    1. min_gap: [0.2, 4.0] - Minimum gap up % to qualify
    2. max_gap: [2.0, 15.0] - Maximum gap (avoid exhaustion)

    Entry Timing:
    3. entry_start: [5, 60] - Minutes after open to start looking
    4. entry_end: [30, 180] - Minutes after open to stop looking (up to 3 hours)

    Continuation Filter:
    5. above_open_pct: [-0.5, 2.0] - Must be this % above open price

    Risk Management:
    6. stop_loss: [0.5%, 5%] - Stop loss range
    7. profit_target: [1%, 10%] - Profit target range

    Entry:
    - Stock gapped up between min_gap and max_gap
    - Within entry window (entry_start to entry_end minutes)
    - Price still above open by above_open_pct
    - Price making new highs (continuation)

    Exit:
    - EOD
    - Stop loss or profit target
    - Price drops below open (gap fill = failed)
    - Max hold: 5 hours

    NOTE: Use -i 50 for warmup. Use -u tech. *)
let tech_gap_go_opt : Gadt_strategy.t =
  (* Gap Variables *)
  let min_gap_var = Gadt_fo.var ~lower:0.2 ~upper:4.0 Type.Float in
  let max_gap_var = Gadt_fo.var ~lower:2.0 ~upper:15.0 Type.Float in

  (* Timing Variables - for 1-min bars *)
  let entry_start_var = Gadt_fo.var ~lower:5.0 ~upper:60.0 Type.Float in
  let entry_end_var = Gadt_fo.var ~lower:30.0 ~upper:180.0 Type.Float in

  (* Continuation Variable *)
  let above_open_pct_var = Gadt_fo.var ~lower:(-0.5) ~upper:2.0 Type.Float in

  (* Risk Variables *)
  let stop_loss_var = Gadt_fo.var ~lower:0.005 ~upper:0.05 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.01 ~upper:0.10 Type.Float in

  (* Entry Conditions *)
  let gap_ok = (GapPct >. min_gap_var) &&. (GapPct <. max_gap_var) in
  let in_window =
    (MinutesSinceOpen >. entry_start_var) &&. (MinutesSinceOpen <. entry_end_var)
  in
  let above_open = DayChangePct >. above_open_pct_var in
  let making_highs = last >. lag high 5 in  (* New 5-bar high *)

  (* Exit Conditions *)
  let gap_fill = last <. DayOpen in  (* Dropped below open = failed *)
  let stop_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_mult = Const (1.0, Float) +. profit_target_var in

  {
    name = "Tech_Gap_Go_Opt";
    buy_trigger =
      gap_ok &&. in_window &&. above_open &&. making_highs &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. gap_fill
      ||. (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (300, Int));
    (* Score: Bigger gap + higher above open = better *)
    score = GapPct +. DayChangePct;
    max_positions = 1;
    position_size = 1.0;
  }

(** Tech_Gap_Go_A - ISRES-trained Gap and Go (Q3Q4 2025)

    Locked parameters from optimization:
    - min_gap: 0.389%
    - max_gap: 2.689%
    - entry_start: 17.75 minutes
    - entry_end: 66.36 minutes
    - above_open_pct: 1.595%
    - stop_loss: 4.31%
    - profit_target: 4.02%

    Training results (Q3Q4 2025):
    - Final Cash: $143,810
    - Trades: 130 (W: 42, L: 88)
    - Win Rate: 32.31%
    - Profit Factor: 2.626
    - Sharpe: 0.223
    - P-value: 0.58%

    Battery (quarterly_2023_2025, tech universe):
    - Consistency: 66.7%
    - Avg Sharpe: 0.013
    - Worst Drawdown: 28.87% (Q3Q4 2024) *)
let tech_gap_go_a : Gadt_strategy.t =
  (* Locked parameters *)
  let min_gap = Const (0.389403, Float) in
  let max_gap = Const (2.688776, Float) in
  let entry_start = Const (17.754513, Float) in
  let entry_end = Const (66.362601, Float) in
  let above_open_pct = Const (1.595218, Float) in
  let stop_loss = Const (0.043129, Float) in
  let profit_target = Const (0.040237, Float) in

  (* Entry Conditions *)
  let gap_ok = (GapPct >. min_gap) &&. (GapPct <. max_gap) in
  let in_window =
    (MinutesSinceOpen >. entry_start) &&. (MinutesSinceOpen <. entry_end)
  in
  let above_open = DayChangePct >. above_open_pct in
  let making_highs = last >. lag high 5 in

  (* Exit Conditions *)
  let gap_fill = last <. DayOpen in
  let stop_mult = Const (1.0, Float) -. stop_loss in
  let profit_mult = Const (1.0, Float) +. profit_target in

  {
    name = "Tech_Gap_Go_A";
    buy_trigger =
      gap_ok &&. in_window &&. above_open &&. making_highs &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. gap_fill
      ||. (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (300, Int));
    score = GapPct +. DayChangePct;
    max_positions = 1;
    position_size = 1.0;
  }

(** Tech_Gap_Go_B - Gap and Go with tighter risk management

    Same entry logic as A, but with:
    - 2 positions at 50% each (diversification)
    - Tighter stop loss: 2.5% (vs 4.31%)
    - Same profit target: 4.02%

    Battery (quarterly_2023_2025, tech universe):
    - Avg Sharpe: -0.008
    - Avg Return: 0.33%
    - Worst Drawdown: 19.27%
    - Consistency: 66.7%
    VERDICT: Tighter stop hurt returns more than helped risk. *)
let tech_gap_go_b : Gadt_strategy.t =
  (* Locked parameters - same entry as A *)
  let min_gap = Const (0.389403, Float) in
  let max_gap = Const (2.688776, Float) in
  let entry_start = Const (17.754513, Float) in
  let entry_end = Const (66.362601, Float) in
  let above_open_pct = Const (1.595218, Float) in
  (* Tighter stop loss *)
  let stop_loss = Const (0.025, Float) in
  let profit_target = Const (0.040237, Float) in

  (* Entry Conditions *)
  let gap_ok = (GapPct >. min_gap) &&. (GapPct <. max_gap) in
  let in_window =
    (MinutesSinceOpen >. entry_start) &&. (MinutesSinceOpen <. entry_end)
  in
  let above_open = DayChangePct >. above_open_pct in
  let making_highs = last >. lag high 5 in

  (* Exit Conditions *)
  let gap_fill = last <. DayOpen in
  let stop_mult = Const (1.0, Float) -. stop_loss in
  let profit_mult = Const (1.0, Float) +. profit_target in

  {
    name = "Tech_Gap_Go_B";
    buy_trigger =
      gap_ok &&. in_window &&. above_open &&. making_highs &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. gap_fill
      ||. (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (300, Int));
    score = GapPct +. DayChangePct;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Gap_Go_C - Gap and Go with long-term trend filter

    Same entry logic as A, but with:
    - 2 positions at 50% each
    - Original stop loss: 4.31%
    - Long SMA trend filter (1000 bars = ~2.5 trading days)
    - Only enter if price > SMA (uptrend)

    Battery (quarterly_2023_2025, tech universe):
    - Avg Sharpe: -0.016
    - Avg Return: 0.21%
    - Worst Drawdown: 19.27%
    - Consistency: 66.7%
    VERDICT: Trend filter didn't help - same drawdown as B.

    NOTE: Requires -i 1100 for warmup due to long SMA *)
let tech_gap_go_c : Gadt_strategy.t =
  (* Locked parameters - same entry as A *)
  let min_gap = Const (0.389403, Float) in
  let max_gap = Const (2.688776, Float) in
  let entry_start = Const (17.754513, Float) in
  let entry_end = Const (66.362601, Float) in
  let above_open_pct = Const (1.595218, Float) in
  let stop_loss = Const (0.043129, Float) in
  let profit_target = Const (0.040237, Float) in

  (* Long-term trend filter - 1000 bars = ~2.5 trading days *)
  let sma_long =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1000, Int)) ))
  in
  let in_uptrend = last >. sma_long in

  (* Entry Conditions *)
  let gap_ok = (GapPct >. min_gap) &&. (GapPct <. max_gap) in
  let in_window =
    (MinutesSinceOpen >. entry_start) &&. (MinutesSinceOpen <. entry_end)
  in
  let above_open = DayChangePct >. above_open_pct in
  let making_highs = last >. lag high 5 in

  (* Exit Conditions *)
  let gap_fill = last <. DayOpen in
  let stop_mult = Const (1.0, Float) -. stop_loss in
  let profit_mult = Const (1.0, Float) +. profit_target in

  {
    name = "Tech_Gap_Go_C";
    buy_trigger =
      in_uptrend &&. gap_ok &&. in_window &&. above_open &&. making_highs
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. gap_fill
      ||. (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (300, Int));
    score = GapPct +. DayChangePct;
    max_positions = 2;
    position_size = 0.5;
  }

(* ============================================================
   SIMPLE LONG-PERIOD STRATEGIES
   Testing the hypothesis that long-period indicators on 1-min bars
   can capture daily-style signals with better execution.
   ============================================================ *)

(** Tech_LongMA_MR - Simple Long-Period Mean Reversion (Multi-Day Hold)

    HYPOTHESIS: When price drops significantly below a long-term MA,
    it tends to revert back. Use 1-min bars with multi-day MA for
    daily-style signal with minute-level execution.

    Very simple - only 2 real parameters:
    - SMA period: 3900 bars (~2 weeks)
    - Dip threshold: 2% below SMA to enter

    Entry: price < SMA * 0.98 (2% dip)
    Exit: price > SMA (recovered) or stop or max hold (~4 trading days)

    NO EOD EXIT - holds overnight for multi-day mean reversion.

    Battery (quarterly_2023_2025, tech universe, -i 4000):
    - Avg Sharpe: 0.021
    - Avg Return: 2.44%
    - Worst Drawdown: 30.84%
    - Consistency: 16.7% (1/6 periods positive)
    - Q1Q2 2023: +46% (outlier - post-bear recovery)
    - All other periods: negative
    VERDICT: One lucky period carried average. Does not generalize.

    NOTE: Requires -i 4000 for warmup *)
let tech_long_ma_mr : Gadt_strategy.t =
  (* Fixed long-period SMA - 3900 bars = ~2 weeks *)
  let sma_long =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (3900, Int)) ))
  in

  (* Entry: price 2% below SMA *)
  let dip_threshold = Const (0.98, Float) in
  let oversold = last <. sma_long *. dip_threshold in

  (* Exit: recovered to SMA or 5% stop or max hold 1500 bars (~4 days) *)
  let recovered = last >. sma_long in
  let stop_mult = Const (0.95, Float) in

  {
    name = "Tech_LongMA_MR";
    buy_trigger = oversold;
    sell_trigger =
      recovered
      ||. (last <. EntryPrice *. stop_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1500, Int));
    score = sma_long -. last;  (* Bigger dip = higher priority *)
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_LongMA_Trend - Simple Long-Period Trend Follower (Multi-Day Hold)

    HYPOTHESIS: When price is above a long-term MA, it tends to
    continue upward. Ride the trend with minute-level entry.

    Very simple - only 2 real parameters:
    - SMA period: 3900 bars (~2 weeks)
    - Must be X% above SMA to confirm trend

    Entry: price > SMA * 1.01 (1% above)
    Exit: price < SMA (trend broken) or stop or max hold (~4 trading days)

    NO EOD EXIT - holds overnight for multi-day trend following.

    Battery (quarterly_2023_2025, tech universe, -i 4000):
    - Avg Sharpe: -0.010
    - Avg Return: -1.07%
    - Worst Drawdown: 24.84%
    - Avg Invested: 94.8%
    - Consistency: 50.0%
    VERDICT: High invested time but losing money. Exit too reactive -
    getting shaken out on normal pullbacks. Trend following on individual
    tech stocks doesn't work well with tight exits.

    NOTE: Requires -i 4000 for warmup *)
let tech_long_ma_trend : Gadt_strategy.t =
  (* Fixed long-period SMA - 3900 bars = ~2 weeks *)
  let sma_long =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (3900, Int)) ))
  in

  (* Entry: price 1% above SMA (confirmed uptrend) *)
  let trend_threshold = Const (1.01, Float) in
  let in_uptrend = last >. sma_long *. trend_threshold in

  (* Exit: trend broken (price below SMA) or 3% stop or max hold 1500 bars (~4 days) *)
  let trend_broken = last <. sma_long in
  let stop_mult = Const (0.97, Float) in

  {
    name = "Tech_LongMA_Trend";
    buy_trigger = in_uptrend;
    sell_trigger =
      trend_broken
      ||. (last <. EntryPrice *. stop_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1500, Int));
    score = last -. sma_long;  (* Further above MA = stronger trend *)
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_LongBB_MR - Long-Period Bollinger Band Mean Reversion (Multi-Day Hold)

    HYPOTHESIS: Bollinger Bands adapt to volatility - a touch of the lower
    band means price is stretched relative to RECENT volatility, not just
    a fixed % below average. This should be more robust than fixed SMA %.

    Parameters:
    - BB period: 3900 bars (~2 weeks)
    - BB std: 2.0 (standard)

    Entry: price < lower band (oversold relative to volatility)
    Exit: price > middle band (reverted to mean) or stop or max hold

    NO EOD EXIT - holds overnight for multi-day mean reversion.

    NOTE: Requires -i 4000 for warmup *)
let tech_long_bb_mr : Gadt_strategy.t =
  (* Long-period Bollinger Bands - 3900 bars, 2.0 std *)
  let bb_period = Const (3900, Int) in
  let bb_std = Const (2.0, Float) in

  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", I.lower_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", I.middle_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  (* Entry: price touches lower band *)
  let oversold = last <. bb_lower in

  (* Exit: reverted to middle band, or 5% stop, or max hold 1500 bars *)
  let reverted = last >. bb_middle in
  let stop_mult = Const (0.95, Float) in

  {
    name = "Tech_LongBB_MR";
    buy_trigger = oversold;
    sell_trigger =
      reverted
      ||. (last <. EntryPrice *. stop_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1500, Int));
    score = bb_middle -. last;  (* Further below middle = higher priority *)
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_LongBB_Trend - Long-Period Bollinger Band Trend (Multi-Day Hold)

    HYPOTHESIS: When price breaks above upper band with sustained momentum,
    it indicates a strong trend that continues. Use bands to filter for
    genuine breakouts vs noise.

    Parameters:
    - BB period: 3900 bars (~2 weeks)
    - BB std: 2.0 (standard)

    Entry: price > upper band (breakout above volatility range)
    Exit: price < middle band (trend exhausted) or stop or max hold

    NO EOD EXIT - holds overnight.

    NOTE: Requires -i 4000 for warmup *)
let tech_long_bb_trend : Gadt_strategy.t =
  (* Long-period Bollinger Bands - 3900 bars, 2.0 std *)
  let bb_period = Const (3900, Int) in
  let bb_std = Const (2.0, Float) in

  let bb_upper =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.upper_bband", I.upper_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", I.middle_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  (* Entry: price breaks above upper band *)
  let breakout = last >. bb_upper in

  (* Exit: drops to middle band, or 3% stop, or max hold 1500 bars *)
  let trend_exhausted = last <. bb_middle in
  let stop_mult = Const (0.97, Float) in

  {
    name = "Tech_LongBB_Trend";
    buy_trigger = breakout;
    sell_trigger =
      trend_exhausted
      ||. (last <. EntryPrice *. stop_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1500, Int));
    score = last -. bb_upper;  (* Further above upper = stronger breakout *)
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_LongBB_MR_V2 - BB Mean Reversion with Short-Term MFI Filter

    Building on Tech_LongBB_MR with:
    - 3 positions at 33% each (more diversification)
    - Short-period MFI(15) for entry/exit timing
    - Buy: below lower BB AND MFI < 35 (capitulation)
    - Sell: above middle BB AND MFI > 70 (both required)

    The short MFI acts as a "timing" filter on top of the long BB signal.
    Entry: Low MFI + below band = genuine capitulation selling.
    Exit: Must revert to mean AND have buying exhaustion confirmed.

    NOTE: Requires -i 4000 for warmup *)
let tech_long_bb_mr_v2 : Gadt_strategy.t =
  (* Long-period Bollinger Bands - 3900 bars, 2.0 std *)
  let bb_period = Const (3900, Int) in
  let bb_std = Const (2.0, Float) in

  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", I.lower_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", I.middle_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  (* Short-period MFI for timing - 15 bars = 15 minutes *)
  let mfi_short =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (15, Int)) ))
  in

  (* Entry: below lower band AND weak MFI (capitulation) *)
  let below_band = last <. bb_lower in
  let weak_mfi = mfi_short <. Const (35.0, Float) in

  (* Exit: reverted to middle band AND overbought MFI (both required) *)
  let overbought_mfi = mfi_short >. Const (70.0, Float) in
  let reverted = last >. bb_middle in
  let take_profit = reverted &&. overbought_mfi in
  let stop_mult = Const (0.95, Float) in

  {
    name = "Tech_LongBB_MR_V2";
    buy_trigger = below_band &&. weak_mfi;
    sell_trigger =
      take_profit
      ||. (last <. EntryPrice *. stop_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1500, Int));
    score = bb_middle -. last;  (* Further below middle = higher priority *)
    max_positions = 3;
    position_size = 0.33;
  }

(** Tech_LongBB_MR_V3 - Refined BB Mean Reversion

    Building on V2 with:
    - 2 positions at 50% (better for 17-stock universe)
    - Looser MFI exit: 60 instead of 70 (exit sooner)
    - Tighter stop: 3% instead of 5%
    - Buy: below lower BB AND MFI < 35
    - Sell: above middle BB AND MFI > 60 (both required)

    NOTE: Requires -i 4000 for warmup *)
let tech_long_bb_mr_v3 : Gadt_strategy.t =
  (* Long-period Bollinger Bands - 3900 bars, 2.0 std *)
  let bb_period = Const (3900, Int) in
  let bb_std = Const (2.0, Float) in

  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", I.lower_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", I.middle_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  (* Short-period MFI for timing - 15 bars = 15 minutes *)
  let mfi_short =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (15, Int)) ))
  in

  (* Entry: below lower band AND weak MFI *)
  let below_band = last <. bb_lower in
  let weak_mfi = mfi_short <. Const (35.0, Float) in

  (* Exit: reverted AND MFI > 60 (looser than V2's 70) *)
  let overbought_mfi = mfi_short >. Const (60.0, Float) in
  let reverted = last >. bb_middle in
  let take_profit = reverted &&. overbought_mfi in
  let stop_mult = Const (0.97, Float) in  (* Tighter 3% stop *)

  {
    name = "Tech_LongBB_MR_V3";
    buy_trigger = below_band &&. weak_mfi;
    sell_trigger =
      take_profit
      ||. (last <. EntryPrice *. stop_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1500, Int));
    score = bb_middle -. last;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy - Buy Small Dips in Uptrend

    HYPOTHESIS: In an uptrend (above 3-day SMA), small dips (low MFI)
    are buying opportunities. More frequent trades than deep MR.

    Entry:
    - Price > 3-day SMA (1170 bars) - confirms uptrend
    - MFI(30) < 30 - short-term dip/weakness

    Exit:
    - 5% stop loss
    - 10% take profit
    - MFI(30) > 70 - price spiked, take profits
    - Max hold 1170 bars (~3 days)

    Should generate more trades than deep BB mean reversion.

    NOTE: Requires -i 1200 for warmup *)
let tech_dip_buy : Gadt_strategy.t =
  (* 3-day SMA - 1170 bars *)
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in

  (* 30-period MFI for timing *)
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in

  (* Entry: uptrend AND weak short-term MFI *)
  let in_uptrend = last >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  (* Exit conditions *)
  let stop_mult = Const (0.95, Float) in
  let profit_mult = Const (1.10, Float) in
  let mfi_spike = mfi_30 >. Const (70.0, Float) in

  {
    name = "Tech_Dip_Buy";
    buy_trigger = in_uptrend &&. dip;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. mfi_spike
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = Const (30.0, Float) -. mfi_30;  (* Lower MFI = better opportunity *)
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_TightStop - Variant with 3% stop instead of 5% *)
let tech_dip_buy_tight_stop : Gadt_strategy.t =
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let in_uptrend = last >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in
  let stop_mult = Const (0.97, Float) in  (* 3% stop *)
  let profit_mult = Const (1.10, Float) in
  let mfi_spike = mfi_30 >. Const (70.0, Float) in
  {
    name = "Tech_Dip_Buy_TightStop";
    buy_trigger = in_uptrend &&. dip;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. mfi_spike
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = Const (30.0, Float) -. mfi_30;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_Recovery - Variant requiring price bounce (price > lag 1) *)
let tech_dip_buy_recovery : Gadt_strategy.t =
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let in_uptrend = last >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in
  let recovering = last >. lag last 1 in  (* Price bouncing *)
  let stop_mult = Const (0.95, Float) in
  let profit_mult = Const (1.10, Float) in
  let mfi_spike = mfi_30 >. Const (70.0, Float) in
  {
    name = "Tech_Dip_Buy_Recovery";
    buy_trigger = in_uptrend &&. dip &&. recovering;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. mfi_spike
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = Const (30.0, Float) -. mfi_30;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_RisingSMA - Variant requiring SMA to be rising *)
let tech_dip_buy_rising_sma : Gadt_strategy.t =
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let in_uptrend = last >. sma_3day in
  let sma_rising = sma_3day >. lag sma_3day 30 in  (* SMA higher than 30 bars ago *)
  let dip = mfi_30 <. Const (30.0, Float) in
  let stop_mult = Const (0.95, Float) in
  let profit_mult = Const (1.10, Float) in
  let mfi_spike = mfi_30 >. Const (70.0, Float) in
  {
    name = "Tech_Dip_Buy_RisingSMA";
    buy_trigger = in_uptrend &&. sma_rising &&. dip;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. mfi_spike
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = Const (30.0, Float) -. mfi_30;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_QuickProfit - Variant with 6% profit target instead of 10% *)
let tech_dip_buy_quick_profit : Gadt_strategy.t =
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let in_uptrend = last >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in
  let stop_mult = Const (0.95, Float) in
  let profit_mult = Const (1.06, Float) in  (* 6% profit target *)
  let mfi_spike = mfi_30 >. Const (70.0, Float) in
  {
    name = "Tech_Dip_Buy_QuickProfit";
    buy_trigger = in_uptrend &&. dip;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. mfi_spike
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = Const (30.0, Float) -. mfi_30;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V2 - Combined best features

    Combines:
    - 3% tight stop
    - Stacked SMA trend: SMA(390) > SMA(780) > SMA(1170)
      (1-day > 2-day > 3-day = bullish alignment)

    Entry:
    - Price > SMA(1170)
    - Stacked SMAs (bullish trend)
    - MFI(30) < 30 (short-term dip)

    Exit:
    - 3% stop
    - 10% profit
    - MFI > 70
    - Max hold 1170 bars *)
let tech_dip_buy_v2 : Gadt_strategy.t =
  (* Multi-period SMAs for trend alignment *)
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_2day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (780, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in

  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in

  (* Stacked SMAs: 1-day > 2-day > 3-day = bullish alignment *)
  let stacked_bullish = (sma_1day >. sma_2day) &&. (sma_2day >. sma_3day) in
  let in_uptrend = last >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  let stop_mult = Const (0.97, Float) in  (* 3% stop *)
  let profit_mult = Const (1.10, Float) in
  let mfi_spike = mfi_30 >. Const (70.0, Float) in

  {
    name = "Tech_Dip_Buy_V2";
    buy_trigger = in_uptrend &&. stacked_bullish &&. dip;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. mfi_spike
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = Const (30.0, Float) -. mfi_30;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V3 - Relaxed trend filter with trend strength in score

    Changes from V2:
    - Entry filter: just SMA(390) > SMA(1170) (short > long = uptrend)
    - Trend strength (SMA difference) added to score
    - Keeps 3% tight stop

    Entry:
    - Price > SMA(1170) (above 3-day)
    - SMA(390) > SMA(1170) (1-day > 3-day = uptrend)
    - MFI(30) < 30 (short-term dip)

    Exit:
    - 3% stop
    - 10% profit
    - MFI > 70
    - Max hold 1170 bars

    Score:
    - Base: 30 - MFI (lower MFI = better dip)
    - Plus: SMA(390) - SMA(1170) (stronger trend = higher priority) *)
let tech_dip_buy_v3 : Gadt_strategy.t =
  (* SMAs for trend detection *)
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in

  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in

  (* Entry: relaxed trend filter - just short > long *)
  let in_uptrend = last >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  let stop_mult = Const (0.97, Float) in  (* 3% stop *)
  let profit_mult = Const (1.10, Float) in
  let mfi_spike = mfi_30 >. Const (70.0, Float) in

  (* Score: lower MFI + stronger stacking = better
     sma_1day - sma_3day measures trend strength *)
  let base_score = Const (30.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V3";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. mfi_spike
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V4 - No MFI exit (let winners run)

    Theory: MFI > 70 exit is cutting winners too early.
    Remove it - only exit on stop, profit target, or max hold.
    3% stop, 10% profit target. *)
let tech_dip_buy_v4 : Gadt_strategy.t =
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in

  let in_uptrend = last >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  let stop_mult = Const (0.97, Float) in  (* 3% stop *)
  let profit_mult = Const (1.10, Float) in
  (* NO MFI exit - let it run *)

  let base_score = Const (30.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V4";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V5 - Long MFI exit (no fixed profit target)

    Theory: Let winners run using indicator-based exit.
    Entry: short MFI(30) < 30 (dip)
    Exit: daily MFI(390) > 70 (momentum exhaustion)
    No fixed profit target - ride until daily momentum fades.

    BASELINE RESULTS (quarterly_2023_2025, tech universe):
    - Avg Sharpe: 0.053 (std: 0.062)
    - Avg Return: 8.70% (std: 13.15%)
    - Worst Drawdown: 24.90%
    - Consistency: 83.3% (5/6 periods profitable)
    - Avg Invested: 93.5%
    - Trades per period: ~118

    Best result so far. Goal: >0.1 Sharpe in 5/6 periods. *)
let tech_dip_buy_v5 : Gadt_strategy.t =
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  (* Daily MFI for exit *)
  let mfi_daily =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (390, Int)) ))
  in

  let in_uptrend = last >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  (* Exit on daily momentum exhaustion, hard stop, or max hold *)
  let momentum_exhausted = mfi_daily >. Const (70.0, Float) in
  let hard_stop = last <. EntryPrice *. Const (0.95, Float) in

  let base_score = Const (30.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V5";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      momentum_exhausted
      ||. hard_stop
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V6 - Scalper (tiny symmetric targets)

    Theory: With 60% win rate, we don't need big wins.
    Match stop to target: 2% stop, 2% profit.
    Quick in/out - don't wait for home runs. *)
let tech_dip_buy_v6 : Gadt_strategy.t =
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in

  let in_uptrend = last >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  (* Symmetric small targets *)
  let stop_mult = Const (0.98, Float) in   (* 2% stop *)
  let profit_mult = Const (1.02, Float) in (* 2% profit *)

  let base_score = Const (30.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V6";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (390, Int));  (* 1 day max *)
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V7 - V5 with tighter stop (3% instead of 5%)

    Testing if tighter stop improves risk-adjusted returns. *)
let tech_dip_buy_v7 : Gadt_strategy.t =
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let mfi_daily =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (390, Int)) ))
  in

  let in_uptrend = last >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  let momentum_exhausted = mfi_daily >. Const (70.0, Float) in
  let hard_stop = last <. EntryPrice *. Const (0.97, Float) in  (* 3% stop *)

  let base_score = Const (30.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V7";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      momentum_exhausted
      ||. hard_stop
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V8 - V5 with earlier MFI exit (60 instead of 70)

    Testing if exiting earlier on momentum preserves more gains. *)
let tech_dip_buy_v8 : Gadt_strategy.t =
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let mfi_daily =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (390, Int)) ))
  in

  let in_uptrend = last >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  let momentum_exhausted = mfi_daily >. Const (60.0, Float) in  (* Earlier exit *)
  let hard_stop = last <. EntryPrice *. Const (0.95, Float) in

  let base_score = Const (30.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V8";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      momentum_exhausted
      ||. hard_stop
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V9 - V5 with 2-day MFI exit (780 bars)

    Testing if longer MFI period lets winners run even more. *)
let tech_dip_buy_v9 : Gadt_strategy.t =
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let mfi_2day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (780, Int)) ))
  in

  let in_uptrend = last >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  let momentum_exhausted = mfi_2day >. Const (70.0, Float) in  (* 2-day MFI *)
  let hard_stop = last <. EntryPrice *. Const (0.95, Float) in

  let base_score = Const (30.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V9";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      momentum_exhausted
      ||. hard_stop
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V10 - Smoothed price (no raw last)

    Use SMA(5) instead of raw price for all comparisons.
    Reduces noise/whipsaws from minute-to-minute fluctuations.

    Entry: SMA(5) > SMA(1170) AND SMA(390) > SMA(1170) AND MFI(30) < 30
    Exit: daily MFI > 70 OR smoothed price stop OR max hold *)
let tech_dip_buy_v10 : Gadt_strategy.t =
  (* Smoothed price - 5 bar SMA *)
  let price_smoothed =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (5, Int)) ))
  in
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let mfi_daily =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (390, Int)) ))
  in

  (* All price comparisons use smoothed price *)
  let in_uptrend = price_smoothed >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (30.0, Float) in

  let momentum_exhausted = mfi_daily >. Const (70.0, Float) in
  let hard_stop = price_smoothed <. EntryPrice *. Const (0.95, Float) in

  let base_score = Const (30.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V10";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      momentum_exhausted
      ||. hard_stop
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V11 - BEST ACTIVE STRATEGY on tech universe

    Same as V10 but with looser dip threshold (MFI < 40 instead of < 30).

    Entry:
    - Smoothed price SMA(5) > SMA(1170)
    - SMA(390) > SMA(1170) (trend alignment)
    - MFI(30) < 40 (shallow dip)

    Exit:
    - Daily MFI(390) > 70 (momentum exhaustion)
    - 5% hard stop
    - Max hold 1170 bars

    RESULTS (quarterly_2023_2025, tech universe):
    - Avg Sharpe: 0.098 (std: 0.093)
    - Avg Return: 16.05% (std: 15.28%)
    - Worst Drawdown: 26.89%
    - Consistency: 83.3% (5/6 periods profitable)
    - Avg Invested: 96.4%
    - Trades per period: ~115

    Still underperforms Buy & Hold Tech (0.609 Sharpe, 17.86% return, 100% consistency)
    but this is the best active strategy we've found for tech stocks. *)
let tech_dip_buy_v11 : Gadt_strategy.t =
  let price_smoothed =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (5, Int)) ))
  in
  let sma_1day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (390, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let mfi_daily =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (390, Int)) ))
  in

  let in_uptrend = price_smoothed >. sma_3day in
  let short_above_long = sma_1day >. sma_3day in
  let dip = mfi_30 <. Const (40.0, Float) in  (* Looser: 40 instead of 30 *)

  let momentum_exhausted = mfi_daily >. Const (70.0, Float) in
  let hard_stop = price_smoothed <. EntryPrice *. Const (0.95, Float) in

  let base_score = Const (40.0, Float) -. mfi_30 in
  let trend_strength = sma_1day -. sma_3day in

  {
    name = "Tech_Dip_Buy_V11";
    buy_trigger = in_uptrend &&. short_above_long &&. dip;
    sell_trigger =
      momentum_exhausted
      ||. hard_stop
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score +. trend_strength;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V12 - More trades: remove SMA stacking requirement

    Only require price > SMA(1170), drop the SMA(390) > SMA(1170) filter. *)
let tech_dip_buy_v12 : Gadt_strategy.t =
  let price_smoothed =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (5, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let mfi_daily =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (390, Int)) ))
  in

  let in_uptrend = price_smoothed >. sma_3day in
  (* No SMA stacking requirement *)
  let dip = mfi_30 <. Const (30.0, Float) in

  let momentum_exhausted = mfi_daily >. Const (70.0, Float) in
  let hard_stop = price_smoothed <. EntryPrice *. Const (0.95, Float) in

  let base_score = Const (30.0, Float) -. mfi_30 in

  {
    name = "Tech_Dip_Buy_V12";
    buy_trigger = in_uptrend &&. dip;
    sell_trigger =
      momentum_exhausted
      ||. hard_stop
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score;
    max_positions = 2;
    position_size = 0.5;
  }

(** Tech_Dip_Buy_V13 - More trades: both looser MFI AND no SMA stacking

    Combines V11 and V12 for maximum trade generation. *)
let tech_dip_buy_v13 : Gadt_strategy.t =
  let price_smoothed =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (5, Int)) ))
  in
  let sma_3day =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.sma", I.sma), Const (1170, Int)) ))
  in
  let mfi_30 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (30, Int)) ))
  in
  let mfi_daily =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (390, Int)) ))
  in

  let in_uptrend = price_smoothed >. sma_3day in
  let dip = mfi_30 <. Const (40.0, Float) in  (* Looser *)

  let momentum_exhausted = mfi_daily >. Const (70.0, Float) in
  let hard_stop = price_smoothed <. EntryPrice *. Const (0.95, Float) in

  let base_score = Const (40.0, Float) -. mfi_30 in

  {
    name = "Tech_Dip_Buy_V13";
    buy_trigger = in_uptrend &&. dip;
    sell_trigger =
      momentum_exhausted
      ||. hard_stop
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (1170, Int));
    score = base_score;
    max_positions = 2;
    position_size = 0.5;
  }

(** SPY_MFI_Simple - Braindead simple: buy SPY on dip, sell on spike

    Entry: MFI(15) < 30
    Exit: MFI(15) > 70
    Universe: spy (just SPY)

    RESULT: 0% consistency, -23% avg return. Perfectly wrong!
    Mean reversion doesn't work on SPY - momentum does. *)
let spy_mfi_simple : Gadt_strategy.t =
  let mfi_15 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (15, Int)) ))
  in

  let oversold = mfi_15 <. Const (30.0, Float) in
  let overbought = mfi_15 >. Const (70.0, Float) in

  {
    name = "SPY_MFI_Simple";
    buy_trigger = oversold;
    sell_trigger = overbought;
    score = Const (30.0, Float) -. mfi_15;
    max_positions = 1;
    position_size = 1.0;
  }

(** SPY_MFI_Opt - 4-variable optimizable MFI strategy for SPY

    Buy: MFI(buy_period) < buy_threshold
    Sell: MFI(sell_period) > sell_threshold

    Variables:
    1. buy_period: [5, 1000] - MFI lookback for entry
    2. buy_threshold: [10, 50] - MFI level to buy below
    3. sell_period: [5, 1000] - MFI lookback for exit
    4. sell_threshold: [50, 90] - MFI level to sell above

    Philosophy: SPY drifts up forever, buy oversold, sell overbought.
    ISRES finds optimal periods and thresholds. *)
let spy_mfi_opt : Gadt_strategy.t =
  let buy_period_var = Gadt_fo.var ~lower:5.0 ~upper:1000.0 Type.Int in
  let buy_threshold_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Type.Float in
  let sell_period_var = Gadt_fo.var ~lower:5.0 ~upper:1000.0 Type.Int in
  let sell_threshold_var = Gadt_fo.var ~lower:50.0 ~upper:90.0 Type.Float in

  let buy_mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), buy_period_var) ))
  in
  let sell_mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), sell_period_var) ))
  in

  let oversold = buy_mfi <. buy_threshold_var in
  let overbought = sell_mfi >. sell_threshold_var in

  {
    name = "SPY_MFI_Opt";
    buy_trigger = oversold;
    sell_trigger = overbought;
    score = buy_threshold_var -. buy_mfi;  (* Deeper oversold = higher priority *)
    max_positions = 1;
    position_size = 1.0;
  }

(** SPY_MFI_Flipped - Momentum: buy strength, sell weakness

    Entry: MFI(15) > 70 (momentum, not mean reversion)
    Exit: MFI(15) < 30 (weakness)

    Opposite of SPY_MFI_Simple which was 0% consistent. *)
let spy_mfi_flipped : Gadt_strategy.t =
  let mfi_15 =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", I.mfi), Const (15, Int)) ))
  in

  let strong = mfi_15 >. Const (70.0, Float) in
  let weak = mfi_15 <. Const (30.0, Float) in

  {
    name = "SPY_MFI_Flipped";
    buy_trigger = strong;
    sell_trigger = weak;
    score = mfi_15;  (* Higher MFI = better *)
    max_positions = 1;
    position_size = 1.0;
  }

(** Buy_And_Hold_Tech - Benchmark: buy and hold tech universe

    Simple benchmark - buy everything and hold.
    No sell trigger except max hold (entire period). *)
let buy_and_hold_tech : Gadt_strategy.t =
  {
    name = "Buy_And_Hold_Tech";
    buy_trigger = Const (true, Bool);  (* Always buy *)
    sell_trigger = Const (false, Bool);  (* Never sell *)
    score = Const (1.0, Float);
    max_positions = 17;  (* All 17 tech stocks *)
    position_size = 0.058;  (* ~1/17 each *)
  }

(* Export all strategies *)
let all_strategies =
  [
    tech_momentum_opt;
    tech_breakout_opt;
    tech_ema_cross_opt;
    tech_gap_go_opt;
    tech_gap_go_a;
    tech_gap_go_b;
    tech_gap_go_c;
    tech_long_ma_mr;
    tech_long_ma_trend;
    tech_long_bb_mr;
    tech_long_bb_trend;
    tech_long_bb_mr_v2;
    tech_long_bb_mr_v3;
    tech_dip_buy;
    tech_dip_buy_tight_stop;
    tech_dip_buy_recovery;
    tech_dip_buy_rising_sma;
    tech_dip_buy_quick_profit;
    tech_dip_buy_v2;
    tech_dip_buy_v3;
    tech_dip_buy_v4;
    tech_dip_buy_v5;
    tech_dip_buy_v6;
    tech_dip_buy_v7;
    tech_dip_buy_v8;
    tech_dip_buy_v9;
    tech_dip_buy_v10;
    tech_dip_buy_v11;
    tech_dip_buy_v12;
    tech_dip_buy_v13;
    spy_mfi_simple;
    spy_mfi_opt;
    spy_mfi_flipped;
    buy_and_hold_tech;
  ]
