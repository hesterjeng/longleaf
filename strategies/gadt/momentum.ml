(** Momentum and Trend Following Strategies

    This module contains momentum and trend-following strategies, contrasting
    with mean reversion approaches. Instead of buying weakness and selling
    strength, these strategies ride established trends.

    == RESEARCH CONTEXT (Dec 2025) ==

    After extensive testing of mean reversion strategies on S&P 100 with
    realistic execution costs (2 bps/side), we found that MR edge is largely
    competed away. Momentum/trend following may offer better opportunities:

    1. LOWER TURNOVER: Trend following typically has fewer trades than MR,
       reducing execution cost drag.

    2. DIFFERENT EDGE: Momentum exploits behavioral biases (herding, anchoring)
       rather than market microstructure inefficiencies.

    3. REGIME COMPLEMENT: Momentum works in trending markets where MR fails,
       providing potential diversification.

    4. LONGER HOLDS: Trend rides can last hours/days vs MR's minutes, allowing
       larger moves to develop.

    Key indicators used:
    - ADX (Average Directional Index): Trend strength measurement
    - EMA crossovers: Trend direction detection
    - MACD: Momentum confirmation
    - ROC (Rate of Change): Momentum magnitude
    - ATR: Volatility-adjusted stops and targets
    - MFI: Volume-weighted momentum *)

open Gadt
open Gadt_strategy
module Data = Longleaf_bars.Data

(** Momentum_Breakout_Opt - Intraday Momentum Breakout Strategy

    Catches stocks breaking out of consolidation with strong momentum.
    Uses ATR for volatility-adjusted entry thresholds.

    Variables (8 total):
    1. atr_period: [10, 50] - ATR period for volatility measurement
    2. atr_mult: [1.0, 3.0] - ATR multiplier for breakout threshold
    3. adx_period: [10, 30] - ADX period for trend strength
    4. adx_threshold: [20, 35] - Minimum ADX to confirm trend
    5. roc_period: [5, 20] - Rate of change period
    6. roc_threshold: [0.5, 2.0] - Minimum ROC % for entry
    7. stop_loss: [0.01, 0.03] - Stop loss percentage (1-3%)
    8. profit_target: [0.02, 0.06] - Profit target percentage (2-6%)

    Entry:
    - Price > SMA + (ATR * mult) - breakout above volatility band
    - ADX > threshold - strong trend
    - ROC > threshold - positive momentum
    - Safe to enter (not near EOD)

    Exit:
    - EOD (no overnight)
    - Stop loss hit
    - Profit target hit
    - ADX drops below 15 (trend weakening)
    - Max hold 180 bars (3 hours)

    NOTE: Use starting index of at least 100 (-i 100) for indicator warmup. *)
let momentum_breakout_opt =
  let atr_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Type.Int in
  let atr_mult_var = Gadt_fo.var ~lower:1.0 ~upper:3.0 Type.Float in
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:30.0 Type.Int in
  let adx_threshold_var = Gadt_fo.var ~lower:20.0 ~upper:35.0 Type.Float in
  let roc_period_var = Gadt_fo.var ~lower:5.0 ~upper:20.0 Type.Int in
  let roc_threshold_var = Gadt_fo.var ~lower:0.5 ~upper:2.0 Type.Float in
  let stop_loss_var = Gadt_fo.var ~lower:0.01 ~upper:0.03 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.02 ~upper:0.06 Type.Float in

  let sma_20 = Gadt_fo.Constant.sma 20 () in
  let atr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.atr", Tacaml.Indicator.Raw.atr), atr_period_var) ))
  in
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), adx_period_var) ))
  in
  let roc =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.roc", Tacaml.Indicator.Raw.roc), roc_period_var) ))
  in

  let breakout_level = sma_20 +. (atr *. atr_mult_var) in
  let stop_loss_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_target_mult = Const (1.0, Float) +. profit_target_var in

  {
    name = "Momentum_Breakout_Opt";
    buy_trigger =
      last >. breakout_level
      &&. (adx >. adx_threshold_var)
      &&. (roc >. roc_threshold_var)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. (adx <. Const (15.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = adx *. roc;  (* Higher trend strength and momentum = higher score *)
    max_positions = 3;
    position_size = 0.33;
  }

(** EMA_Crossover_Opt - Classic EMA Crossover with Trend Filter

    Golden cross / death cross strategy with ADX trend confirmation.
    Only enters when trend is strong enough to follow.

    Variables (6 total):
    1. fast_ema: [10, 30] - Fast EMA period
    2. slow_ema: [30, 80] - Slow EMA period
    3. adx_period: [10, 25] - ADX period
    4. adx_threshold: [20, 35] - Minimum ADX for entry
    5. stop_loss: [0.015, 0.04] - Stop loss (1.5-4%)
    6. profit_target: [0.03, 0.08] - Profit target (3-8%)

    Entry:
    - Fast EMA crosses above Slow EMA (golden cross)
    - ADX > threshold (trending market)
    - Safe to enter

    Exit:
    - EOD
    - Fast EMA crosses below Slow EMA (death cross)
    - Stop loss or profit target
    - ADX drops below 15
    - Max hold 240 bars (4 hours)

    NOTE: Use starting index of at least 100 (-i 100) for indicator warmup. *)
let ema_crossover_opt =
  let fast_ema_var = Gadt_fo.var ~lower:10.0 ~upper:30.0 Type.Int in
  let slow_ema_var = Gadt_fo.var ~lower:30.0 ~upper:80.0 Type.Int in
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:25.0 Type.Int in
  let adx_threshold_var = Gadt_fo.var ~lower:20.0 ~upper:35.0 Type.Float in
  let stop_loss_var = Gadt_fo.var ~lower:0.015 ~upper:0.04 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.03 ~upper:0.08 Type.Float in

  let fast_ema =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.ema", Tacaml.Indicator.Raw.ema), fast_ema_var) ))
  in
  let slow_ema =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.ema", Tacaml.Indicator.Raw.ema), slow_ema_var) ))
  in
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), adx_period_var) ))
  in

  let stop_loss_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_target_mult = Const (1.0, Float) +. profit_target_var in

  {
    name = "EMA_Crossover_Opt";
    buy_trigger =
      cross_up fast_ema slow_ema
      &&. (adx >. adx_threshold_var)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. cross_down fast_ema slow_ema
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. (adx <. Const (15.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (240, Int));
    score = adx;  (* Higher trend strength = higher priority *)
    max_positions = 3;
    position_size = 0.33;
  }

(** MACD_Momentum_Opt - MACD Histogram Momentum Strategy

    Uses MACD histogram direction and magnitude for momentum signals.
    Enters when histogram is positive and increasing.

    Variables (6 total):
    1. fast_period: [8, 15] - MACD fast EMA period
    2. slow_period: [20, 30] - MACD slow EMA period
    3. signal_period: [5, 12] - MACD signal line period
    4. adx_threshold: [20, 30] - Minimum ADX for trend confirmation
    5. stop_loss: [0.01, 0.03] - Stop loss (1-3%)
    6. profit_target: [0.02, 0.05] - Profit target (2-5%)

    Entry:
    - MACD histogram > 0 (bullish)
    - MACD histogram > histogram from 3 bars ago (accelerating)
    - ADX(14) > threshold
    - Safe to enter

    Exit:
    - EOD
    - MACD histogram turns negative
    - MACD histogram decreasing for 3 bars
    - Stop loss or profit target
    - Max hold 180 bars

    NOTE: Use starting index of at least 50 (-i 50) for indicator warmup. *)
let macd_momentum_opt =
  let fast_period_var = Gadt_fo.var ~lower:8.0 ~upper:15.0 Type.Int in
  let slow_period_var = Gadt_fo.var ~lower:20.0 ~upper:30.0 Type.Int in
  let signal_period_var = Gadt_fo.var ~lower:5.0 ~upper:12.0 Type.Int in
  let adx_threshold_var = Gadt_fo.var ~lower:20.0 ~upper:30.0 Type.Float in
  let stop_loss_var = Gadt_fo.var ~lower:0.01 ~upper:0.03 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.02 ~upper:0.05 Type.Float in

  let macd_hist =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3 (Fun ("I.macd_hist", Tacaml.Indicator.Raw.macd_hist),
                 fast_period_var, slow_period_var, signal_period_var) ))
  in
  let adx = Gadt_fo.Constant.adx 14 () in

  let hist_positive = macd_hist >. Const (0.0, Float) in
  let hist_accelerating = macd_hist >. lag macd_hist 3 in

  let stop_loss_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_target_mult = Const (1.0, Float) +. profit_target_var in

  {
    name = "MACD_Momentum_Opt";
    buy_trigger =
      hist_positive
      &&. hist_accelerating
      &&. (adx >. adx_threshold_var)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (macd_hist <. Const (0.0, Float))  (* Histogram turned negative *)
      ||. (macd_hist <. lag macd_hist 3)     (* Histogram decelerating *)
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = macd_hist *. Const (100.0, Float);  (* Higher histogram = higher score *)
    max_positions = 3;
    position_size = 0.33;
  }

(** MFI_Momentum_Opt - MFI Momentum (NOT Mean Reversion)

    Uses MFI as a momentum indicator rather than overbought/oversold.
    Enters when MFI is in the "momentum zone" (50-70) and rising.
    MFI incorporates volume, making it more robust than RSI alone.

    Research: MFI between 50-70 often indicates sustained upward momentum
    with volume confirmation, while MFI > 70 can signal exhaustion.
    This strategy rides the momentum zone rather than fading extremes.

    Variables (6 total):
    1. mfi_period: [10, 50] - MFI period
    2. mfi_low: [45, 55] - Lower bound of momentum zone
    3. mfi_high: [65, 80] - Upper bound (exit when exceeded)
    4. adx_threshold: [20, 30] - Minimum ADX
    5. stop_loss: [0.01, 0.025] - Stop loss (1-2.5%)
    6. profit_target: [0.02, 0.04] - Profit target (2-4%)

    Entry:
    - MFI in momentum zone (50-70)
    - MFI rising (current > 3 bars ago)
    - ADX > threshold
    - Safe to enter

    Exit:
    - EOD
    - MFI exits momentum zone (< low or > high)
    - MFI falling (current < 3 bars ago)
    - Stop loss or profit target
    - Max hold 180 bars

    NOTE: Use starting index of at least 100 (-i 100) for indicator warmup. *)
let mfi_momentum_opt =
  let mfi_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Type.Int in
  let mfi_low_var = Gadt_fo.var ~lower:45.0 ~upper:55.0 Type.Float in
  let mfi_high_var = Gadt_fo.var ~lower:65.0 ~upper:80.0 Type.Float in
  let adx_threshold_var = Gadt_fo.var ~lower:20.0 ~upper:30.0 Type.Float in
  let stop_loss_var = Gadt_fo.var ~lower:0.01 ~upper:0.025 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.02 ~upper:0.04 Type.Float in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period_var) ))
  in
  let adx = Gadt_fo.Constant.adx 14 () in

  let in_momentum_zone = (mfi >. mfi_low_var) &&. (mfi <. mfi_high_var) in
  let mfi_rising = mfi >. lag mfi 3 in

  let stop_loss_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_target_mult = Const (1.0, Float) +. profit_target_var in

  {
    name = "MFI_Momentum_Opt";
    buy_trigger =
      in_momentum_zone
      &&. mfi_rising
      &&. (adx >. adx_threshold_var)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (mfi <. mfi_low_var)  (* Dropped below momentum zone *)
      ||. (mfi >. mfi_high_var) (* Exceeded momentum zone - exhaustion *)
      ||. (mfi <. lag mfi 3)    (* MFI falling *)
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = mfi -. Const (50.0, Float);  (* Higher MFI in zone = higher score *)
    max_positions = 3;
    position_size = 0.33;
  }

(* Export all strategies *)
let all_strategies =
  [
    momentum_breakout_opt;
    ema_crossover_opt;
    macd_momentum_opt;
    mfi_momentum_opt;
  ]
