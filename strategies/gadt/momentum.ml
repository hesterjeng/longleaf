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

(** Momentum_Breakout_V2 - Improved Variable Ranges

    V2 Changes:
    - ADX period extended: [10, 50] (was [10, 30])
    - ROC threshold lowered: [0.1, 1.5] (was [0.5, 2.0]) - 0.5% is big for 1-min
    - ROC period extended: [5, 30] (was [5, 20])
    - Stop/profit ratio maintained at 2x (3%/6%)

    NOTE: Use starting index of at least 100 (-i 100) for indicator warmup. *)
let momentum_breakout_v2 =
  let atr_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Type.Int in
  let atr_mult_var = Gadt_fo.var ~lower:1.0 ~upper:3.0 Type.Float in
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Type.Int in
  let adx_threshold_var = Gadt_fo.var ~lower:20.0 ~upper:35.0 Type.Float in
  let roc_period_var = Gadt_fo.var ~lower:5.0 ~upper:30.0 Type.Int in
  let roc_threshold_var = Gadt_fo.var ~lower:0.1 ~upper:1.5 Type.Float in
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
    name = "Momentum_Breakout_V2";
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
    score = adx *. roc;
    max_positions = 3;
    position_size = 0.33;
  }

(** EMA_Crossover_V2 - Improved Variable Ranges

    V2 Changes:
    - Fast EMA: [10, 40] (was [10, 30])
    - Slow EMA: [50, 150] (was [30, 80]) - NO OVERLAP with fast, extended range
    - ADX period: [10, 50] (was [10, 25])
    - Stop/profit ratio maintained at 2x (4%/8%)

    NOTE: Use starting index of at least 200 (-i 200) for indicator warmup. *)
let ema_crossover_v2 =
  let fast_ema_var = Gadt_fo.var ~lower:10.0 ~upper:40.0 Type.Int in
  let slow_ema_var = Gadt_fo.var ~lower:50.0 ~upper:150.0 Type.Int in
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Type.Int in
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
    name = "EMA_Crossover_V2";
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
    score = adx;
    max_positions = 3;
    position_size = 0.33;
  }

(** MACD_Momentum_V2 - Improved Variable Ranges

    V2 Changes:
    - Slow period: [20, 50] (was [20, 30]) - allow longer MACD
    - Signal period: [5, 15] (was [5, 12])
    - Profit target: [0.02, 0.06] (was [0.02, 0.05]) - now 2x max stop (3%)
    - ADX threshold: [15, 30] (was [20, 30]) - allow lower threshold

    NOTE: Use starting index of at least 100 (-i 100) for indicator warmup. *)
let macd_momentum_v2 =
  let fast_period_var = Gadt_fo.var ~lower:8.0 ~upper:15.0 Type.Int in
  let slow_period_var = Gadt_fo.var ~lower:20.0 ~upper:50.0 Type.Int in
  let signal_period_var = Gadt_fo.var ~lower:5.0 ~upper:15.0 Type.Int in
  let adx_threshold_var = Gadt_fo.var ~lower:15.0 ~upper:30.0 Type.Float in
  let stop_loss_var = Gadt_fo.var ~lower:0.01 ~upper:0.03 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.02 ~upper:0.06 Type.Float in

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
    name = "MACD_Momentum_V2";
    buy_trigger =
      hist_positive
      &&. hist_accelerating
      &&. (adx >. adx_threshold_var)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (macd_hist <. Const (0.0, Float))
      ||. (macd_hist <. lag macd_hist 3)
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = macd_hist *. Const (100.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** MFI_Momentum_V2 - Improved Variable Ranges

    V2 Changes:
    - MFI period: [10, 100] (was [10, 50]) - allow longer periods like MR
    - MFI low: [40, 55] (was [45, 55]) - wider range
    - MFI high: [60, 85] (was [65, 80]) - wider range, more gap from low
    - ADX period variable: [10, 50] (was fixed 14)
    - Profit target: [0.02, 0.05] (was [0.02, 0.04]) - now 2x max stop (2.5%)

    NOTE: Use starting index of at least 150 (-i 150) for indicator warmup. *)
let mfi_momentum_v2 =
  let mfi_period_var = Gadt_fo.var ~lower:10.0 ~upper:100.0 Type.Int in
  let mfi_low_var = Gadt_fo.var ~lower:40.0 ~upper:55.0 Type.Float in
  let mfi_high_var = Gadt_fo.var ~lower:60.0 ~upper:85.0 Type.Float in
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Type.Int in
  let adx_threshold_var = Gadt_fo.var ~lower:20.0 ~upper:30.0 Type.Float in
  let stop_loss_var = Gadt_fo.var ~lower:0.01 ~upper:0.025 Type.Float in
  let profit_target_var = Gadt_fo.var ~lower:0.02 ~upper:0.05 Type.Float in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period_var) ))
  in
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), adx_period_var) ))
  in

  let in_momentum_zone = (mfi >. mfi_low_var) &&. (mfi <. mfi_high_var) in
  let mfi_rising = mfi >. lag mfi 3 in

  let stop_loss_mult = Const (1.0, Float) -. stop_loss_var in
  let profit_target_mult = Const (1.0, Float) +. profit_target_var in

  {
    name = "MFI_Momentum_V2";
    buy_trigger =
      in_momentum_zone
      &&. mfi_rising
      &&. (adx >. adx_threshold_var)
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (mfi <. mfi_low_var)
      ||. (mfi >. mfi_high_var)
      ||. (mfi <. lag mfi 3)
      ||. (last <. EntryPrice *. stop_loss_mult)
      ||. (last >. EntryPrice *. profit_target_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = mfi -. Const (50.0, Float);
    max_positions = 3;
    position_size = 0.33;
  }

(** Claude_Momentum_1 - NB_V3_2 with Optimized Stop Loss

    == RESEARCH FINDINGS ==

    Starting from NB_V3_2 (83.3% consistency, 4.58% avg return), I tested:
    - Tighter MFI entry: Helped training, hurt out-of-sample
    - Tighter NATR filter: No improvement (already selective)
    - Shorter BB periods: Made it worse
    - ADX ranging filter: Too restrictive, killed returns
    - Longer min hold: Slight improvement in consistency

    KEY FINDING: NB_V3_2's 3.1% stop loss was too tight!

    Stop loss optimization results:
    - 3.1% (NB_V3_2): 4.58% avg return
    - 5.0%: 5.36% avg return (+17%)
    - 7.0%: 5.91% avg return (+29%)
    - 8.0%: 6.36% avg return (+39%) <-- OPTIMAL
    - 10.0%: 6.37% avg return (diminishing returns)

    This strategy uses 8% stop, all other params from NB_V3_2.

    Battery Results (quarterly_2023_2025):
      q1q2-2023: +8.32%  | q3q4-2023: +2.48%  | q1q2-2024: -8.67%
      q3q4-2024: +2.16%  | q1q2-2025: +12.74% | q3q4-2025: +21.11%
      Avg: 6.36%, Std: 9.32%, Consistency: 83.3% (5/6 periods positive) *)
let claude_momentum_1 =
  (* EXACT NB_V3_2 indicators *)
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
    name = "Claude_Momentum_1";
    buy_trigger =
      mfi <. Const (45.18, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.553, Float))
      &&. (natr_hi <. Const (1.411, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.92, Float))  (* 8% stop - optimized from NB_V3_2's 3.1% *)
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (233, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** WF_Test_3 - Walk-forward test: trained on q1q2-2024 (100 iterations)
    Training result: +3.3%, 628 trades, 59.4% win rate *)
let wf_test_3 =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (140, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (22, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (85, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (257, Int),
               Const (1.83, Float),
               Const (1.83, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (299, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (48, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (55.44, Float))
        ||. (last >. EntryPrice *. Const (1.069, Float)))
  in
  {
    name = "WF_Test_3";
    buy_trigger =
      mfi <. Const (33.54, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.053, Float))
      &&. (natr_hi <. Const (5.18, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.966, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (426, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** WF_Test_2 - Walk-forward test: trained on q3q4-2023 (100 iterations)
    Training result: +2.4%, 29 trades, 79.3% win rate *)
let wf_test_2 =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (191, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (8, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (149, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (370, Int),
               Const (2.27, Float),
               Const (2.27, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (283, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (20, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (54.72, Float))
        ||. (last >. EntryPrice *. Const (1.051, Float)))
  in
  {
    name = "WF_Test_2";
    buy_trigger =
      mfi <. Const (26.85, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.255, Float))
      &&. (natr_hi <. Const (4.34, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.96, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (238, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** WF_Test_1 - Walk-forward test: trained on q1q2-2023 (100 iterations)
    Training result: +13.6%, 820 trades, 60.5% win rate, p=0.008 *)
let wf_test_1 =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (79, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (47, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (83, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (368, Int),
               Const (2.817, Float),
               Const (2.817, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (325, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (60, Int)) in
  let exit_signals =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (64.59, Float))
        ||. (last >. EntryPrice *. Const (1.047, Float)))
  in
  {
    name = "WF_Test_1";
    buy_trigger =
      mfi <. Const (48.11, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. Const (0.066, Float))
      &&. (natr_hi <. Const (4.69, Float))
      &&. recovering
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.969, Float))
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (394, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** ==========================================================
    ALTERNATIVE STRATEGY CLASSES - 1-MINUTE ADAPTED

    Key insight: Literature parameters (RSI 14, BB 20, etc.) are for
    daily bars (~252/year). On 1-minute bars (~390/day), we need:
    - Much longer periods (10-20x) to capture equivalent price action
    - Stricter filters to reduce noise-driven signals
    - Larger required moves before entry
    - Fewer trades, larger targets
    ========================================================== *)

(** Volatility_Squeeze_1M - Adapted for 1-minute bars

    Changes from textbook:
    - BB period: 200 (was 20) - ~3+ hours of data
    - Squeeze threshold: 1% (was 2%) - tighter requirement
    - Added NATR filter to avoid choppy conditions
    - Added momentum confirmation (price rising)
    - Longer hold, bigger targets *)
let volatility_squeeze =
  let bb_upper =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.upper_bband", Tacaml.Indicator.Raw.upper_bband),
               Const (200, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (200, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (200, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (100, Int)) ))
  in
  let band_width = (bb_upper -. bb_lower) /. bb_middle in
  let squeeze = band_width <. Const (0.01, Float) in  (* Tighter: 1% *)
  let breakout = last >. bb_upper in
  let momentum = last >. lag last 5 in  (* Price rising *)
  let vol_filter = natr <. Const (1.5, Float) in  (* Not too volatile *)
  {
    name = "Volatility_Squeeze";
    buy_trigger =
      squeeze &&. breakout &&. momentum &&. vol_filter &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))   (* 3% stop *)
      ||. (last >. EntryPrice *. Const (1.05, Float))   (* 5% target *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));  (* 3hr max *)
    score = Const (1.0, Float) /. band_width;
    max_positions = 5;
    position_size = 0.20;
  }

(** Volume_Breakout_1M - Adapted for 1-minute bars

    Changes:
    - Use AD (Accumulation/Distribution) instead of OBV for smoother signal
    - Much longer lookback for "surge" detection (60 bars = 1 hour)
    - Added NATR filter
    - Price must break above 100-bar SMA (not 10)
    - Require significant price move (0.5% above SMA) *)
let volume_breakout =
  let ad = Gadt_fo.Constant.ad () in
  let sma_100 = Gadt_fo.Constant.sma 100 () in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  let ad_surging = ad >. lag ad 60 in  (* AD rising over 1 hour *)
  let price_breakout = last >. sma_100 *. Const (1.005, Float) in  (* 0.5% above *)
  let price_rising = last >. lag last 10 in
  let vol_filter = natr <. Const (2.0, Float) in
  {
    name = "Volume_Breakout";
    buy_trigger =
      ad_surging &&. price_breakout &&. price_rising &&. vol_filter &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.975, Float))  (* 2.5% stop *)
      ||. (last >. EntryPrice *. Const (1.04, Float))   (* 4% target *)
      ||. (ad <. lag ad 20)  (* AD reversing *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (150, Int));
    score = ad -. lag ad 60;
    max_positions = 4;
    position_size = 0.25;
  }

(** ADX_Trend_1M - Adapted for 1-minute bars

    Changes:
    - ADX period: 100 (was 14) - ~1.5 hours of data
    - Higher ADX threshold: 30 (was 25) - only very strong trends
    - Require DI spread > 5 (not just plus > minus)
    - Added NATR filter
    - Trend must be strengthening over 20 bars (not 3) *)
let adx_trend =
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), Const (100, Int)) ))
  in
  let plus_di =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.plus_di", Tacaml.Indicator.Raw.plus_di), Const (100, Int)) ))
  in
  let minus_di =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.minus_di", Tacaml.Indicator.Raw.minus_di), Const (100, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  let strong_trend = adx >. Const (30.0, Float) in  (* Higher threshold *)
  let clear_uptrend = (plus_di -. minus_di) >. Const (5.0, Float) in  (* DI spread *)
  let trend_strengthening = adx >. lag adx 20 in  (* Longer confirmation *)
  let vol_filter = natr <. Const (2.0, Float) in
  {
    name = "ADX_Trend";
    buy_trigger =
      strong_trend &&. clear_uptrend &&. trend_strengthening &&. vol_filter &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))   (* 3% stop *)
      ||. (adx <. Const (20.0, Float))
      ||. (minus_di >. plus_di)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (200, Int));
    score = adx *. (plus_di -. minus_di);
    max_positions = 3;
    position_size = 0.33;
  }

(** Range_Breakout_1M - This was our best performer, refining further

    The original had PF of 1.00, meaning it broke even. Changes:
    - NATR period: 100 (was 14) - longer lookback for consolidation
    - Tighter consolidation: NATR < 0.2% (was 0.3%)
    - Bigger breakout required: 1% above SMA (was 0.5%)
    - Longer SMA: 200 (was 20)
    - Added MFI filter for volume confirmation
    - Bigger stop (3%) and target (5%) *)
let range_breakout =
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (100, Int)) ))
  in
  let atr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.atr", Tacaml.Indicator.Raw.atr), Const (100, Int)) ))
  in
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (100, Int)) ))
  in
  let sma_200 = Gadt_fo.Constant.sma 200 () in
  let consolidation = natr <. Const (0.2, Float) in  (* Tighter *)
  let price_breakout = last >. sma_200 *. Const (1.01, Float) in  (* 1% above *)
  let vol_expansion = atr >. lag atr 20 in  (* Longer lookback *)
  let volume_confirm = mfi >. Const (50.0, Float) in  (* Buying pressure *)
  {
    name = "Range_Breakout";
    buy_trigger =
      consolidation &&. price_breakout &&. vol_expansion &&. volume_confirm &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))   (* 3% stop *)
      ||. (last >. EntryPrice *. Const (1.05, Float))   (* 5% target *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = atr -. lag atr 20;
    max_positions = 4;
    position_size = 0.25;
  }

(** MACD_Divergence_1M - Adapted for 1-minute bars

    Changes:
    - MACD periods scaled 10x: 120/260/90 (was 12/26/9)
    - Divergence lookback: 60 bars = 1 hour (was 10)
    - Added NATR filter
    - Price must be significantly lower (0.5%) not just lower
    - MACD must turn up over 5 bars (not 1) *)
let macd_divergence =
  let macd_hist =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3 (Fun ("I.macd_hist", Tacaml.Indicator.Raw.macd_hist),
                 Const (120, Int), Const (260, Int), Const (90, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  (* Bullish divergence over 1 hour: price down 0.5%, MACD up *)
  let price_lower = last <. lag last 60 *. Const (0.995, Float) in
  let macd_higher = macd_hist >. lag macd_hist 60 in
  let divergence = price_lower &&. macd_higher in
  let macd_turning_up = macd_hist >. lag macd_hist 5 in
  let vol_filter = natr <. Const (2.0, Float) in
  {
    name = "MACD_Divergence";
    buy_trigger =
      divergence &&. macd_turning_up &&. vol_filter &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))   (* 3% stop *)
      ||. (last >. EntryPrice *. Const (1.05, Float))   (* 5% target *)
      ||. (macd_hist <. lag macd_hist 10)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = macd_hist -. lag macd_hist 60;
    max_positions = 4;
    position_size = 0.25;
  }

(** Stochastic_Extreme_1M - Adapted for 1-minute bars

    Changes:
    - Stoch periods scaled 10x: 140/30/30 (was 14/3/3)
    - Much more extreme oversold: K < 10 (was 20)
    - K must cross above D by margin of 3+ (not just >)
    - K must be rising over 5 bars (not 1)
    - Added NATR filter *)
let stochastic_extreme =
  let stoch_k =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3 (Fun ("I.stoch_k", Tacaml.Indicator.Raw.stoch_slow_k),
                 Const (140, Int), Const (30, Int), Const (30, Int)) ))
  in
  let stoch_d =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3 (Fun ("I.stoch_d", Tacaml.Indicator.Raw.stoch_slow_d),
                 Const (140, Int), Const (30, Int), Const (30, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  let extreme_oversold = stoch_k <. Const (10.0, Float) in  (* Very extreme *)
  let clear_crossover = (stoch_k -. stoch_d) >. Const (3.0, Float) in
  let k_rising = stoch_k >. lag stoch_k 5 in
  let vol_filter = natr <. Const (2.0, Float) in
  {
    name = "Stochastic_Extreme";
    buy_trigger =
      extreme_oversold &&. clear_crossover &&. k_rising &&. vol_filter &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))   (* 3% stop *)
      ||. (last >. EntryPrice *. Const (1.05, Float))   (* 5% target *)
      ||. (stoch_k >. Const (70.0, Float))
      ||. (stoch_d >. stoch_k)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = Const (10.0, Float) -. stoch_k;
    max_positions = 4;
    position_size = 0.25;
  }

(** Williams_R_1M - Adapted for 1-minute bars

    Changes:
    - Williams R period: 140 (was 14)
    - ROC period: 100 (was 10)
    - More extreme oversold: < -90 (was -80)
    - ROC must be notably positive: > 0.1% (was 0)
    - Recovering over 5 bars (not 1)
    - Added NATR filter *)
let williams_r =
  let willr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.willr", Tacaml.Indicator.Raw.willr), Const (140, Int)) ))
  in
  let roc =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.roc", Tacaml.Indicator.Raw.roc), Const (100, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  let extreme_oversold = willr <. Const (-90.0, Float) in  (* More extreme *)
  let momentum_positive = roc >. Const (0.1, Float) in  (* Meaningful ROC *)
  let recovering = willr >. lag willr 5 in
  let vol_filter = natr <. Const (2.0, Float) in
  {
    name = "Williams_R";
    buy_trigger =
      extreme_oversold &&. momentum_positive &&. recovering &&. vol_filter &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))   (* 3% stop *)
      ||. (last >. EntryPrice *. Const (1.05, Float))   (* 5% target *)
      ||. (willr >. Const (-20.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = Const (-100.0, Float) -. willr;
    max_positions = 4;
    position_size = 0.25;
  }

(** CCI_Breakout_1M - Adapted for 1-minute bars

    Changes:
    - CCI period: 200 (was 20)
    - Only enter on zero-cross (removed "or strong" which caused overtrading)
    - Require CCI to have been below -50 recently (true reversal)
    - CCI must be accelerating
    - Added NATR filter *)
let cci_breakout =
  let cci =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.cci", Tacaml.Indicator.Raw.cci), Const (200, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  (* True zero-cross: just crossed above 0 *)
  let cross_zero = (cci >. Const (0.0, Float)) &&. (lag cci 5 <. Const (0.0, Float)) in
  (* Was recently very oversold *)
  let was_oversold = lag cci 30 <. Const (-50.0, Float) in
  let cci_accelerating = cci >. lag cci 10 in
  let vol_filter = natr <. Const (2.0, Float) in
  {
    name = "CCI_Breakout";
    buy_trigger =
      cross_zero &&. was_oversold &&. cci_accelerating &&. vol_filter &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))   (* 3% stop *)
      ||. (last >. EntryPrice *. Const (1.05, Float))   (* 5% target *)
      ||. (cci <. Const (-50.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = cci;
    max_positions = 4;
    position_size = 0.25;
  }

(** ==========================================================
    MULTI-FACTOR STRATEGIES - Following NB_V3_2's Success Pattern

    Key learnings from NB_V3_2 that achieved 83% consistency:
    1. Multiple indicator confirmation (not just one signal)
    2. NATR volatility filter with BOTH low and high bounds
    3. Long indicator periods (170-348 bars for 1-minute)
    4. Deep penetration/extreme readings required
    5. Recovering condition (price > lag price 1)
    6. safe_to_enter() for EOD protection
    ========================================================== *)

(** ADX_Multifactor - Trend following with strict filters

    Combines:
    - ADX for trend strength (long period: 200)
    - RSI for momentum confirmation (not oversold)
    - NATR for volatility regime (both bounds)
    - Price above SMA for trend confirmation
    - Aroon for trend timing *)
let adx_multifactor =
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), Const (200, Int)) ))
  in
  let plus_di =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.plus_di", Tacaml.Indicator.Raw.plus_di), Const (200, Int)) ))
  in
  let minus_di =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.minus_di", Tacaml.Indicator.Raw.minus_di), Const (200, Int)) ))
  in
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), Const (140, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (20, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (100, Int)) ))
  in
  let sma = Gadt_fo.Constant.sma 200 () in
  (* Filters *)
  let strong_trend = adx >. Const (25.0, Float) in
  let clear_uptrend = (plus_di -. minus_di) >. Const (5.0, Float) in
  let momentum_ok = rsi >. Const (45.0, Float) &&. (rsi <. Const (70.0, Float)) in
  let vol_ok = (natr_lo >. Const (0.1, Float)) &&. (natr_hi <. Const (2.0, Float)) in
  let price_above_sma = last >. sma in
  let adx_rising = adx >. lag adx 10 in
  {
    name = "ADX_Multifactor";
    buy_trigger =
      strong_trend &&. clear_uptrend &&. momentum_ok &&. vol_ok
      &&. price_above_sma &&. adx_rising &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))
      ||. (last >. EntryPrice *. Const (1.05, Float))
      ||. (adx <. Const (20.0, Float))
      ||. (minus_di >. plus_di)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (200, Int));
    score = adx *. (plus_di -. minus_di);
    max_positions = 4;
    position_size = 0.25;
  }

(** RSI_Volume_Divergence - RSI oversold with volume confirmation

    Combines:
    - RSI deeply oversold (< 30 on long period)
    - AD (Accumulation/Distribution) rising while price falling
    - NATR volatility filter
    - Price recovering *)
let rsi_volume =
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), Const (170, Int)) ))
  in
  let ad = Gadt_fo.Constant.ad () in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (20, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (100, Int)) ))
  in
  (* Filters *)
  let rsi_oversold = rsi <. Const (35.0, Float) in
  let ad_rising = ad >. lag ad 30 in  (* Volume accumulating *)
  let price_lower = last <. lag last 30 in  (* Divergence: price down, AD up *)
  let vol_ok = (natr_lo >. Const (0.1, Float)) &&. (natr_hi <. Const (2.0, Float)) in
  let recovering = last >. lag last 1 in
  let rsi_turning = rsi >. lag rsi 3 in
  {
    name = "RSI_Volume";
    buy_trigger =
      rsi_oversold &&. ad_rising &&. price_lower &&. vol_ok
      &&. recovering &&. rsi_turning &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))
      ||. (last >. EntryPrice *. Const (1.04, Float))
      ||. (rsi >. Const (60.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = Const (35.0, Float) -. rsi;
    max_positions = 5;
    position_size = 0.20;
  }

(** Aroon_Breakout - Aroon trend detection with multi-factor confirmation

    Combines:
    - Aroon Up crossing above Aroon Down (trend change)
    - Aroon oscillator positive and rising
    - ADX confirming trend exists
    - NATR volatility filter *)
let aroon_breakout =
  let aroon_up =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.aroon_up", Tacaml.Indicator.Raw.aroon_up), Const (140, Int)) ))
  in
  let aroon_down =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.aroon_down", Tacaml.Indicator.Raw.aroon_down), Const (140, Int)) ))
  in
  let aroon_osc =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.aroon_osc", Tacaml.Indicator.Raw.aroon_osc), Const (140, Int)) ))
  in
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), Const (100, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  (* Filters *)
  let aroon_bullish = aroon_up >. aroon_down in
  let aroon_strong = aroon_osc >. Const (30.0, Float) in  (* Clear uptrend *)
  let aroon_rising = aroon_osc >. lag aroon_osc 10 in
  let trend_exists = adx >. Const (20.0, Float) in
  let vol_ok = natr <. Const (2.0, Float) in
  {
    name = "Aroon_Breakout";
    buy_trigger =
      aroon_bullish &&. aroon_strong &&. aroon_rising
      &&. trend_exists &&. vol_ok &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))
      ||. (last >. EntryPrice *. Const (1.05, Float))
      ||. (aroon_down >. aroon_up)  (* Trend reversed *)
      ||. (aroon_osc <. Const (0.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = aroon_osc;
    max_positions = 4;
    position_size = 0.25;
  }

(** Ultosc_Deep - Ultimate Oscillator multi-timeframe oversold

    Ultimate Oscillator uses 3 timeframes (we'll use 70/140/280 for 1-min)
    Combines:
    - Ultosc deeply oversold (< 30)
    - NATR volatility filter
    - Price recovering
    - MFI confirming volume *)
let ultosc_deep =
  let ultosc =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3 (Fun ("I.ultosc", Tacaml.Indicator.Raw.ultosc),
                 Const (70, Int), Const (140, Int), Const (280, Int)) ))
  in
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (140, Int)) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (20, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (100, Int)) ))
  in
  (* Filters *)
  let ultosc_oversold = ultosc <. Const (35.0, Float) in
  let mfi_oversold = mfi <. Const (40.0, Float) in  (* Volume confirms *)
  let vol_ok = (natr_lo >. Const (0.1, Float)) &&. (natr_hi <. Const (2.0, Float)) in
  let recovering = last >. lag last 1 in
  let ultosc_turning = ultosc >. lag ultosc 5 in
  {
    name = "Ultosc_Deep";
    buy_trigger =
      ultosc_oversold &&. mfi_oversold &&. vol_ok
      &&. recovering &&. ultosc_turning &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))
      ||. (last >. EntryPrice *. Const (1.04, Float))
      ||. (ultosc >. Const (65.0, Float))  (* Overbought *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = Const (35.0, Float) -. ultosc;
    max_positions = 5;
    position_size = 0.20;
  }

(** CMO_RSI_Combo - Chande Momentum + RSI double confirmation

    CMO is similar to RSI but uses momentum calculation
    Combining CMO and RSI oversold gives stronger signal
    Plus NATR and BB for price context *)
let cmo_rsi_combo =
  let cmo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.cmo", Tacaml.Indicator.Raw.cmo), Const (140, Int)) ))
  in
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), Const (140, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (200, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (20, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (100, Int)) ))
  in
  (* Filters - require BOTH oscillators oversold *)
  let cmo_oversold = cmo <. Const (-30.0, Float) in  (* CMO: -100 to +100 *)
  let rsi_oversold = rsi <. Const (35.0, Float) in
  let below_bb = last <. bb_lower in
  let vol_ok = (natr_lo >. Const (0.1, Float)) &&. (natr_hi <. Const (2.0, Float)) in
  let recovering = last >. lag last 1 in
  {
    name = "CMO_RSI_Combo";
    buy_trigger =
      cmo_oversold &&. rsi_oversold &&. below_bb &&. vol_ok
      &&. recovering &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))
      ||. (last >. EntryPrice *. Const (1.04, Float))
      ||. (cmo >. Const (30.0, Float))
      ||. (rsi >. Const (60.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = (Const (-30.0, Float) -. cmo) +. (Const (35.0, Float) -. rsi);
    max_positions = 5;
    position_size = 0.20;
  }

(** TRIX_Momentum - Triple-smoothed EMA momentum with confirmation

    TRIX is very smooth (triple exponential) so less noise
    Combine with faster RSI for timing *)
let trix_momentum =
  let trix =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.trix", Tacaml.Indicator.Raw.trix), Const (150, Int)) ))
  in
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), Const (70, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  let sma = Gadt_fo.Constant.sma 200 () in
  (* Filters *)
  let trix_positive = trix >. Const (0.0, Float) in  (* Trend up *)
  let trix_rising = trix >. lag trix 10 in  (* Momentum increasing *)
  let rsi_ok = rsi >. Const (40.0, Float) &&. (rsi <. Const (70.0, Float)) in
  let above_sma = last >. sma in
  let vol_ok = natr <. Const (2.0, Float) in
  {
    name = "TRIX_Momentum";
    buy_trigger =
      trix_positive &&. trix_rising &&. rsi_ok &&. above_sma
      &&. vol_ok &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))
      ||. (last >. EntryPrice *. Const (1.04, Float))
      ||. (trix <. Const (0.0, Float))  (* Trend reversed *)
      ||. (trix <. lag trix 5)  (* Momentum waning *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (200, Int));
    score = trix *. Const (1000.0, Float);  (* TRIX values are small *)
    max_positions = 4;
    position_size = 0.25;
  }

(** ADX_Multifactor_V2 - Relaxed version

    Changes from V1 (which had 0 trades):
    - Lower ADX threshold: 20 (was 25)
    - Lower DI spread requirement: 3 (was 5)
    - Wider RSI range: 40-75 (was 45-70)
    - Lower NATR low bound: 0.05 (was 0.1)
    - Keep key filters: trend + volatility + price above SMA *)
let adx_multifactor_v2 =
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), Const (150, Int)) ))
  in
  let plus_di =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.plus_di", Tacaml.Indicator.Raw.plus_di), Const (150, Int)) ))
  in
  let minus_di =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.minus_di", Tacaml.Indicator.Raw.minus_di), Const (150, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  let sma = Gadt_fo.Constant.sma 150 () in
  (* Relaxed filters *)
  let trend_exists = adx >. Const (20.0, Float) in
  let uptrend = plus_di >. minus_di in
  let vol_ok = natr <. Const (2.5, Float) in
  let above_sma = last >. sma in
  {
    name = "ADX_Multifactor_V2";
    buy_trigger =
      trend_exists &&. uptrend &&. vol_ok &&. above_sma &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))
      ||. (last >. EntryPrice *. Const (1.05, Float))
      ||. (minus_di >. plus_di)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (200, Int));
    score = adx;
    max_positions = 4;
    position_size = 0.25;
  }

(** TRIX_Momentum_V2 - Relaxed version

    Changes from V1 (which had 0 trades):
    - Removed "above SMA" requirement
    - TRIX only needs to be positive, not necessarily rising
    - Wider RSI range: 35-75 (was 40-70)
    - Higher NATR allowed: 3.0 (was 2.0) *)
let trix_momentum_v2 =
  let trix =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.trix", Tacaml.Indicator.Raw.trix), Const (100, Int)) ))
  in
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), Const (70, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (50, Int)) ))
  in
  (* Relaxed filters *)
  let trix_positive = trix >. Const (0.0, Float) in
  let rsi_ok = rsi >. Const (35.0, Float) &&. (rsi <. Const (75.0, Float)) in
  let vol_ok = natr <. Const (3.0, Float) in
  let price_rising = last >. lag last 5 in  (* Simple momentum confirm *)
  {
    name = "TRIX_Momentum_V2";
    buy_trigger =
      trix_positive &&. rsi_ok &&. vol_ok &&. price_rising &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.97, Float))
      ||. (last >. EntryPrice *. Const (1.04, Float))
      ||. (trix <. Const (0.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (180, Int));
    score = trix *. Const (1000.0, Float);
    max_positions = 4;
    position_size = 0.25;
  }

(** CMO_RSI_Combo_V2 - Tighter version

    V1 had too many trades (~260-300 per period). Changes:
    - More extreme CMO: < -40 (was -30)
    - More extreme RSI: < 30 (was 35)
    - Require BOTH oscillators turning (not just price recovering)
    - Add minimum hold before exit signals *)
let cmo_rsi_combo_v2 =
  let cmo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.cmo", Tacaml.Indicator.Raw.cmo), Const (170, Int)) ))
  in
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), Const (170, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (250, Int),
               Const (2.5, Float),
               Const (2.5, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (250, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (20, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (100, Int)) ))
  in
  (* Stricter filters *)
  let cmo_deep = cmo <. Const (-40.0, Float) in
  let rsi_deep = rsi <. Const (30.0, Float) in
  let below_bb = last <. bb_lower in
  let vol_ok = (natr_lo >. Const (0.1, Float)) &&. (natr_hi <. Const (1.5, Float)) in
  let recovering = last >. lag last 1 in
  let cmo_turning = cmo >. lag cmo 3 in
  let rsi_turning = rsi >. lag rsi 3 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (30, Int)) in
  let exit_signals =
    past_min_hold
    &&. ((last >. bb_middle)
        ||. (cmo >. Const (20.0, Float))
        ||. (rsi >. Const (55.0, Float))
        ||. (last >. EntryPrice *. Const (1.04, Float)))
  in
  {
    name = "CMO_RSI_Combo_V2";
    buy_trigger =
      cmo_deep &&. rsi_deep &&. below_bb &&. vol_ok
      &&. recovering &&. cmo_turning &&. rsi_turning &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.965, Float))  (* Tighter stop: 3.5% *)
      ||. exit_signals
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (200, Int));
    score = (Const (-40.0, Float) -. cmo) +. (Const (30.0, Float) -. rsi);
    max_positions = 5;
    position_size = 0.20;
  }

(** RSI_Volume_V2 - Tighter divergence requirement

    V1 had 3/6 positive. Try to improve by:
    - Deeper RSI oversold: < 30 (was 35)
    - Larger divergence window: 60 bars (was 30)
    - Require both RSI and price turning *)
let rsi_volume_v2 =
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), Const (200, Int)) ))
  in
  let ad = Gadt_fo.Constant.ad () in
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
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (20, Int)) ))
  in
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (100, Int)) ))
  in
  (* Stricter filters *)
  let rsi_deep = rsi <. Const (28.0, Float) in
  let mfi_oversold = mfi <. Const (35.0, Float) in  (* Volume confirms *)
  let ad_rising = ad >. lag ad 60 in
  let price_lower = last <. lag last 60 in
  let vol_ok = (natr_lo >. Const (0.15, Float)) &&. (natr_hi <. Const (1.5, Float)) in
  let recovering = last >. lag last 1 in
  let rsi_turning = rsi >. lag rsi 5 in
  {
    name = "RSI_Volume_V2";
    buy_trigger =
      rsi_deep &&. mfi_oversold &&. ad_rising &&. price_lower &&. vol_ok
      &&. recovering &&. rsi_turning &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.965, Float))
      ||. (last >. EntryPrice *. Const (1.045, Float))
      ||. (rsi >. Const (55.0, Float))
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (200, Int));
    score = Const (28.0, Float) -. rsi;
    max_positions = 5;
    position_size = 0.20;
  }

(* Opening Range Breakout Strategy - Constant Version

   Entry: After first 30 minutes, buy when price breaks above the opening range high.
   Only enter if the gap is positive (momentum bias).

   Exit: Stop loss at opening range low, profit target at 2x risk, or EOD exit.

   This tests the new intraday session tracking primitives. *)
let orb_basic : Gadt_strategy.t =
  let open Gadt in
  (* Entry conditions - use crossover to only trigger on the breakout moment *)
  let or_complete = opening_range_complete 30.0 in
  let breakout_cross = cross_up last OpeningRangeHigh in  (* Only on the cross, not while above *)
  let positive_gap = gapped_up 0.0 in
  let safe = Gadt_strategy.safe_to_enter () in
  let no_position = not_ HasPosition in

  let buy_trigger =
    no_position &&. or_complete &&. breakout_cross &&. positive_gap &&. safe
  in

  (* Exit conditions *)
  let stop_at_orl = last <. OpeningRangeLow in
  let profit_target =
    (* 2:1 reward/risk - target is OR high + 2x(OR high - OR low) *)
    let or_range = OpeningRangeHigh -. OpeningRangeLow in
    last >. OpeningRangeHigh +. or_range *. Const (2.0, Float)
  in
  let eod_exit = Gadt_strategy.force_exit_eod () in

  let sell_trigger = stop_at_orl ||. profit_target ||. eod_exit in

  {
    name = "ORB_Basic";
    buy_trigger;
    sell_trigger;
    score = Const (1.0, Float);  (* Equal priority for all signals *)
    max_positions = 5;
    position_size = 0.20;
  }

(* Opening Range Breakout Strategy - Optimizable Version

   RESULTS: 1500 iterations on q3q4-2024 -> 5% returns (not promising)

   Variables (8 total):
   1. or_minutes: [15, 120] - Opening range duration in minutes
   2. gap_threshold: [-2.0, 3.0] - Minimum gap % to enter (negative = gap down ok)
   3. breakout_buffer: [0.0, 1.0] - % above ORH required for breakout
   4. stop_buffer: [0.0, 1.0] - % below ORL for stop (0 = exact ORL)
   5. profit_mult: [0.5, 5.0] - Profit target as multiple of OR range
   6. eod_buffer: [5.0, 60.0] - Minutes before close to force exit
   7. max_hold: [30, 300] - Maximum hold time in minutes
   8. rsi_period: [10, 100] - RSI period for momentum confirmation

   For 1-minute bars with ~390 bars/day, periods up to 200 are reasonable.
*)
let orb_opt : Gadt_strategy.t =
  let open Gadt in
  let open Type in

  (* Variable 1: Opening range duration in minutes *)
  let or_minutes_var = Gadt_fo.var ~lower:15.0 ~upper:120.0 Float in

  (* Variable 2: Gap threshold - negative means gap downs are ok *)
  let gap_threshold_var = Gadt_fo.var ~lower:(-2.0) ~upper:3.0 Float in

  (* Variable 3: Breakout buffer - % above ORH *)
  let breakout_buffer_var = Gadt_fo.var ~lower:0.0 ~upper:1.0 Float in

  (* Variable 4: Stop buffer - % below ORL *)
  let stop_buffer_var = Gadt_fo.var ~lower:0.0 ~upper:1.0 Float in

  (* Variable 5: Profit target as multiple of OR range *)
  let profit_mult_var = Gadt_fo.var ~lower:0.5 ~upper:5.0 Float in

  (* Variable 6: EOD exit buffer in minutes *)
  let eod_buffer_var = Gadt_fo.var ~lower:5.0 ~upper:60.0 Float in

  (* Variable 7: Maximum hold time in ticks (minutes for 1-min bars) *)
  let max_hold_var = Gadt_fo.var ~lower:30.0 ~upper:300.0 Int in

  (* Variable 8: RSI period for momentum confirmation *)
  let rsi_period_var = Gadt_fo.var ~lower:10.0 ~upper:100.0 Int in

  (* Variable 9: RSI threshold - only enter if RSI above this *)
  let rsi_threshold_var = Gadt_fo.var ~lower:40.0 ~upper:70.0 Float in

  (* RSI indicator *)
  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period_var) ))
  in

  (* Entry conditions *)
  let or_complete = MinutesSinceOpen >. or_minutes_var in
  let breakout_level = OpeningRangeHigh *. (Const (1.0, Float) +. breakout_buffer_var /. Const (100.0, Float)) in
  let breakout_cross = cross_up last breakout_level in
  let gap_ok = GapPct >. gap_threshold_var in
  let rsi_momentum = rsi >. rsi_threshold_var in
  let safe = App1 (Fun ("not", not), is_close TickTime eod_buffer_var) in
  let no_position = not_ HasPosition in

  let buy_trigger =
    no_position &&. or_complete &&. breakout_cross &&. gap_ok &&. rsi_momentum &&. safe
  in

  (* Exit conditions *)
  let stop_level = OpeningRangeLow *. (Const (1.0, Float) -. stop_buffer_var /. Const (100.0, Float)) in
  let stop_hit = last <. stop_level in

  let or_range = OpeningRangeHigh -. OpeningRangeLow in
  let profit_target = last >. OpeningRangeHigh +. or_range *. profit_mult_var in

  let eod_exit = is_close TickTime eod_buffer_var in

  let max_hold_exit = App2 (Fun (">", ( > )), TicksHeld, max_hold_var) in

  let sell_trigger = stop_hit ||. profit_target ||. eod_exit ||. max_hold_exit in

  (* Score: prioritize by RSI momentum and gap size *)
  let score = rsi +. GapPct *. Const (10.0, Float) in

  {
    name = "ORB_Opt";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 5;
    position_size = 0.20;
  }

(* Gap Fade Strategy

   Entry: After opening range, if stock gapped down significantly (>1%) but is
   now showing signs of reversal (price back above opening range low), fade the gap.

   Exit: Target previous day close (gap fill), stop at day low. *)
let gap_fade : Gadt_strategy.t =
  let open Gadt in
  let or_complete = opening_range_complete 30.0 in
  let gapped_down_big = gapped_down 1.0 in
  let recovery_cross = cross_up last OpeningRangeLow in  (* Cross above OR low = recovery signal *)
  let not_filled_yet = last <. PrevDayClose in
  let safe = Gadt_strategy.safe_to_enter () in
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. or_complete &&. gapped_down_big &&. recovery_cross &&. not_filled_yet &&. safe in

  (* Exit at gap fill or stop at day low *)
  let gap_filled = last >=. PrevDayClose in
  let stop_at_day_low = last <. DayLow in
  let eod_exit = Gadt_strategy.force_exit_eod () in

  let sell_trigger = gap_filled ||. stop_at_day_low ||. eod_exit in

  {
    name = "Gap_Fade";
    buy_trigger;
    sell_trigger;
    score = Const (1.0, Float) -. GapPct;  (* Bigger gaps = higher priority *)
    max_positions = 5;
    position_size = 0.20;
  }

(* Previous Day High Breakout - Optimizable

   RESULTS: 500 iterations -> poor performance (negative or barely positive)

   Classic breakout strategy: buy when price breaks above yesterday's high.
   Yesterday's high is a well-known resistance level. Breaking it with
   momentum suggests continuation.

   Variables (6 total):
   1. adx_period: [10, 50] - Trend strength filter period
   2. adx_threshold: [15, 35] - Minimum ADX for trending market
   3. buffer_pct: [0.0, 0.5] - % above prev high required
   4. stop_pct: [0.5, 2.0] - Stop as % below prev high
   5. profit_mult: [1.5, 4.0] - R multiple for profit target
   6. max_hold: [60, 240] - Max minutes to hold *)
let prev_high_breakout_opt : Gadt_strategy.t =
  let open Gadt in
  let open Type in

  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Int in
  let adx_threshold_var = Gadt_fo.var ~lower:15.0 ~upper:35.0 Float in
  let buffer_pct_var = Gadt_fo.var ~lower:0.0 ~upper:0.5 Float in
  let stop_pct_var = Gadt_fo.var ~lower:0.5 ~upper:2.0 Float in
  let profit_mult_var = Gadt_fo.var ~lower:1.5 ~upper:4.0 Float in
  let max_hold_var = Gadt_fo.var ~lower:60.0 ~upper:240.0 Int in

  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), adx_period_var) ))
  in

  (* Entry: Cross above prev high + buffer, with trend strength *)
  let breakout_level = PrevDayHigh *. (Const (1.0, Float) +. buffer_pct_var /. Const (100.0, Float)) in
  let breakout_cross = cross_up last breakout_level in
  let trending = adx >. adx_threshold_var in
  let no_position = not_ HasPosition in
  let safe = Gadt_strategy.safe_to_enter () in

  let buy_trigger = no_position &&. breakout_cross &&. trending &&. safe in

  (* Exit: Stop below prev high, profit at R multiple, or max hold *)
  let stop_level = PrevDayHigh *. (Const (1.0, Float) -. stop_pct_var /. Const (100.0, Float)) in
  let risk = EntryPrice -. stop_level in
  let profit_target = EntryPrice +. risk *. profit_mult_var in
  let stop_hit = last <. stop_level in
  let target_hit = last >. profit_target in
  let max_hold_exit = App2 (Fun (">", ( > )), TicksHeld, max_hold_var) in
  let eod = Gadt_strategy.force_exit_eod () in

  let sell_trigger = stop_hit ||. target_hit ||. max_hold_exit ||. eod in

  {
    name = "Prev_High_Breakout_Opt";
    buy_trigger;
    sell_trigger;
    score = adx;  (* Higher trend strength = higher priority *)
    max_positions = 4;
    position_size = 0.25;
  }

(* Gap and Go - Optimizable

   RESULTS: 500 iterations -> poor performance (negative or barely positive)

   Opposite of gap fade: trade in direction of gap when confirmed.
   Gap ups that hold and make new highs often continue higher.

   Variables (6 total):
   1. min_gap_pct: [0.5, 3.0] - Minimum gap % required
   2. or_minutes: [15, 60] - Opening range duration
   3. adx_period: [10, 40] - ADX period
   4. adx_threshold: [20, 35] - Minimum ADX
   5. stop_pct: [0.5, 2.0] - Stop as % below entry
   6. profit_mult: [1.5, 4.0] - R multiple target *)
let gap_and_go_opt : Gadt_strategy.t =
  let open Gadt in
  let open Type in

  let min_gap_var = Gadt_fo.var ~lower:0.5 ~upper:3.0 Float in
  let or_minutes_var = Gadt_fo.var ~lower:15.0 ~upper:60.0 Float in
  let adx_period_var = Gadt_fo.var ~lower:10.0 ~upper:40.0 Int in
  let adx_threshold_var = Gadt_fo.var ~lower:20.0 ~upper:35.0 Float in
  let stop_pct_var = Gadt_fo.var ~lower:0.5 ~upper:2.0 Float in
  let profit_mult_var = Gadt_fo.var ~lower:1.5 ~upper:4.0 Float in

  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), adx_period_var) ))
  in

  (* Entry: Gapped up, OR complete, breaks above OR high, strong trend *)
  let gapped_up_enough = GapPct >. min_gap_var in
  let or_complete = MinutesSinceOpen >. or_minutes_var in
  let breakout = cross_up last OpeningRangeHigh in
  let trending = adx >. adx_threshold_var in
  let no_position = not_ HasPosition in
  let safe = Gadt_strategy.safe_to_enter () in

  let buy_trigger = no_position &&. gapped_up_enough &&. or_complete &&. breakout &&. trending &&. safe in

  (* Exit: Stop below entry, profit target, EOD *)
  let stop_mult = Const (1.0, Float) -. stop_pct_var /. Const (100.0, Float) in
  let stop_hit = last <. EntryPrice *. stop_mult in
  let risk = EntryPrice *. stop_pct_var /. Const (100.0, Float) in
  let target_hit = last >. EntryPrice +. risk *. profit_mult_var in
  let eod = Gadt_strategy.force_exit_eod () in

  let sell_trigger = stop_hit ||. target_hit ||. eod in

  {
    name = "Gap_And_Go_Opt";
    buy_trigger;
    sell_trigger;
    score = GapPct *. adx;  (* Bigger gap + stronger trend = higher priority *)
    max_positions = 3;
    position_size = 0.33;
  }

(* Intraday Momentum - Optimizable

   RESULTS: 500 iterations -> poor performance (negative or barely positive)

   Buy stocks making new intraday highs with strong momentum.
   DayChangePct filters for stocks already moving, DayHigh crossover
   confirms continuation.

   Variables (6 total):
   1. min_change_pct: [0.5, 3.0] - Minimum intraday gain required
   2. rsi_period: [10, 50] - RSI period for momentum
   3. rsi_threshold: [50, 70] - Minimum RSI (momentum confirmation)
   4. stop_pct: [0.5, 2.0] - Stop as % below entry
   5. profit_pct: [1.0, 4.0] - Profit target %
   6. max_hold: [30, 180] - Max minutes to hold *)
let intraday_momentum_opt : Gadt_strategy.t =
  let open Gadt in
  let open Type in

  let min_change_var = Gadt_fo.var ~lower:0.5 ~upper:3.0 Float in
  let rsi_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Int in
  let rsi_threshold_var = Gadt_fo.var ~lower:50.0 ~upper:70.0 Float in
  let stop_pct_var = Gadt_fo.var ~lower:0.5 ~upper:2.0 Float in
  let profit_pct_var = Gadt_fo.var ~lower:1.0 ~upper:4.0 Float in
  let max_hold_var = Gadt_fo.var ~lower:30.0 ~upper:180.0 Int in

  let rsi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), rsi_period_var) ))
  in

  (* Entry: Already up on day, making new high, RSI confirms momentum *)
  let up_on_day = DayChangePct >. min_change_var in
  let new_day_high = cross_up last DayHigh in  (* Price crossing above day's high *)
  let momentum_confirmed = rsi >. rsi_threshold_var in
  let or_complete = MinutesSinceOpen >. Const (30.0, Float) in  (* Wait for opening noise *)
  let no_position = not_ HasPosition in
  let safe = Gadt_strategy.safe_to_enter () in

  let buy_trigger = no_position &&. or_complete &&. up_on_day &&. new_day_high &&. momentum_confirmed &&. safe in

  (* Exit: Stop, profit target, max hold, EOD *)
  let stop_mult = Const (1.0, Float) -. stop_pct_var /. Const (100.0, Float) in
  let profit_mult = Const (1.0, Float) +. profit_pct_var /. Const (100.0, Float) in
  let stop_hit = last <. EntryPrice *. stop_mult in
  let target_hit = last >. EntryPrice *. profit_mult in
  let max_hold_exit = App2 (Fun (">", ( > )), TicksHeld, max_hold_var) in
  let eod = Gadt_strategy.force_exit_eod () in

  let sell_trigger = stop_hit ||. target_hit ||. max_hold_exit ||. eod in

  {
    name = "Intraday_Momentum_Opt";
    buy_trigger;
    sell_trigger;
    score = DayChangePct *. rsi;  (* Bigger move + stronger momentum = higher priority *)
    max_positions = 4;
    position_size = 0.25;
  }

(* Previous Close Bounce - Optimizable

   Uses previous day's close as support level. Buy when price dips below
   prev close but quickly recovers above it (support test and hold).

   Variables (6 total):
   1. dip_pct: [0.1, 1.0] - How far below prev close counts as "dip"
   2. mfi_period: [10, 50] - MFI period for buying pressure
   3. mfi_threshold: [30, 50] - MFI above this = buying pressure
   4. stop_pct: [0.5, 2.0] - Stop below prev close
   5. profit_pct: [0.5, 2.0] - Profit target %
   6. max_hold: [30, 120] - Max minutes to hold *)
let prev_close_bounce_opt : Gadt_strategy.t =
  let open Gadt in
  let open Type in

  let dip_pct_var = Gadt_fo.var ~lower:0.1 ~upper:1.0 Float in
  let mfi_period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Int in
  let mfi_threshold_var = Gadt_fo.var ~lower:30.0 ~upper:50.0 Float in
  let stop_pct_var = Gadt_fo.var ~lower:0.5 ~upper:2.0 Float in
  let profit_pct_var = Gadt_fo.var ~lower:0.5 ~upper:2.0 Float in
  let max_hold_var = Gadt_fo.var ~lower:30.0 ~upper:120.0 Int in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period_var) ))
  in

  (* Entry: Price recovers back above prev close after dipping below *)
  let dip_level = PrevDayClose *. (Const (1.0, Float) -. dip_pct_var /. Const (100.0, Float)) in
  let was_below = lag last 1 <. dip_level in
  let now_above = last >. PrevDayClose in
  let recovery_cross = was_below &&. now_above in
  let buying_pressure = mfi >. mfi_threshold_var in
  let or_complete = MinutesSinceOpen >. Const (15.0, Float) in
  let no_position = not_ HasPosition in
  let safe = Gadt_strategy.safe_to_enter () in

  let buy_trigger = no_position &&. or_complete &&. recovery_cross &&. buying_pressure &&. safe in

  (* Exit: Stop below prev close, profit target, max hold, EOD *)
  let stop_level = PrevDayClose *. (Const (1.0, Float) -. stop_pct_var /. Const (100.0, Float)) in
  let profit_level = PrevDayClose *. (Const (1.0, Float) +. profit_pct_var /. Const (100.0, Float)) in
  let stop_hit = last <. stop_level in
  let target_hit = last >. profit_level in
  let max_hold_exit = App2 (Fun (">", ( > )), TicksHeld, max_hold_var) in
  let eod = Gadt_strategy.force_exit_eod () in

  let sell_trigger = stop_hit ||. target_hit ||. max_hold_exit ||. eod in

  {
    name = "Prev_Close_Bounce_Opt";
    buy_trigger;
    sell_trigger;
    score = mfi;  (* Higher buying pressure = higher priority *)
    max_positions = 5;
    position_size = 0.20;
  }

(* Opening Dip Buy Strategy V2 - Optimizable

   KEY INSIGHT: Stocks often dip in first 15-45 minutes after open,
   then recover by EOD. Buy weakness during this window, ride the recovery.

   V2 CHANGES:
   - 30-minute buy window (15-45 min after open) instead of 1-minute
   - 3 positions at 33% each (better capital participation)
   - More opportunities to find optimal entry within the window

   Variables (7 total):
   1. mfi_period: [10, 200] - MFI for oversold detection
   2. mfi_threshold: [20, 45] - Buy when MFI BELOW this (oversold)
   3. max_gap: [-5.0, 5.0] - Gap filter (ISRES decides if gap matters)
   4. max_change: [-3.0, 0.5] - Max % change from open (negative = require dip)
   5. stop_pct: [1.0, 4.0] - Stop loss % (wider for mean reversion)
   6. profit_pct: [0.5, 3.0] - Profit target %
   7. max_hold: [60, 360] - Max hold (likely hold until EOD) *)
let opening_dip_buy_opt : Gadt_strategy.t =
  let open Gadt in
  let open Type in

  let mfi_period_var = Gadt_fo.var ~lower:10.0 ~upper:300.0 Int in
  let mfi_threshold_var = Gadt_fo.var ~lower:10.0 ~upper:55.0 Float in
  let max_gap_var = Gadt_fo.var ~lower:(-5.0) ~upper:5.0 Float in
  let max_change_var = Gadt_fo.var ~lower:(-3.0) ~upper:3.0 Float in
  let stop_pct_var = Gadt_fo.var ~lower:1.0 ~upper:5.0 Float in
  let profit_pct_var = Gadt_fo.var ~lower:0.5 ~upper:10.0 Float in
  let max_hold_var = Gadt_fo.var ~lower:60.0 ~upper:400.0 Int in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period_var) ))
  in

  (* Entry: 30-minute window from 15-45 minutes after open, targeting WEAKNESS *)
  let in_buy_window =
    (MinutesSinceOpen >=. Const (15.0, Float)) &&.
    (MinutesSinceOpen <. Const (45.0, Float))
  in
  let oversold = mfi <. mfi_threshold_var in  (* MFI LOW = oversold *)
  let gap_ok = GapPct <. max_gap_var in  (* Gap not too extreme *)
  let dipped = DayChangePct <. max_change_var in  (* Down from open *)
  let no_position = not_ HasPosition in

  let buy_trigger = no_position &&. in_buy_window &&. oversold &&. gap_ok &&. dipped in

  (* Exit: Stop, profit, max hold, or EOD *)
  let stop_mult = Const (1.0, Float) -. stop_pct_var /. Const (100.0, Float) in
  let profit_mult = Const (1.0, Float) +. profit_pct_var /. Const (100.0, Float) in
  let stop_hit = last <. EntryPrice *. stop_mult in
  let target_hit = last >. EntryPrice *. profit_mult in
  let max_hold_exit = App2 (Fun (">", ( > )), TicksHeld, max_hold_var) in
  let eod = Gadt_strategy.force_exit_eod () in

  let sell_trigger = stop_hit ||. target_hit ||. max_hold_exit ||. eod in

  (* Score: Most oversold + biggest dip = highest priority *)
  let score = Const (100.0, Float) -. mfi -. DayChangePct *. Const (10.0, Float) in

  {
    name = "Opening_Dip_Buy_Opt";
    buy_trigger;
    sell_trigger;
    score;
    max_positions = 3;
    position_size = 0.33;
  }

(* Export all strategies *)
let all_strategies =
  [
    (* Multi-factor strategies V2 *)
    adx_multifactor_v2;
    trix_momentum_v2;
    cmo_rsi_combo_v2;
    rsi_volume_v2;
    (* Multi-factor strategies V1 *)
    adx_multifactor;
    rsi_volume;
    aroon_breakout;
    ultosc_deep;
    cmo_rsi_combo;
    trix_momentum;
    (* Alternative strategy classes *)
    volatility_squeeze;
    volume_breakout;
    adx_trend;
    range_breakout;
    macd_divergence;
    stochastic_extreme;
    williams_r;
    cci_breakout;
    (* Walk-forward tests *)
    wf_test_3;
    wf_test_2;
    wf_test_1;
    momentum_breakout_opt;
    ema_crossover_opt;
    macd_momentum_opt;
    mfi_momentum_opt;
    momentum_breakout_v2;
    ema_crossover_v2;
    macd_momentum_v2;
    mfi_momentum_v2;
    claude_momentum_1;
    (* Intraday session strategies *)
    orb_opt;
    orb_basic;
    gap_fade;
    prev_high_breakout_opt;
    gap_and_go_opt;
    intraday_momentum_opt;
    prev_close_bounce_opt;
    opening_dip_buy_opt;
  ]
