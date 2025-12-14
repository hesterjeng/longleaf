(** Volume Momentum Strategies

    Strategies based on the hypothesis that price moves confirmed by volume
    (measured via MFI - Money Flow Index) tend to continue.

    MFI is essentially "volume-weighted RSI" - it combines price direction with
    volume to measure buying/selling pressure on a 0-100 scale. *)

open Gadt
open Gadt_strategy
module Real = Gadt_fo.Constant

(* Re-use helper functions from gadt_strategy *)
let safe_to_enter ?(close_buffer = 10.0) () : bool Gadt.t =
  is_open TickTime
  &&. App1 (Fun ("not", not), is_close TickTime (Const (close_buffer, Float)))

let force_exit_eod ?(close_buffer = 10.0) () : bool Gadt.t =
  is_close TickTime (Const (close_buffer, Float))

let max_holding_time max_ticks : bool Gadt.t =
  let max_ticks_expr = Const (max_ticks, Int) in
  App2 (Fun (">", ( > )), TicksHeld, max_ticks_expr)

let stop_loss stop_loss_pct : bool Gadt.t =
  let multiplier = Float.(1.0 - stop_loss_pct) in
  last <. EntryPrice *. Const (multiplier, Float)

let profit_target profit_target_pct : bool Gadt.t =
  let multiplier = Float.(1.0 + profit_target_pct) in
  last >. EntryPrice *. Const (multiplier, Float)

(** VolumeConfirms_Opt - Volume-Confirmed Momentum Strategy

    HYPOTHESIS: Price moves accompanied by high volume (high MFI) represent
    institutional/informed activity and tend to continue. Price moves on
    low volume are noise.

    MFI (Money Flow Index) is the perfect indicator for this:
    - Range 0-100 like RSI
    - High MFI (>70) = strong buying pressure confirmed by volume
    - Low MFI (<30) = weak/selling pressure
    - Combines price AND volume into one clean signal

    Variables to optimize (4 total):
    1. mfi_period: [5, 50] - MFI lookback period
    2. mfi_entry: [50, 90] - Enter when MFI exceeds this (strong buying pressure)
    3. mfi_exit: [20, 60] - Exit when MFI drops below (pressure fading)
    4. mom_period: [3, 30] - Momentum period for direction confirmation

    Entry Logic:
    - MFI(var1) > var2 (volume-confirmed buying pressure)
    - Momentum(var4) > 0 (price is actually moving up)
    - Safe to enter (not near close)

    Exit Logic:
    - Force exit EOD (no overnight positions)
    - MFI(var1) < var3 (buying pressure fading - the move is over)
    - Momentum turns negative (price direction reversed)
    - 120 tick max hold (allow momentum to play out)

    Note: No stop loss initially - we let MFI exit handle it naturally.
    The hypothesis is that MFI dropping = move exhausted, which IS the exit signal.
    Stop loss can be added in refinement round after seeing raw behavior.

    Position sizing: 10 positions @ 10% each for diversification
*)
let volume_confirms_opt =
  (* Variables with wide bounds for thorough optimization *)
  let mfi_period = Gadt_fo.var ~lower:5.0 ~upper:50.0 Gadt.Type.Int in
  let mfi_entry = Gadt_fo.var ~lower:50.0 ~upper:90.0 Gadt.Type.Float in
  let mfi_exit = Gadt_fo.var ~lower:20.0 ~upper:60.0 Gadt.Type.Float in
  let mom_period = Gadt_fo.var ~lower:3.0 ~upper:30.0 Gadt.Type.Int in

  (* Create MFI indicator with variable period *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period) ))
  in

  (* Create Momentum indicator with variable period *)
  let mom =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mom", Tacaml.Indicator.Raw.mom), mom_period) ))
  in

  {
    name = "VolumeConfirms_Opt";
    (* Entry: Volume-confirmed buying pressure + positive momentum *)
    buy_trigger =
      mfi >. mfi_entry (* Strong buying pressure *)
      &&. (mom >. Const (0.0, Float)) (* Price moving up *)
      &&. safe_to_enter ();
    (* Exit: Buying pressure fades or momentum reverses *)
    sell_trigger =
      force_exit_eod () (* No overnight positions *)
      ||. (mfi <. mfi_exit) (* Buying pressure fading *)
      ||. (mom <. Const (0.0, Float)) (* Momentum reversed *)
      ||. max_holding_time 120;
    (* Allow momentum to play out *)
    (* Score: Higher MFI = stronger signal = higher priority *)
    score = mfi;
    max_positions = 10;
    position_size = 0.10;
  }

(** VolumeConfirms_WithStop_Opt - Volume-Confirmed Momentum with Stop Loss

    Same hypothesis as VolumeConfirms_Opt but adds a stop loss for risk control.
    Use this version after initial optimization to see if stops improve results.

    Variables to optimize (5 total): 1. mfi_period: [5, 50] - MFI lookback
    period 2. mfi_entry: [50, 90] - Enter when MFI exceeds this 3. mfi_exit:
    [20, 60] - Exit when MFI drops below 4. mom_period: [3, 30] - Momentum
    period 5. stop_pct: [0.01, 0.08] - Stop loss percentage (1-8%) *)
let volume_confirms_with_stop_opt =
  let mfi_period = Gadt_fo.var ~lower:5.0 ~upper:50.0 Gadt.Type.Int in
  let mfi_entry = Gadt_fo.var ~lower:50.0 ~upper:90.0 Gadt.Type.Float in
  let mfi_exit = Gadt_fo.var ~lower:20.0 ~upper:60.0 Gadt.Type.Float in
  let mom_period = Gadt_fo.var ~lower:3.0 ~upper:30.0 Gadt.Type.Int in
  let stop_pct = Gadt_fo.var ~lower:0.01 ~upper:0.08 Gadt.Type.Float in

  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period) ))
  in

  let mom =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mom", Tacaml.Indicator.Raw.mom), mom_period) ))
  in

  let stop_mult = Const (1.0, Float) -. stop_pct in

  {
    name = "VolumeConfirms_WithStop_Opt";
    buy_trigger =
      mfi >. mfi_entry &&. (mom >. Const (0.0, Float)) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod () ||. (mfi <. mfi_exit)
      ||. (mom <. Const (0.0, Float))
      ||. (last <. EntryPrice *. stop_mult) (* Variable stop loss *)
      ||. max_holding_time 120;
    score = mfi;
    max_positions = 10;
    position_size = 0.10;
  }

(** Horseman_Opt - MFI-Based Mean Reversion Strategy

    HYPOTHESIS: MFI (Money Flow Index) is "volume-weighted RSI". Low MFI means
    selling pressure is exhausted AND confirmed by volume. This should be a
    higher-quality oversold signal than plain RSI for mean reversion.

    This is a direct test: does replacing RSI with MFI improve mean reversion?

    Variables to optimize (4 total):
    1. mfi_period: [5, 300] - MFI lookback period
    2. mfi_oversold: [15, 40] - Enter when MFI < this (volume-confirmed oversold)
    3. bb_period: [15, 300] - Bollinger Band period
    4. bb_std: [1.0, 3.5] - Bollinger Band standard deviation

    NOTE: Use starting index of at least 350 (-i 350) to allow indicator warmup.

    Entry Logic:
    - MFI(var1) < var2 (volume-confirmed selling exhaustion)
    - Price < Lower BB(var3, var4) (price stretched below mean)
    - Safe to enter (not near close)

    Exit Logic:
    - Force exit EOD (no overnight positions)
    - Price > Middle BB (mean reversion complete)
    - MFI > (100 - mfi_oversold) (symmetric overbought)
    - 2% stop loss (fixed risk control)
    - 5% profit target (fixed)

    Position sizing: 20 positions @ 5% each (wide diversification like estridatter)
*)
let horseman_opt =
  (* Variables with wide bounds for thorough optimization *)
  let mfi_period = Gadt_fo.var ~lower:5.0 ~upper:300.0 Gadt.Type.Int in
  let mfi_oversold = Gadt_fo.var ~lower:15.0 ~upper:40.0 Gadt.Type.Float in
  let bb_period = Gadt_fo.var ~lower:15.0 ~upper:300.0 Gadt.Type.Int in
  let bb_std = Gadt_fo.var ~lower:1.0 ~upper:3.5 Gadt.Type.Float in

  (* Derived: Symmetric MFI overbought threshold *)
  let mfi_overbought = Const (100.0, Float) -. mfi_oversold in

  (* Create MFI indicator with variable period *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period) ))
  in

  (* Create Bollinger Bands with variable period and std *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_period,
               bb_std,
               bb_std ) ))
  in

  {
    name = "Horseman_Opt";
    (* Entry: Volume-confirmed oversold + price below lower BB *)
    buy_trigger =
      mfi <. mfi_oversold (* Selling exhaustion confirmed by volume *)
      &&. (last <. bb_lower) (* Price stretched below mean *)
      &&. safe_to_enter ();
    (* Exit: Mean reversion complete or risk controls *)
    sell_trigger =
      force_exit_eod () (* No overnight positions *)
      ||. (last >. bb_middle) (* Mean reversion complete *)
      ||. (mfi >. mfi_overbought) (* Symmetric overbought exit *)
      ||. stop_loss 0.02 (* Fixed 2% stop *)
      ||. profit_target 0.05;
    (* Fixed 5% target *)
    (* Score: Lower MFI = more oversold = higher priority *)
    score = Const (100.0, Float) -. mfi;
    max_positions = 20;
    position_size = 0.05;
  }

(** Horseman_Opt_R2 - Round 2 Optimization with Risk Management Variables

    Builds on Round 1 winning parameters:
    - mfi_period = 214 (LOCKED)
    - mfi_oversold = 36.01 (LOCKED)
    - bb_period = 258 (LOCKED)
    - bb_std = 1.185 (LOCKED)

    New variables to optimize (4 total): 1. stop_loss_pct: [0.005, 0.08] - Stop
    loss percentage (0.5-8%) 2. profit_target_pct: [0.01, 0.15] - Profit target
    percentage (1-15%) 3. mfi_exit: [50, 85] - Exit when MFI exceeds this
    (overbought exit) 4. max_hold: [20, 390] - Maximum holding time in ticks (20
    min to full day)

    NOTE: Use starting index of at least 350 (-i 350) to allow indicator warmup.
*)
let horseman_opt_r2 =
  (* Round 1 winning parameters - LOCKED *)
  let mfi_period_locked = 214 in
  let mfi_oversold_locked = 36.01 in
  let bb_period_locked = 258 in
  let bb_std_locked = 1.185 in

  (* Round 2 variables for risk management refinement *)
  let stop_loss_pct = Gadt_fo.var ~lower:0.005 ~upper:0.08 Gadt.Type.Float in
  let profit_target_pct = Gadt_fo.var ~lower:0.01 ~upper:0.15 Gadt.Type.Float in
  let mfi_exit = Gadt_fo.var ~lower:50.0 ~upper:85.0 Gadt.Type.Float in
  let max_hold = Gadt_fo.var ~lower:20.0 ~upper:390.0 Gadt.Type.Int in

  (* Create MFI indicator with locked period *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1
             ( Fun ("I.mfi", Tacaml.Indicator.Raw.mfi),
               Const (mfi_period_locked, Int) ) ))
  in

  (* Create Bollinger Bands with locked parameters *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (bb_period_locked, Int),
               Const (bb_std_locked, Float),
               Const (bb_std_locked, Float) ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (bb_period_locked, Int),
               Const (bb_std_locked, Float),
               Const (bb_std_locked, Float) ) ))
  in

  (* Derived expressions for exits *)
  let stop_mult = Const (1.0, Float) -. stop_loss_pct in
  let profit_mult = Const (1.0, Float) +. profit_target_pct in

  {
    name = "Horseman_Opt_R2";
    (* Entry: Volume-confirmed oversold + price below lower BB (same as R1) *)
    buy_trigger =
      mfi
      <. Const (mfi_oversold_locked, Float)
         (* Selling exhaustion confirmed by volume *)
      &&. (last <. bb_lower) (* Price stretched below mean *)
      &&. safe_to_enter ();
    (* Exit: Now with variable risk management parameters *)
    sell_trigger =
      force_exit_eod () (* No overnight positions *)
      ||. (last >. bb_middle) (* Mean reversion complete *)
      ||. (mfi >. mfi_exit) (* Variable overbought exit *)
      ||. (last <. EntryPrice *. stop_mult) (* Variable stop loss *)
      ||. (last >. EntryPrice *. profit_mult) (* Variable profit target *)
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    (* Variable max hold *)
    (* Score: Lower MFI = more oversold = higher priority *)
    score = Const (100.0, Float) -. mfi;
    max_positions = 20;
    position_size = 0.05;
  }

(** Horseman_R2 - Staged Optimization Winner (R1 + R2)

    Concrete strategy with winning parameters from staged optimization.

    R1 Results (entry parameters):
    - mfi_period = 214
    - mfi_oversold = 36.01
    - bb_period = 258
    - bb_std = 1.185

    R2 Results (exit/risk parameters):
    - mfi_exit = 54.04
    - stop_loss_pct = 7.1%
    - profit_target_pct = 11.86%
    - max_hold = 229 ticks

    Performance (3-month backtest Sep-Nov 2024):
    - Return: 6.11% ($100k -> $106,110)
    - Win Rate: 65.68% (530W / 277L)
    - Trades: 807
    - Profit Factor: 1.582
    - Sharpe: 0.157
    - P-value: 0.0000 (statistically significant edge) *)
let horseman_r2 =
  (* Entry MFI *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (214, Int)) ))
  in

  (* Bollinger Bands *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (258, Int),
               Const (1.185, Float),
               Const (1.185, Float) ) ))
  in

  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (258, Int),
               Const (1.185, Float),
               Const (1.185, Float) ) ))
  in

  {
    name = "Horseman_R2";
    buy_trigger =
      mfi <. Const (36.01, Float) &&. (last <. bb_lower) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod () ||. (last >. bb_middle)
      ||. (mfi >. Const (54.04, Float))
      ||. (last <. EntryPrice *. Const (0.929, Float)) (* 7.1% stop *)
      ||. (last >. EntryPrice *. Const (1.1186, Float)) (* 11.86% target *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (229, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 20;
    position_size = 0.05;
  }

(** Free_Horseman_Opt - Full 10-Variable Optimization

    Optimizes all parameters simultaneously without staged locking. Entry and
    exit signals are fully independent.

    Variables to optimize (10 total):

    Entry parameters (4): 1. mfi_entry_period: [5, 300] - MFI lookback period
    for entry 2. mfi_oversold: [15, 40] - Enter when MFI < this threshold 3.
    bb_entry_period: [15, 300] - Bollinger Band period for entry 4.
    bb_entry_std: [1.0, 3.5] - Bollinger Band std dev for entry

    Exit parameters (6): 5. mfi_exit_period: [5, 300] - Separate MFI period for
    exit 6. mfi_exit_level: [50, 85] - Exit when MFI > this threshold 7.
    bb_exit_period: [15, 300] - Separate BB period for exit (middle band) 8.
    stop_loss_pct: [0.005, 0.08] - Stop loss percentage (0.5-8%) 9.
    profit_target_pct: [0.01, 0.15] - Profit target percentage (1-15%) 10.
    max_hold: [20, 390] - Maximum holding time in ticks

    NOTE: Use starting index of at least 350 (-i 350) to allow indicator warmup.
    NOTE: Use higher maxeval (6000-8000) for reliable convergence with 10
    variables. *)
let free_horseman_opt =
  (* Entry parameters *)
  let mfi_entry_period = Gadt_fo.var ~lower:5.0 ~upper:300.0 Gadt.Type.Int in
  let mfi_oversold = Gadt_fo.var ~lower:15.0 ~upper:40.0 Gadt.Type.Float in
  let bb_entry_period = Gadt_fo.var ~lower:15.0 ~upper:300.0 Gadt.Type.Int in
  let bb_entry_std = Gadt_fo.var ~lower:1.0 ~upper:3.5 Gadt.Type.Float in

  (* Exit parameters *)
  let mfi_exit_period = Gadt_fo.var ~lower:5.0 ~upper:300.0 Gadt.Type.Int in
  let mfi_exit_level = Gadt_fo.var ~lower:50.0 ~upper:85.0 Gadt.Type.Float in
  let bb_exit_period = Gadt_fo.var ~lower:15.0 ~upper:300.0 Gadt.Type.Int in
  let stop_loss_pct = Gadt_fo.var ~lower:0.005 ~upper:0.08 Gadt.Type.Float in
  let profit_target_pct = Gadt_fo.var ~lower:0.01 ~upper:0.15 Gadt.Type.Float in
  let max_hold = Gadt_fo.var ~lower:20.0 ~upper:390.0 Gadt.Type.Int in

  (* Entry MFI indicator *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_entry_period) ))
  in

  (* Exit MFI indicator - independent period *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_exit_period) ))
  in

  (* Entry Bollinger Bands *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_entry_period,
               bb_entry_std,
               bb_entry_std ) ))
  in

  (* Exit Bollinger Band - independent period *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_exit_period,
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Derived expressions *)
  let stop_mult = Const (1.0, Float) -. stop_loss_pct in
  let profit_mult = Const (1.0, Float) +. profit_target_pct in

  {
    name = "Free_Horseman_Opt";
    (* Entry: MFI oversold + price below lower BB *)
    buy_trigger =
      mfi_entry <. mfi_oversold &&. (last <. bb_lower) &&. safe_to_enter ();
    (* Exit: Independent MFI and BB signals + risk controls *)
    sell_trigger =
      force_exit_eod () ||. (last >. bb_middle)
      ||. (mfi_exit >. mfi_exit_level)
      ||. (last <. EntryPrice *. stop_mult)
      ||. (last >. EntryPrice *. profit_mult)
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    (* Score: Lower entry MFI = more oversold = higher priority *)
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 20;
    position_size = 0.05;
  }

(** Free_Horseman - 10-Variable Optimization Winner

    Concrete strategy from full 10D optimization (no staged locking).

    Parameters found:
    - mfi_entry_period = 252
    - mfi_oversold = 37.47
    - bb_entry_period = 95
    - bb_entry_std = 1.544
    - mfi_exit_period = 274
    - mfi_exit_level = 57.88
    - bb_exit_period = 166
    - stop_loss_pct = 2.04%
    - profit_target_pct = 12.47%
    - max_hold = 217 ticks

    Performance (3-month backtest Sep-Nov 2024):
    - Return: 5.90% ($100k -> $105,900)
    - Win Rate: 63.87% (700W / 396L)
    - Trades: 1096
    - Profit Factor: 1.466
    - Sharpe: 0.129
    - P-value: 0.0000 (statistically significant edge)

    Note: Underperformed staged optimization (Horseman_R2) despite more
    variables. Tighter stop loss (2% vs 7.1%) led to more trades but lower win
    rate and profit factor. *)
let free_horseman =
  (* Entry MFI - period 252 *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (252, Int)) ))
  in

  (* Exit MFI - independent period 274 *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (274, Int)) ))
  in

  (* Entry Bollinger Band - period 95, std 1.544 *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (95, Int),
               Const (1.544, Float),
               Const (1.544, Float) ) ))
  in

  (* Exit Bollinger Band - independent period 166 *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (166, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  {
    name = "Free_Horseman";
    buy_trigger =
      mfi_entry
      <. Const (37.47, Float)
      &&. (last <. bb_lower) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod () ||. (last >. bb_middle)
      ||. (mfi_exit >. Const (57.88, Float))
      ||. (last <. EntryPrice *. Const (0.9796, Float)) (* 2.04% stop *)
      ||. (last >. EntryPrice *. Const (1.1247, Float)) (* 12.47% target *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (217, Int));
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 20;
    position_size = 0.05;
  }

(** Free_Horseman_V2 - Second 10-Variable Optimization Run

    Parameters found:
    - mfi_entry_period = 214
    - mfi_oversold = 37.60
    - bb_entry_period = 278
    - bb_entry_std = 1.536
    - mfi_exit_period = 55
    - mfi_exit_level = 64.00
    - bb_exit_period = 292
    - stop_loss_pct = 6.11%
    - profit_target_pct = 10.88%
    - max_hold = 369 ticks

    Performance (3-month backtest Sep-Nov 2024):
    - Return: 6.13% ($100k -> $106,126)
    - Win Rate: 63.15% (653W / 381L)
    - Trades: 1034
    - Profit Factor: 1.430
    - Sharpe: 0.127
    - P-value: 0.0000 (statistically significant edge)

    Note: Entry params converged toward staged solution (mfi=214, bb≈278). Exit
    params diverged significantly from Run 1 (mfi_exit_period 55 vs 274). *)
let free_horseman_v2 =
  (* Entry MFI - period 214 (same as staged!) *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (214, Int)) ))
  in

  (* Exit MFI - very short period 55 *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (55, Int)) ))
  in

  (* Entry Bollinger Band - period 278, std 1.536 *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (278, Int),
               Const (1.536, Float),
               Const (1.536, Float) ) ))
  in

  (* Exit Bollinger Band - period 292 *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (292, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  {
    name = "Free_Horseman_V2";
    buy_trigger =
      mfi_entry
      <. Const (37.60, Float)
      &&. (last <. bb_lower) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod () ||. (last >. bb_middle)
      ||. (mfi_exit >. Const (64.00, Float))
      ||. (last <. EntryPrice *. Const (0.9389, Float)) (* 6.11% stop *)
      ||. (last >. EntryPrice *. Const (1.1088, Float)) (* 10.88% target *)
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (369, Int));
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 20;
    position_size = 0.05;
  }

(** Horseman_Exit_Explorer - Deep Exit Optimization with Min Hold

    Hypothesis: Entry timing is solved (consensus from multiple runs). Exit
    timing is noisy. Adding min_hold may prevent premature exits and let mean
    reversion complete.

    Locked entry (R1 consensus):
    - mfi_entry_period = 214
    - mfi_oversold = 36.0
    - bb_entry_period = 258
    - bb_entry_std = 1.185

    Optimized exit (7 variables): 1. mfi_exit_period: [5, 300] - Exit MFI
    lookback 2. mfi_exit_level: [50, 85] - Exit when MFI > this 3.
    bb_exit_period: [15, 300] - Exit BB middle period 4. stop_loss_pct:
    [0.02, 0.10] - Stop loss (always active) 5. profit_target_pct: [0.03, 0.15]
    \- Profit target 6. min_hold: [5, 60] - Minimum ticks before exit signals
    activate 7. max_hold: [100, 390] - Maximum holding time

    Exit logic:
    - Stop loss and EOD: always active (risk control)
    - BB middle, MFI exit, profit target: only after min_hold ticks
    - Max hold: always applies

    NOTE: Use starting index of at least 350 (-i 350). NOTE: Use maxeval 10000
    for 7 variables. *)
let horseman_exit_explorer =
  (* Locked entry parameters - R1 consensus *)
  let mfi_entry_period = 214 in
  let mfi_oversold_val = 36.0 in
  let bb_entry_period = 258 in
  let bb_entry_std = 1.185 in

  (* Optimized exit parameters *)
  let mfi_exit_period = Gadt_fo.var ~lower:5.0 ~upper:300.0 Gadt.Type.Int in
  let mfi_exit_level = Gadt_fo.var ~lower:50.0 ~upper:85.0 Gadt.Type.Float in
  let bb_exit_period = Gadt_fo.var ~lower:15.0 ~upper:300.0 Gadt.Type.Int in
  let stop_loss_pct = Gadt_fo.var ~lower:0.02 ~upper:0.10 Gadt.Type.Float in
  let profit_target_pct = Gadt_fo.var ~lower:0.03 ~upper:0.15 Gadt.Type.Float in
  let min_hold = Gadt_fo.var ~lower:5.0 ~upper:60.0 Gadt.Type.Int in
  let max_hold = Gadt_fo.var ~lower:100.0 ~upper:390.0 Gadt.Type.Int in

  (* Entry MFI - locked *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1
             ( Fun ("I.mfi", Tacaml.Indicator.Raw.mfi),
               Const (mfi_entry_period, Int) ) ))
  in

  (* Exit MFI - variable period *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_exit_period) ))
  in

  (* Entry Bollinger Band - locked *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (bb_entry_period, Int),
               Const (bb_entry_std, Float),
               Const (bb_entry_std, Float) ) ))
  in

  (* Exit Bollinger Band - variable period *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_exit_period,
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Derived expressions *)
  let stop_mult = Const (1.0, Float) -. stop_loss_pct in
  let profit_mult = Const (1.0, Float) +. profit_target_pct in

  (* Min hold gate: TicksHeld >= min_hold *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, min_hold) in

  (* Gated exit signals - only trigger after min_hold *)
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi_exit >. mfi_exit_level)
        ||. (last >. EntryPrice *. profit_mult))
  in

  {
    name = "Horseman_Exit_Explorer";
    buy_trigger =
      mfi_entry
      <. Const (mfi_oversold_val, Float)
      &&. (last <. bb_lower) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod () (* Always active *)
      ||. (last <. EntryPrice *. stop_mult) (* Stop loss always active *)
      ||. gated_exits (* BB/MFI/profit after min_hold *)
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    (* Max hold always applies *)
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 20;
    position_size = 0.05;
  }

(** Horseman_Exit_Winner - Best Risk-Adjusted Strategy

    Winner from Exit Explorer optimization. Introduced min_hold concept which
    improved profit factor and Sharpe ratio by preventing premature exits.

    Entry (locked R1 consensus):
    - mfi_entry_period = 214
    - mfi_oversold = 36.0
    - bb_entry_period = 258
    - bb_entry_std = 1.185

    Exit (optimized):
    - mfi_exit_period = 183
    - mfi_exit_level = 51.15
    - bb_exit_period = 293
    - stop_loss_pct = 7.75%
    - profit_target_pct = 13.09%
    - min_hold = 34 ticks (~34 min before exit signals activate)
    - max_hold = 364 ticks

    Performance (3-month backtest Sep-Nov 2024):
    - Return: 6.03% ($100k -> $106,027)
    - Win Rate: 63.68% (491W / 280L)
    - Trades: 771 (fewest of all variants)
    - Profit Factor: 1.643 (BEST)
    - Sharpe: 0.169 (BEST)
    - Avg per trade: $7.59 (BEST)
    - P-value: 0.0000

    Key insight: 34-minute min_hold lets mean reversion complete before taking
    profits, reducing premature exits. *)
let horseman_exit_winner =
  (* Entry MFI - locked *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (214, Int)) ))
  in

  (* Exit MFI - period 183 *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (183, Int)) ))
  in

  (* Entry Bollinger Band - locked *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (258, Int),
               Const (1.185, Float),
               Const (1.185, Float) ) ))
  in

  (* Exit Bollinger Band - period 293 *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (293, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Min hold gate: TicksHeld >= 34 *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (34, Int)) in

  (* Gated exit signals *)
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi_exit >. Const (51.15, Float))
        ||. (last >. EntryPrice *. Const (1.1309, Float)) (* 13.09% target *))
  in

  {
    name = "Horseman_Exit_Winner";
    buy_trigger =
      mfi_entry
      <. Const (36.0, Float)
      &&. (last <. bb_lower) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.9225, Float)) (* 7.75% stop *)
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (364, Int));
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 20;
    position_size = 0.05;
  }

(* Exit Winner 10x10 - 10 positions at 10% each *)
let horseman_exit_winner_10x10 =
  (* Entry MFI - locked *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (214, Int)) ))
  in
  (* Exit MFI - period 183 *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (183, Int)) ))
  in
  (* Entry Bollinger Band - locked *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (258, Int),
               Const (1.185, Float),
               Const (1.185, Float) ) ))
  in
  (* Exit Bollinger Band - period 293 *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (293, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (34, Int)) in
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi_exit >. Const (51.15, Float))
        ||. (last >. EntryPrice *. Const (1.1309, Float)))
  in
  {
    name = "Horseman_Exit_Winner_10x10";
    buy_trigger =
      mfi_entry
      <. Const (36.0, Float)
      &&. (last <. bb_lower) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.9225, Float))
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (364, Int));
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 10;
    position_size = 0.10;
  }

(* Exit Winner 5x20 - 5 positions at 20% each *)
let horseman_exit_winner_5x20 =
  (* Entry MFI - locked *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (214, Int)) ))
  in
  (* Exit MFI - period 183 *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (183, Int)) ))
  in
  (* Entry Bollinger Band - locked *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (258, Int),
               Const (1.185, Float),
               Const (1.185, Float) ) ))
  in
  (* Exit Bollinger Band - period 293 *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (293, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (34, Int)) in
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi_exit >. Const (51.15, Float))
        ||. (last >. EntryPrice *. Const (1.1309, Float)))
  in
  {
    name = "Horseman_Exit_Winner_5x20";
    buy_trigger =
      mfi_entry
      <. Const (36.0, Float)
      &&. (last <. bb_lower) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.9225, Float))
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (364, Int));
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 5;
    position_size = 0.20;
  }

(** Cowboy_Opt - Comprehensive 12-Variable Optimization with ADX Regime Filter

    Full optimization over entry, exit, risk, and regime parameters. Includes
    ADX filter to avoid trending markets where mean reversion fails.

    Entry parameters (4): 1. mfi_entry_period: [50, 250] - MFI lookback 2.
    mfi_oversold: [20, 40] - Entry threshold 3. bb_entry_period: [50, 300] - BB
    period for entry 4. bb_entry_std: [1.0, 2.5] - BB std dev (how extreme)

    Regime filter (2): 5. adx_period: [10, 30] - ADX lookback 6. adx_threshold:
    [15, 35] - Only enter when ADX < this (ranging market)

    Exit parameters (3): 7. mfi_exit_level: [45, 75] - Exit when MFI > this 8.
    bb_exit_period: [50, 300] - BB period for exit 9. min_hold: [10, 60] -
    Minimum ticks before exit signals activate

    Risk parameters (3): 10. stop_loss_pct: [0.02, 0.10] - Stop loss 11.
    profit_target_pct: [0.03, 0.15] - Profit target 12. max_hold: [100, 390] -
    Maximum holding time

    Entry logic:
    - MFI < mfi_oversold (volume-confirmed oversold)
    - Price < BB_lower (price stretched below mean)
    - ADX < adx_threshold (ranging market, not trending)
    - Price > lag(Price, 1) (starting to recover, not falling knife)

    NOTE: Use starting index of at least 350 (-i 350). NOTE: Use maxeval 10000+
    for 12 variables. *)
let cowboy_opt =
  (* Entry parameters *)
  let mfi_entry_period = Gadt_fo.var ~lower:50.0 ~upper:250.0 Gadt.Type.Int in
  let mfi_oversold = Gadt_fo.var ~lower:20.0 ~upper:40.0 Gadt.Type.Float in
  let bb_entry_period = Gadt_fo.var ~lower:50.0 ~upper:300.0 Gadt.Type.Int in
  let bb_entry_std = Gadt_fo.var ~lower:1.0 ~upper:2.5 Gadt.Type.Float in

  (* Regime filter *)
  let adx_period = Gadt_fo.var ~lower:10.0 ~upper:30.0 Gadt.Type.Int in
  let adx_threshold = Gadt_fo.var ~lower:15.0 ~upper:35.0 Gadt.Type.Float in

  (* Exit parameters *)
  let mfi_exit_level = Gadt_fo.var ~lower:45.0 ~upper:75.0 Gadt.Type.Float in
  let bb_exit_period = Gadt_fo.var ~lower:50.0 ~upper:300.0 Gadt.Type.Int in
  let min_hold = Gadt_fo.var ~lower:10.0 ~upper:60.0 Gadt.Type.Int in

  (* Risk parameters *)
  let stop_loss_pct = Gadt_fo.var ~lower:0.02 ~upper:0.10 Gadt.Type.Float in
  let profit_target_pct = Gadt_fo.var ~lower:0.03 ~upper:0.15 Gadt.Type.Float in
  let max_hold = Gadt_fo.var ~lower:100.0 ~upper:390.0 Gadt.Type.Int in

  (* Entry MFI *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_entry_period) ))
  in

  (* Exit MFI - use same period as entry, just different threshold *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_entry_period) ))
  in

  (* ADX for regime filter *)
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), adx_period) ))
  in

  (* Entry Bollinger Band *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_entry_period,
               bb_entry_std,
               bb_entry_std ) ))
  in

  (* Exit Bollinger Band *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_exit_period,
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Recovery filter: current price > previous price *)
  let recovering = last >. lag last 1 in

  (* Derived expressions *)
  let stop_mult = Const (1.0, Float) -. stop_loss_pct in
  let profit_mult = Const (1.0, Float) +. profit_target_pct in

  (* Min hold gate *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, min_hold) in

  (* Gated exit signals *)
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi_exit >. mfi_exit_level)
        ||. (last >. EntryPrice *. profit_mult))
  in

  {
    name = "Cowboy_Opt";
    buy_trigger =
      mfi_entry <. mfi_oversold (* Volume-confirmed oversold *)
      &&. (last <. bb_lower) (* Price below BB lower *)
      &&. (adx <. adx_threshold) (* Ranging market, not trending *)
      &&. recovering (* Price starting to recover *) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_mult) (* Stop loss always active *)
      ||. gated_exits (* Other exits after min_hold *)
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 20;
    position_size = 0.05;
  }

(** ADX_Cowboy - First Cowboy Optimization Result

    12-variable optimization with ADX regime filter. Suspicious result - $1002
    max winner (38x average) may indicate luck.

    Parameters:
    - mfi_entry_period = 195
    - mfi_oversold = 39.85 (higher than Horseman's 36 - less extreme)
    - bb_entry_period = 263
    - bb_entry_std = 1.17
    - adx_period = 28
    - adx_threshold = 23.17
    - mfi_exit_level = 50.04
    - bb_exit_period = 242
    - min_hold = 47
    - stop_loss_pct = 4.64%
    - profit_target_pct = 12.25%
    - max_hold = 321

    Performance (training data):
    - Return: 6.73% ($100k -> $106,732)
    - Win Rate: 60.66% (805W / 522L)
    - Trades: 1327
    - Profit Factor: 1.419
    - Sharpe: 0.102
    - Max Winner: $1002.76 (outlier concern)
    - P-value: 0.0001

    Note: Lower risk-adjusted metrics than Horseman Exit Winner. Testing on
    out-of-sample to see if ADX filter helps generalization. *)
let adx_cowboy =
  (* Entry MFI - period 195 *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (195, Int)) ))
  in

  (* ADX for regime filter - period 28 *)
  let adx =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.adx", Tacaml.Indicator.Raw.adx), Const (28, Int)) ))
  in

  (* Entry Bollinger Band - period 263, std 1.17 *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (263, Int),
               Const (1.17, Float),
               Const (1.17, Float) ) ))
  in

  (* Exit Bollinger Band - period 242 *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (242, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Recovery filter *)
  let recovering = last >. lag last 1 in

  (* Min hold gate: >= 47 ticks *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (47, Int)) in

  (* Gated exits *)
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (50.04, Float))
        ||. (last >. EntryPrice *. Const (1.1225, Float)) (* 12.25% target *))
  in

  {
    name = "ADX_Cowboy";
    buy_trigger =
      mfi
      <. Const (39.85, Float)
      &&. (last <. bb_lower)
      &&. (adx <. Const (23.17, Float))
      &&. recovering &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.9536, Float)) (* 4.64% stop *)
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (321, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 20;
    position_size = 0.05;
  }

(** Nature_Boy_Opt - 12-Variable Optimization with NATR Volatility Filter

    Uses NATR (Normalized ATR) instead of ADX to filter out high-volatility
    periods. NATR directly measures price movement magnitude as a percentage,
    which should better capture the chaos of Mar-Aug 2025 tariff period.

    Hypothesis: Low NATR = calm market = mean reversion works High NATR =
    volatile/chaotic = stay out

    Entry parameters (4): 1. mfi_entry_period: [50, 250] - MFI lookback 2.
    mfi_oversold: [20, 40] - Entry threshold 3. bb_entry_period: [50, 300] - BB
    period for entry 4. bb_entry_std: [1.0, 2.5] - BB std dev (how extreme)

    Volatility filter (2): 5. natr_period: [10, 30] - NATR lookback 6.
    natr_threshold: [1.0, 5.0] - Only enter when NATR < this (calm market)

    Exit parameters (3): 7. mfi_exit_level: [45, 75] - Exit when MFI > this 8.
    bb_exit_period: [50, 300] - BB period for exit 9. min_hold: [10, 60] -
    Minimum ticks before exit signals activate

    Risk parameters (3): 10. stop_loss_pct: [0.02, 0.10] - Stop loss 11.
    profit_target_pct: [0.03, 0.15] - Profit target 12. max_hold: [100, 390] -
    Maximum holding time

    Entry logic:
    - MFI < mfi_oversold (volume-confirmed oversold)
    - Price < BB_lower (price stretched below mean)
    - NATR < natr_threshold (calm market, not chaotic)
    - Price > lag(Price, 1) (starting to recover, not falling knife)

    NOTE: Use starting index of at least 350 (-i 350). NOTE: Use maxeval 10000+
    for 12 variables. *)
let nature_boy_opt =
  (* Entry parameters *)
  let mfi_entry_period = Gadt_fo.var ~lower:50.0 ~upper:250.0 Gadt.Type.Int in
  let mfi_oversold = Gadt_fo.var ~lower:20.0 ~upper:40.0 Gadt.Type.Float in
  let bb_entry_period = Gadt_fo.var ~lower:50.0 ~upper:300.0 Gadt.Type.Int in
  let bb_entry_std = Gadt_fo.var ~lower:1.0 ~upper:2.5 Gadt.Type.Float in

  (* Volatility filter - NATR *)
  let natr_period = Gadt_fo.var ~lower:10.0 ~upper:30.0 Gadt.Type.Int in
  let natr_threshold = Gadt_fo.var ~lower:1.0 ~upper:5.0 Gadt.Type.Float in

  (* Exit parameters *)
  let mfi_exit_level = Gadt_fo.var ~lower:45.0 ~upper:75.0 Gadt.Type.Float in
  let bb_exit_period = Gadt_fo.var ~lower:50.0 ~upper:300.0 Gadt.Type.Int in
  let min_hold = Gadt_fo.var ~lower:10.0 ~upper:60.0 Gadt.Type.Int in

  (* Risk parameters *)
  let stop_loss_pct = Gadt_fo.var ~lower:0.02 ~upper:0.10 Gadt.Type.Float in
  let profit_target_pct = Gadt_fo.var ~lower:0.03 ~upper:0.15 Gadt.Type.Float in
  let max_hold = Gadt_fo.var ~lower:100.0 ~upper:390.0 Gadt.Type.Int in

  (* Entry MFI *)
  let mfi_entry =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_entry_period) ))
  in

  (* Exit MFI - use same period as entry, just different threshold *)
  let mfi_exit =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_entry_period) ))
  in

  (* NATR for volatility filter *)
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), natr_period) ))
  in

  (* Entry Bollinger Band *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               bb_entry_period,
               bb_entry_std,
               bb_entry_std ) ))
  in

  (* Exit Bollinger Band *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               bb_exit_period,
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Recovery filter: current price > previous price *)
  let recovering = last >. lag last 1 in

  (* Derived expressions *)
  let stop_mult = Const (1.0, Float) -. stop_loss_pct in
  let profit_mult = Const (1.0, Float) +. profit_target_pct in

  (* Min hold gate *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, min_hold) in

  (* Gated exit signals *)
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi_exit >. mfi_exit_level)
        ||. (last >. EntryPrice *. profit_mult))
  in

  {
    name = "Nature_Boy_Opt";
    buy_trigger =
      mfi_entry <. mfi_oversold (* Volume-confirmed oversold *)
      &&. (last <. bb_lower) (* Price below BB lower *)
      &&. (natr <. natr_threshold) (* Calm market, not chaotic *)
      &&. recovering (* Price starting to recover *) &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. stop_mult) (* Stop loss always active *)
      ||. gated_exits (* Other exits after min_hold *)
      ||. App2 (Fun (">", ( > )), TicksHeld, max_hold);
    score = Const (100.0, Float) -. mfi_entry;
    max_positions = 20;
    position_size = 0.05;
  }

(* Nature Boy Winner - Optimized NATR-filtered mean reversion
   Optimization results from 3-month training (Sep-Nov 2025):
   - Return: 4.96% | Trades: 765 | Win Rate: 59.35%
   - Sharpe: 0.129 | Profit Factor: 1.458
   - P-value: 0.0002 (statistically significant)

   Key feature: NATR < 2.08% filters out chaotic/volatile periods.
   Fewer trades than Exit Winner, but hopefully better out-of-sample.
*)
let nature_boy =
  (* Entry MFI - period 190 *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (190, Int)) ))
  in

  (* NATR for volatility filter *)
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (29, Int)) ))
  in

  (* Entry Bollinger Band *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (248, Int),
               Const (1.61, Float),
               Const (1.61, Float) ) ))
  in

  (* Exit Bollinger Band *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (275, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Recovery filter: current price > previous price *)
  let recovering = last >. lag last 1 in

  (* Min hold gate: 36 ticks *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (36, Int)) in

  (* Gated exit signals *)
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (52.05, Float)) (* MFI exit level *)
        ||. (last >. EntryPrice *. Const (1.1035, Float))
            (* 10.35% profit target *))
  in

  {
    name = "Nature_Boy";
    buy_trigger =
      mfi
      <. Const (36.59, Float) (* MFI oversold *)
      &&. (last <. bb_lower)
      &&. (natr <. Const (2.08, Float)) (* NATR threshold *)
      &&. recovering &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.9093, Float)) (* 9.07% stop loss *)
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (258, Int));
    (* Max hold *)
    score = Const (100.0, Float) -. mfi;
    max_positions = 20;
    position_size = 0.05;
  }

(* Nature Boy 10x10 - Same strategy, 10 positions at 10% each *)
let nature_boy_10x10 =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (190, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (29, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (248, Int),
               Const (1.61, Float),
               Const (1.61, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (275, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (36, Int)) in
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (52.05, Float))
        ||. (last >. EntryPrice *. Const (1.1035, Float)))
  in
  {
    name = "Nature_Boy_10x10";
    buy_trigger =
      mfi
      <. Const (36.59, Float)
      &&. (last <. bb_lower)
      &&. (natr <. Const (2.08, Float))
      &&. recovering &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.9093, Float))
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (258, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 10;
    position_size = 0.10;
  }

(* Nature Boy 5x20 - Same strategy, 5 positions at 20% each (concentrated) *)
let nature_boy_5x20 =
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (190, Int)) ))
  in
  let natr =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), Const (29, Int)) ))
  in
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (248, Int),
               Const (1.61, Float),
               Const (1.61, Float) ) ))
  in
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (275, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in
  let recovering = last >. lag last 1 in
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (36, Int)) in
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (52.05, Float))
        ||. (last >. EntryPrice *. Const (1.1035, Float)))
  in
  {
    name = "Nature_Boy_5x20";
    buy_trigger =
      mfi
      <. Const (36.59, Float)
      &&. (last <. bb_lower)
      &&. (natr <. Const (2.08, Float))
      &&. recovering &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.9093, Float))
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (258, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(** Nature_Boy_Opt_V2 - Dual NATR Filter Optimization

    Builds on Nature Boy's locked entry/exit parameters, but re-optimizes
    the NATR volatility filter with separate indicators for lower and upper
    bounds, each with their own period.

    Hypothesis:
    - Lower bound filters dead/consolidating markets where moves are too small
    - Upper bound filters chaotic markets where mean reversion fails
    - These may benefit from different lookback periods:
      - Lower: shorter period to quickly detect volatility pickup
      - Upper: longer period to smooth out transient spikes

    Locked parameters (from Nature Boy):
    - mfi_period = 190, mfi_oversold = 36.59
    - bb_entry_period = 248, bb_entry_std = 1.61
    - mfi_exit_level = 52.05, bb_exit_period = 275
    - min_hold = 36, stop_loss = 9.07%, profit_target = 10.35%
    - max_hold = 258

    Optimized NATR filter (4 variables):
    1. natr_lower_period: [3, 275] - Lookback for lower bound check
    2. natr_lower: [0.1, 10.0] - Minimum volatility threshold (%)
    3. natr_upper_period: [3, 275] - Lookback for upper bound check
    4. natr_upper: [0.1, 10.0] - Maximum volatility threshold (%)

    NOTE: Use starting index of at least 350 (-i 350).
*)
let nature_boy_opt_v2 =
  (* NATR lower bound filter *)
  let natr_lower_period = Gadt_fo.var ~lower:3.0 ~upper:275.0 Gadt.Type.Int in
  let natr_lower = Gadt_fo.var ~lower:0.1 ~upper:10.0 Gadt.Type.Float in

  (* NATR upper bound filter *)
  let natr_upper_period = Gadt_fo.var ~lower:3.0 ~upper:275.0 Gadt.Type.Int in
  let natr_upper = Gadt_fo.var ~lower:0.1 ~upper:10.0 Gadt.Type.Float in

  (* Entry MFI - locked at 190 *)
  let mfi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), Const (190, Int)) ))
  in

  (* NATR for lower bound check *)
  let natr_lo =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), natr_lower_period) ))
  in

  (* NATR for upper bound check *)
  let natr_hi =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App1 (Fun ("I.natr", Tacaml.Indicator.Raw.natr), natr_upper_period) ))
  in

  (* Entry Bollinger Band - locked *)
  let bb_lower =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
               Const (248, Int),
               Const (1.61, Float),
               Const (1.61, Float) ) ))
  in

  (* Exit Bollinger Band - locked *)
  let bb_middle =
    Gadt.Data
      (App1
         ( Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
           App3
             ( Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
               Const (275, Int),
               Const (2.0, Float),
               Const (2.0, Float) ) ))
  in

  (* Recovery filter *)
  let recovering = last >. lag last 1 in

  (* Min hold gate - locked at 36 *)
  let past_min_hold = App2 (Fun (">=", ( >= )), TicksHeld, Const (36, Int)) in

  (* Gated exit signals - locked thresholds *)
  let gated_exits =
    past_min_hold
    &&. (last >. bb_middle
        ||. (mfi >. Const (52.05, Float))
        ||. (last >. EntryPrice *. Const (1.1035, Float)))
  in

  {
    name = "Nature_Boy_Opt_V2";
    buy_trigger =
      mfi
      <. Const (36.59, Float)
      &&. (last <. bb_lower)
      &&. (natr_lo >. natr_lower) (* Lower bound: enough volatility *)
      &&. (natr_hi <. natr_upper) (* Upper bound: not too chaotic *)
      &&. recovering &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (last <. EntryPrice *. Const (0.9093, Float))
      ||. gated_exits
      ||. App2 (Fun (">", ( > )), TicksHeld, Const (258, Int));
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }

(* Export all strategies *)
let all_strategies =
  [
    volume_confirms_opt;
    volume_confirms_with_stop_opt;
    horseman_opt;
    horseman_opt_r2;
    horseman_r2;
    free_horseman_opt;
    free_horseman;
    free_horseman_v2;
    horseman_exit_explorer;
    horseman_exit_winner;
    horseman_exit_winner_10x10;
    horseman_exit_winner_5x20;
    cowboy_opt;
    adx_cowboy;
    nature_boy_opt;
    nature_boy;
    nature_boy_10x10;
    nature_boy_5x20;
    nature_boy_opt_v2;
  ]
