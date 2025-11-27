(** Volume Momentum Strategies

    Strategies based on the hypothesis that price moves confirmed by volume
    (measured via MFI - Money Flow Index) tend to continue.

    MFI is essentially "volume-weighted RSI" - it combines price direction
    with volume to measure buying/selling pressure on a 0-100 scale.
*)

open Gadt
open Gadt_strategy

module Real = Gadt_fo.Constant

(* Re-use helper functions from gadt_strategy *)
let safe_to_enter ?(close_buffer=10.0) () : bool Gadt.t =
  is_open TickTime &&.
  App1 (Fun ("not", not), is_close TickTime (Const (close_buffer, Float)))

let force_exit_eod ?(close_buffer=10.0) () : bool Gadt.t =
  is_close TickTime (Const (close_buffer, Float))

let max_holding_time max_ticks : bool Gadt.t =
  let max_ticks_expr = Const (max_ticks, Int) in
  App2 (Fun (">", (>)), TicksHeld, max_ticks_expr)

let stop_loss stop_loss_pct : bool Gadt.t =
  let multiplier = Float.((-) 1.0 stop_loss_pct) in
  last <. (EntryPrice *. Const (multiplier, Float))

let profit_target profit_target_pct : bool Gadt.t =
  let multiplier = Float.((+) 1.0 profit_target_pct) in
  last >. (EntryPrice *. Const (multiplier, Float))

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
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
      App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period)))
  in

  (* Create Momentum indicator with variable period *)
  let mom =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
      App1 (Fun ("I.mom", Tacaml.Indicator.Raw.mom), mom_period)))
  in

  {
    name = "VolumeConfirms_Opt";
    (* Entry: Volume-confirmed buying pressure + positive momentum *)
    buy_trigger =
      (mfi >. mfi_entry)                    (* Strong buying pressure *)
      &&. (mom >. Const (0.0, Float))       (* Price moving up *)
      &&. safe_to_enter ();
    (* Exit: Buying pressure fades or momentum reverses *)
    sell_trigger =
      force_exit_eod ()                     (* No overnight positions *)
      ||. (mfi <. mfi_exit)                 (* Buying pressure fading *)
      ||. (mom <. Const (0.0, Float))       (* Momentum reversed *)
      ||. max_holding_time 120;             (* Allow momentum to play out *)
    (* Score: Higher MFI = stronger signal = higher priority *)
    score = mfi;
    max_positions = 10;
    position_size = 0.10;
  }

(** VolumeConfirms_WithStop_Opt - Volume-Confirmed Momentum with Stop Loss

    Same hypothesis as VolumeConfirms_Opt but adds a stop loss for risk control.
    Use this version after initial optimization to see if stops improve results.

    Variables to optimize (5 total):
    1. mfi_period: [5, 50] - MFI lookback period
    2. mfi_entry: [50, 90] - Enter when MFI exceeds this
    3. mfi_exit: [20, 60] - Exit when MFI drops below
    4. mom_period: [3, 30] - Momentum period
    5. stop_pct: [0.01, 0.08] - Stop loss percentage (1-8%)
*)
let volume_confirms_with_stop_opt =
  let mfi_period = Gadt_fo.var ~lower:5.0 ~upper:50.0 Gadt.Type.Int in
  let mfi_entry = Gadt_fo.var ~lower:50.0 ~upper:90.0 Gadt.Type.Float in
  let mfi_exit = Gadt_fo.var ~lower:20.0 ~upper:60.0 Gadt.Type.Float in
  let mom_period = Gadt_fo.var ~lower:3.0 ~upper:30.0 Gadt.Type.Int in
  let stop_pct = Gadt_fo.var ~lower:0.01 ~upper:0.08 Gadt.Type.Float in

  let mfi =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
      App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period)))
  in

  let mom =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
      App1 (Fun ("I.mom", Tacaml.Indicator.Raw.mom), mom_period)))
  in

  let stop_mult = Const (1.0, Float) -. stop_pct in

  {
    name = "VolumeConfirms_WithStop_Opt";
    buy_trigger =
      (mfi >. mfi_entry)
      &&. (mom >. Const (0.0, Float))
      &&. safe_to_enter ();
    sell_trigger =
      force_exit_eod ()
      ||. (mfi <. mfi_exit)
      ||. (mom <. Const (0.0, Float))
      ||. (last <. (EntryPrice *. stop_mult))  (* Variable stop loss *)
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
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
      App1 (Fun ("I.mfi", Tacaml.Indicator.Raw.mfi), mfi_period)))
  in

  (* Create Bollinger Bands with variable period and std *)
  let bb_lower =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
      App3 (Fun ("I.lower_bband", Tacaml.Indicator.Raw.lower_bband),
        bb_period, bb_std, bb_std)))
  in

  let bb_middle =
    Gadt.Data (App1 (Fun ("tacaml", fun x -> Longleaf_bars.Data.Type.Tacaml x),
      App3 (Fun ("I.middle_bband", Tacaml.Indicator.Raw.middle_bband),
        bb_period, bb_std, bb_std)))
  in

  {
    name = "Horseman_Opt";
    (* Entry: Volume-confirmed oversold + price below lower BB *)
    buy_trigger =
      (mfi <. mfi_oversold)                 (* Selling exhaustion confirmed by volume *)
      &&. (last <. bb_lower)                (* Price stretched below mean *)
      &&. safe_to_enter ();
    (* Exit: Mean reversion complete or risk controls *)
    sell_trigger =
      force_exit_eod ()                     (* No overnight positions *)
      ||. (last >. bb_middle)               (* Mean reversion complete *)
      ||. (mfi >. mfi_overbought)           (* Symmetric overbought exit *)
      ||. stop_loss 0.02                    (* Fixed 2% stop *)
      ||. profit_target 0.05;               (* Fixed 5% target *)
    (* Score: Lower MFI = more oversold = higher priority *)
    score = Const (100.0, Float) -. mfi;
    max_positions = 20;
    position_size = 0.05;
  }

(* Export all strategies *)
let all_strategies = [
  volume_confirms_opt;
  volume_confirms_with_stop_opt;
  horseman_opt;
]
