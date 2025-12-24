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

(* Export all strategies *)
let all_strategies =
  [
    opening_dip_a;
    opening_dip_b;
    opening_dip_c;
  ]
