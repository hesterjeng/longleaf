(** Battery execution logic.

    This module handles running strategies across multiple datasets for battery
    testing. It uses Longleaf_template.run_state to execute strategies and
    extract full state for statistics. *)

module Battery = Longleaf_battery
module State = Longleaf_state
module Stats = State.Stats
module Bars = Longleaf_bars
module Template = Longleaf_template
module Options = Longleaf_core.Options
module Mutex = State.Mutex
module Pmutex = Longleaf_util.Pmutex
module Target = Longleaf_core.Target
module Error = Longleaf_core.Error

(** Load bars from a target (file path) *)
let load_bars eio_env (target : Target.t) =
  match target with
  | File path ->
    (try Result.return @@ Bars.of_file ~eio_env path with
    | e -> Error.fatal @@ "Failed to load bars: " ^ Printexc.to_string e)
  | Download ->
    Error.fatal "Download target not yet supported in battery runner"
  | BatteryName _ ->
    Error.fatal "BatteryName is not a valid data source for battery runner"

(** Run a single evaluation: execute strategy on bars, return Eval_result *)
let run_eval builder options mutices source bars =
  let ( let* ) = Result.( let* ) in
  let* final_state = Template.run_state builder bars options mutices in
  let stats = State.stats final_state in
  let trade_stats =
    Stats.TradeStats.compute (State.order_history final_state)
  in
  Result.return { Battery.Eval_result.source; stats; trade_stats }

(** Run a strategy on a target, loading bars and executing *)
let run_on_target builder options mutices source =
  let ( let* ) = Result.( let* ) in
  let* bars = load_bars options.Options.eio_env source in
  run_eval builder options mutices source bars

(** Run Evaluate battery: execute fixed strategy on each target *)
let run_evaluate builder options mutices targets =
  let ( let* ) = Result.( let* ) in
  let* eval_results =
    Result.map_l (run_on_target builder options mutices) targets
  in
  Result.return
    { Battery.Result.mode = "evaluate"; train_test_results = []; eval_results }

(** Run a battery test *)
let run builder options mutices battery =
  match battery with
  | Battery.Evaluate targets -> run_evaluate builder options mutices targets
  | Battery.SingleTrain { train = _; tests = _ } ->
    Error.fatal "SingleTrain battery mode not yet implemented"
  | Battery.WalkForward _ ->
    Error.fatal "WalkForward battery mode not yet implemented"

(** {1 Predefined Batteries} *)

(** All half-year periods from 2023-2025 *)
let quarterly_2023_2025 =
  Battery.evaluate
    [
      Target.File "data/q1q2-2023-1min.json";
      Target.File "data/q3q4-2023-1min.json";
      Target.File "data/q1q2-2024-1min.json";
      Target.File "data/q3q4-2024-1min.json";
      Target.File "data/q1q2-2025-1min.json";
      Target.File "data/q3q4-2025-1min.json";
    ]

(** Lookup a battery by name *)
let get_battery name =
  match name with
  | "quarterly" | "quarterly_2023_2025" -> Some quarterly_2023_2025
  | _ -> None

(** List of available battery names *)
let available_batteries = [ "quarterly"; "quarterly_2023_2025" ]
