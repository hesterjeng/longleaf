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
let run_on_target builder options source =
  let ( let* ) = Result.( let* ) in
  let* bars = load_bars options.Options.eio_env source in
  let mutices = Mutex.create [] in
  run_eval builder options mutices source bars

(** Run Evaluate battery: execute fixed strategy on each target *)
let run_evaluate builder options targets =
  let ( let* ) = Result.( let* ) in
  let* eval_results = Result.map_l (run_on_target builder options) targets in
  Result.return
    { Battery.Result.mode = "evaluate"; train_test_results = []; eval_results }

(** Run a battery test *)
let run builder options battery =
  match battery with
  | Battery.Evaluate targets -> run_evaluate builder options targets
  | Battery.SingleTrain { train = _; tests = _ } ->
    Error.fatal "SingleTrain battery mode not yet implemented"
  | Battery.WalkForward _ ->
    Error.fatal "WalkForward battery mode not yet implemented"
