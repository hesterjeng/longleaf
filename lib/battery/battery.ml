(** Battery testing module for strategy validation.

    A battery is a collection of datasets used to validate a trading strategy
    across multiple time periods. Three distinct modes are supported:

    - {b WalkForward}: Sequential periods where we train on N, test on N+1, then
      advance and repeat. This tests adaptive retraining.

    - {b SingleTrain}: One training period with multiple test periods. This
      tests how well parameters generalize.

    - {b Evaluate}: No training - run a pre-configured strategy on multiple
      datasets. This tests consistency of a fixed strategy. *)

module Stats = Longleaf_state.Stats

(** Result from evaluating one dataset *)
module Eval_result = struct
  type t = {
    source : Target.t;
    stats : Stats.t;
    trade_stats : Stats.TradeStats.t option;
  }
  [@@deriving show]
end

(** Result from one train/test cycle (for WalkForward and SingleTrain) *)
module Train_test_result = struct
  type t = { train_source : Target.t; test_results : Eval_result.t list }
  [@@deriving show]
end

(** Full battery result - structure depends on battery type *)
module Result = struct
  type t = {
    mode : string;  (** "walk_forward" | "single_train" | "evaluate" *)
    train_test_results : Train_test_result.t list;
        (** Empty for Evaluate mode *)
    eval_results : Eval_result.t list;  (** All test/eval results flattened *)
  }
  [@@deriving show]
end

(** Three distinct battery modes *)
type t =
  | WalkForward of Target.t list
      (** Sequential periods: train on N, test on N+1, advance, repeat. Example:
          [Q1; Q2; Q3; Q4] becomes train Q1/test Q2, train Q2/test Q3, train
          Q3/test Q4 *)
  | SingleTrain of { train : Target.t; tests : Target.t list }
      (** One training period, multiple test periods. Example: train on 2023,
          test on [Q1'24; Q2'24; Q3'24; Q4'24] *)
  | Evaluate of Target.t list
      (** No training - run a fixed strategy on multiple datasets. Example: run
          NatureBoyV2 on [Q1; Q2; Q3; Q4] to measure consistency *)
[@@deriving show]

(** {1 Constructors} *)

(** Create a walk-forward battery from sequential periods.

    Given [Q1; Q2; Q3; Q4], this will:
    - Train on Q1, test on Q2
    - Train on Q2, test on Q3
    - Train on Q3, test on Q4 *)
let walk_forward (periods : Target.t list) : t = WalkForward periods

(** Create a single-train-multiple-test battery.

    Train once on [train], then test on each dataset in [tests]. *)
let single_train ~train ~tests : t = SingleTrain { train; tests }

(** Create an evaluation-only battery (no training).

    Run a pre-configured strategy (e.g., NatureBoyV2) on each dataset in
    [targets] to measure consistency. *)
let evaluate (targets : Target.t list) : t = Evaluate targets

(** {1 Utility Functions} *)

(** Get the number of test periods in a battery *)
let num_test_periods = function
  | WalkForward periods -> max 0 (List.length periods - 1)
  | SingleTrain { tests; _ } -> List.length tests
  | Evaluate targets -> List.length targets

(** Get all data sources referenced by a battery *)
let all_sources = function
  | WalkForward periods -> periods
  | SingleTrain { train; tests } -> train :: tests
  | Evaluate targets -> targets

(** Get the mode name as a string *)
let mode_name = function
  | WalkForward _ -> "walk_forward"
  | SingleTrain _ -> "single_train"
  | Evaluate _ -> "evaluate"

(** {1 Result Statistics} *)

(** Extract sharpe ratios from eval results (only from periods with trades) *)
let sharpes (r : Result.t) : float list =
  List.filter_map
    (fun (e : Eval_result.t) ->
      Option.map (fun (ts : Stats.TradeStats.t) -> ts.sharpe) e.trade_stats)
    r.eval_results

(** Extract total returns from eval results *)
let total_returns (r : Result.t) : float list =
  List.map (fun (e : Eval_result.t) -> e.stats.total_return) r.eval_results

(** Extract max drawdowns from eval results *)
let max_drawdowns (r : Result.t) : float list =
  List.map (fun (e : Eval_result.t) -> e.stats.max_drawdown) r.eval_results

(** Average of a float list *)
let avg (l : float list) : float =
  match l with
  | [] -> 0.0
  | l -> List.fold_left ( +. ) 0.0 l /. Float.of_int (List.length l)

(** Standard deviation of a float list *)
let std (l : float list) : float =
  match l with
  | [] -> 0.0
  | l ->
    let mean = avg l in
    let variance =
      List.fold_left (fun acc x -> acc +. ((x -. mean) ** 2.0)) 0.0 l
      /. Float.of_int (List.length l)
    in
    sqrt variance

(** Average out-of-sample Sharpe ratio *)
let avg_sharpe (r : Result.t) : float = avg (sharpes r)

(** Standard deviation of out-of-sample Sharpe ratios *)
let std_sharpe (r : Result.t) : float = std (sharpes r)

(** Average out-of-sample total return *)
let avg_return (r : Result.t) : float = avg (total_returns r)

(** Standard deviation of out-of-sample total returns *)
let std_return (r : Result.t) : float = std (total_returns r)

(** Average max drawdown across periods *)
let avg_drawdown (r : Result.t) : float = avg (max_drawdowns r)

(** Worst (highest) max drawdown across periods *)
let worst_drawdown (r : Result.t) : float =
  match max_drawdowns r with
  | [] -> 0.0
  | l -> List.fold_left Float.max 0.0 l

(** Consistency: fraction of periods with positive Sharpe *)
let consistency (r : Result.t) : float =
  let sharpe_list = sharpes r in
  match sharpe_list with
  | [] -> 0.0
  | l ->
    let positive = List.count (fun s -> Float.compare s 0.0 > 0) l in
    Float.of_int positive /. Float.of_int (List.length l)

(** Best performing period by Sharpe ratio *)
let best_by_sharpe (r : Result.t) : Eval_result.t option =
  let with_sharpe =
    List.filter_map
      (fun (e : Eval_result.t) ->
        Option.map
          (fun (ts : Stats.TradeStats.t) -> (e, ts.sharpe))
          e.trade_stats)
      r.eval_results
  in
  match with_sharpe with
  | [] -> None
  | l ->
    let best, _ =
      List.fold_left
        (fun (best_e, best_s) (e, s) ->
          if Float.compare s best_s > 0 then (e, s) else (best_e, best_s))
        (List.hd l) (List.tl l)
    in
    Some best

(** Worst performing period by Sharpe ratio *)
let worst_by_sharpe (r : Result.t) : Eval_result.t option =
  let with_sharpe =
    List.filter_map
      (fun (e : Eval_result.t) ->
        Option.map
          (fun (ts : Stats.TradeStats.t) -> (e, ts.sharpe))
          e.trade_stats)
      r.eval_results
  in
  match with_sharpe with
  | [] -> None
  | l ->
    let worst, _ =
      List.fold_left
        (fun (worst_e, worst_s) (e, s) ->
          if Float.compare s worst_s < 0 then (e, s) else (worst_e, worst_s))
        (List.hd l) (List.tl l)
    in
    Some worst

(** Number of periods evaluated *)
let num_periods (r : Result.t) : int = List.length r.eval_results

(** Number of periods with at least one trade *)
let num_periods_with_trades (r : Result.t) : int =
  List.count
    (fun (e : Eval_result.t) -> Option.is_some e.trade_stats)
    r.eval_results

(** Extract capital participation rates from eval results *)
let capital_participations (r : Result.t) : float list =
  List.map (fun (e : Eval_result.t) -> e.stats.capital_participation) r.eval_results

(** Average capital participation across periods *)
let avg_participation (r : Result.t) : float = avg (capital_participations r)
