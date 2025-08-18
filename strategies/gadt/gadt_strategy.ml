module Data = Longleaf_bars.Data

exception InvalidGADT of Error.t
exception OptimizationException

(* Strategy structure *)
type t = {
  name : string;
  buy_trigger : bool Gadt.t;
  sell_trigger : bool Gadt.t;
  max_positions : int;
  position_size : float;
}

let buy_conj (x : t) y = { x with buy_trigger = Gadt.(x.buy_trigger &&. y) }
let sell_conj (x : t) y = { x with sell_trigger = Gadt.(x.sell_trigger &&. y) }
let buy_disj (x : t) y = { x with buy_trigger = Gadt.(x.buy_trigger ||. y) }
let sell_disj (x : t) y = { x with sell_trigger = Gadt.(x.sell_trigger ||. y) }

module CollectIndicators : sig
  val top : t -> Tacaml.t list
end = struct
  let rec eval_simple : type a. a Gadt.t -> (a, Error.t) result =
   fun expr ->
    let eval = eval_simple in
    let ( let* ) = Result.( let* ) in
    (* Bounds checking *)
    match expr with
    | Const (x, _) -> Result.return x
    | Fun (_, f) -> Result.return f
    | ContextModifier (_, _, expr) -> eval expr
    | App1 (f, x) ->
      let* f = eval f in
      let* arg = eval x in
      let res = f arg in
      Result.return res
    | App2 (f, x, y) ->
      let* f = eval f in
      let* x = eval x in
      let* y = eval y in
      let res = f x y in
      Result.return res
    | App3 (f, x, y, z) ->
      let* f = eval f in
      let* x = eval x in
      let* y = eval y in
      let* z = eval z in
      let res = f x y z in
      Result.return res
    | Symbol () -> Error.fatal "Symbol unknown in eval_simple"
    | Var _ ->
      invalid_arg "Cannot evalute gadts with variables in them (eval_simple)"
    | Data _ -> Error.fatal "Cannot evaluate Data in eval_simple"
  (* | Indicator _ -> Error.fatal "Cannot evaluate Indicator in eval_simple" *)

  (* Collect all t from GADT expressions *)
  let rec collect_data_types : type a. a Gadt.t -> Data.Type.t list =
   fun x ->
    match x with
    | Fun _
    | Const _
    | Symbol _ ->
      []
    | Var _ ->
      raise
      @@ InvalidGADT
           (`FatalError "Encountered variable inside gadt for evaluation")
    | App1 (_, x) -> collect_data_types x
    | App2 (_, x, y) -> collect_data_types x @ collect_data_types y
    | App3 (_, x, y, z) ->
      collect_data_types x @ collect_data_types y @ collect_data_types z
    | ContextModifier (_, _, x) -> collect_data_types x
    | Data data_type ->
      [
        ( eval_simple data_type |> function
          | Ok x -> x
          | Error e -> raise @@ InvalidGADT e );
      ]

  let collect_indicators x =
    let tys = collect_data_types x in
    List.filter_map
      (function
        | Data.Type.Tacaml x -> Some x
        | _ -> None)
      tys

  let collect_strategy_indicators (strategy : t) : Tacaml.Indicator.t list =
    let buy_indicators = collect_indicators strategy.buy_trigger in
    let sell_indicators = collect_indicators strategy.sell_trigger in
    (* Remove duplicates using sort_uniq with polymorphic compare *)
    let all_indicators = buy_indicators @ sell_indicators in
    List.sort_uniq ~cmp:Stdlib.compare all_indicators

  let top strategy =
    collect_strategy_indicators strategy
    |> List.map Tacaml.Conv.indicator_to_safe
    |> List.uniq ~eq:Equal.poly
end

module Builder : sig
  val top : t -> Longleaf_template.builder
end = struct
  module State = Longleaf_state
  module Template = Longleaf_template

  (* Helper function to evaluate strategy triggers *)
  let eval_strategy_signal (strategy_expr : bool Gadt.t)
      (state : _ Longleaf_state.t) symbol =
    let ( let* ) = Result.( let* ) in
    let* data =
      State.data state symbol
      (* State.get_bars state symbol *)
    in
    let current_index = State.tick state in
    let context : Gadt.context =
      { instrument = symbol; data; index = current_index }
    in
    match Gadt.eval context strategy_expr with
    | Ok should_signal -> Result.return @@ Signal.make symbol should_signal
    | Error e -> Error e

  (* Convert GADT strategy to Template-compatible modules *)
  let gadt_to_buy_trigger (buy_expr : bool Gadt.t) (max_positions : int) =
    let module Buy_input : Longleaf_template.Buy_trigger.INPUT = struct
      let pass state symbol = eval_strategy_signal buy_expr state symbol
      let score _state _symbol = Result.return 1.0
      let num_positions = max_positions
    end in
    let module Buy_trigger = Template.Buy_trigger.Make (Buy_input) in
    (module Buy_trigger : Template.Buy_trigger.S)

  let gadt_to_sell_trigger (sell_expr : bool Gadt.t) =
    let module Sell_impl : Template.Sell_trigger.S = struct
      let make state symbol = eval_strategy_signal sell_expr state symbol
    end in
    (module Sell_impl : Template.Sell_trigger.S)

  let top (strategy : t) =
    let buy_trigger =
      gadt_to_buy_trigger strategy.buy_trigger strategy.max_positions
    in
    let sell_trigger = gadt_to_sell_trigger strategy.sell_trigger in
    let module StrategyBuilder =
      Template.Make ((val buy_trigger)) ((val sell_trigger))
    in
    (module StrategyBuilder : Template.BUILDER)
end

let run bars (options : Options.t) mutices strategy =
  let tacaml_indicators = CollectIndicators.top strategy in
  let options =
    {
      options with
      Options.flags = { options.flags with strategy_arg = strategy.name };
      tacaml_indicators;
    }
  in
  (* Collect custom indicators from the strategy *)
  let res = Longleaf_template.run (Builder.top strategy) bars options mutices in
  res
