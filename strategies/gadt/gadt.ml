module Error = Longleaf_core.Error
module Signal = Longleaf_core.Signal
module Instrument = Longleaf_core.Instrument
module State = Longleaf_state
module Backend = Longleaf_backend
module Bars = Longleaf_bars
module Util = Longleaf_util
module Order = Longleaf_core.Order
module Data = Bars.Data
module Time = Longleaf_core.Time
module Options = Longleaf_core.Options
module Template = Longleaf_template

exception OptimizationException
exception InvalidGADT of Error.t

type const = VFloat of float | VInt of int
(* type env = (Uuidm.t, _ const) List.Assoc.t *)

module Type = struct
  type _ t = Float : float t | Int : int t
  type shadow = A : _ t -> shadow

  let shadow x = A x
end

type context = { instrument : Instrument.t; data : Data.t; index : int }

(* GADT AST with phantom types for compile-time type safety *)
type _ expr =
  | Const : 'a -> 'a expr
  (* Type-safe data access *)
  | Data : Data.Type.t expr -> float expr
  | Indicator : Tacaml.Indicator.t expr -> float expr
  | App1 : ('a -> 'b) expr * 'a expr -> 'b expr
  | App2 : ('a -> 'b -> 'c) expr * 'a expr * 'b expr -> 'c expr
  | App3 : ('a -> 'b -> 'c -> 'd) expr * 'a expr * 'b expr * 'c expr -> 'd expr
  | Fun : ('a -> 'b) -> ('a -> 'b) expr
  | ContextModifier : 'a expr * (context -> 'a -> context) * 'b expr -> 'b expr
  | Var : Uuidm.t * 'a Type.t -> 'a expr
  | Symbol : unit -> Instrument.t expr

let data x = Data x
let const x = Const x
let close = data @@ const @@ Data.Type.Close
let volume = data @@ const @@ Data.Type.Volume
let index = data @@ const @@ Data.Type.Index
let last = data @@ const @@ Data.Type.Last
let open_ = data @@ const @@ Data.Type.Open
let high = data @@ const @@ Data.Type.High
let low = data @@ const @@ Data.Type.Low

(* Strategy structure *)
type strategy = {
  name : string;
  buy_trigger : bool expr;
  sell_trigger : bool expr;
  max_positions : int;
  position_size : float;
}

(* Type-safe evaluation *)
let rec eval : type a. context -> a expr -> (a, Error.t) result =
 fun ({ instrument; data; index } as context) expr ->
  let ( let* ) = Result.( let* ) in
  (* Bounds checking *)
  if index < 0 || index >= Data.length data then
    Error.fatal
      (Printf.sprintf "GADT.eval: index %d out of bounds (data length: %d)"
         index (Data.length data))
  else
    match expr with
    | Const x -> Result.return x
    | Fun f -> Result.return f
    | ContextModifier (x, f, expr) ->
      let* arg = eval context x in
      let context = f context arg in
      let* res = eval context expr in
      Result.return res
    | App1 (f, x) ->
      let* f = eval context f in
      let* arg = eval context x in
      let res = f arg in
      Result.return res
    | App2 (f, x, y) ->
      let* f = eval context f in
      let* x = eval context x in
      let* y = eval context y in
      let res = f x y in
      Result.return res
    | App3 (f, x, y, z) ->
      let* f = eval context f in
      let* x = eval context x in
      let* y = eval context y in
      let* z = eval context z in
      let res = f x y z in
      Result.return res
    | Symbol () -> Result.return instrument
    | Var _ -> invalid_arg "Cannot evalute gadts with variables in them"
    | Data ty ->
      let* ty = eval context ty in
      Error.guard (Error.fatal "Error in GADT evaluation at Data node")
      @@ fun () -> Data.get data ty index
    | Indicator ty ->
      let* ty = eval context ty in
      Error.guard (Error.fatal "Error in GADT evaluation at Data node")
      @@ fun () -> Data.get data (Tacaml ty) index

module CollectIndicators : sig
  val top : strategy -> Tacaml.t list
end = struct
  let rec eval_simple : type a. a expr -> (a, Error.t) result =
   fun expr ->
    let eval = eval_simple in
    let ( let* ) = Result.( let* ) in
    (* Bounds checking *)
    match expr with
    | Const x -> Result.return x
    | Fun f -> Result.return f
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
    | Indicator _ -> Error.fatal "Cannot evaluate Indicator in eval_simple"

  (* Collect all t from GADT expressions *)
  let collect_data_types : type a. a expr -> Data.Type.t list = function
    | Var _ ->
      raise
      @@ InvalidGADT
           (`FatalError "Encountered variable inside gadt for evaluation")
    | Const _
    | App1 _
    | App2 _
    | App3 _
    | Fun _
    | ContextModifier _
    | Symbol _ ->
      []
    | Data data_type ->
      [
        ( eval_simple data_type |> function
          | Ok x -> x
          | Error e -> raise @@ InvalidGADT e );
      ]
    | Indicator data_type ->
      [
        ( eval_simple data_type |> function
          | Ok x -> Data.Type.Tacaml x
          | Error e -> raise @@ InvalidGADT e );
      ]

  let collect_indicators x =
    let tys = collect_data_types x in
    List.filter_map
      (function
        | Data.Type.Tacaml x -> Some x
        | _ -> None)
      tys

  let collect_strategy_indicators (strategy : strategy) :
      Tacaml.Indicator.t list =
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
  val top : strategy -> Template.builder
end = struct
  (* Helper function to evaluate strategy triggers *)
  let eval_strategy_signal (strategy_expr : bool expr) (state : _ State.t)
      symbol =
    let ( let* ) = Result.( let* ) in
    let* data =
      State.data state symbol
      (* State.get_bars state symbol *)
    in
    let current_index = State.tick state in
    let context = { instrument = symbol; data; index = current_index } in
    match eval context strategy_expr with
    | Ok should_signal -> Result.return @@ Signal.make symbol should_signal
    | Error e -> Error e

  (* Convert GADT strategy to Template-compatible modules *)
  let gadt_to_buy_trigger (buy_expr : bool expr) (max_positions : int) =
    let module Buy_input : Longleaf_template.Buy_trigger.INPUT = struct
      let pass state symbol = eval_strategy_signal buy_expr state symbol
      let score _state _symbol = Result.return 1.0
      let num_positions = max_positions
    end in
    let module Buy_trigger = Template.Buy_trigger.Make (Buy_input) in
    (module Buy_trigger : Template.Buy_trigger.S)

  let gadt_to_sell_trigger (sell_expr : bool expr) =
    let module Sell_impl : Template.Sell_trigger.S = struct
      let make state symbol = eval_strategy_signal sell_expr state symbol
    end in
    (module Sell_impl : Template.Sell_trigger.S)

  let top (strategy : strategy) =
    let buy_trigger =
      gadt_to_buy_trigger strategy.buy_trigger strategy.max_positions
    in
    let sell_trigger = gadt_to_sell_trigger strategy.sell_trigger in
    let module StrategyBuilder =
      Template.Make ((val buy_trigger)) ((val sell_trigger))
    in
    (module StrategyBuilder : Template.BUILDER)
end

module Subst = struct
  module Bindings = struct
    include Map.Make (Uuidm)

    let get id map =
      get id map |> function
      | Some x -> Ok x
      | None -> Error.fatal "No binding for variable"
  end

  let rec collect_variables : type a. a expr -> 'b list = function
    | Var (id, ty) -> [ (id, Type.A ty) ]
    | Const _ -> []
    | Symbol _ -> []
    | Data x -> collect_variables x
    | ContextModifier (expr, _, x) ->
      collect_variables expr @ collect_variables x
    | Indicator x -> collect_variables x
    | App1 (f, x) -> collect_variables f @ collect_variables x
    | App2 (f, x, y) ->
      collect_variables f @ collect_variables x @ collect_variables y
    | App3 (f, x, y, z) ->
      collect_variables f @ collect_variables x @ collect_variables y
      @ collect_variables z
    | Fun _ -> []

  let collect_variables : 'a expr -> (Uuidm.t * Type.shadow) list =
   fun x ->
    let vars = collect_variables x in
    List.uniq ~eq:(fun (id0, _) (id1, _) -> Uuidm.equal id0 id1) vars

  type env = { float_map : float Bindings.t; int_map : int Bindings.t }

  (* let of_l l = *)
  (*   let init = { *)
  (*     float_map = Bindings.empty; *)
  (*     int_map = Bindings.empty; *)
  (*   } in *)

  let env_of_arr params (arr : (Uuidm.t * Type.shadow) array) =
    Array.foldi
      (fun env i (id, Type.A ty) ->
        match ty with
        | Type.Float ->
          { env with float_map = Bindings.add id params.(i) env.float_map }
        | Type.Int ->
          let int_val = Int.of_float params.(i) in
          { env with int_map = Bindings.add id int_val env.int_map })
      { float_map = Bindings.empty; int_map = Bindings.empty }
      arr

  let rec instantiate : type a. env -> a expr -> (a expr, Error.t) result =
    let ( let* ) = Result.( let* ) in
    fun env -> function
      | Var (id, ty) -> (
        match ty with
        | Float -> (
          match Bindings.get id env.float_map with
          | Ok res -> Result.return @@ Const res
          | Error e -> Error e)
        | Int -> (
          match Bindings.get id env.int_map with
          | Ok res -> Result.return @@ Const res
          | Error e -> Error e))
      | Const c -> Result.return (Const c)
      | Fun f -> Result.return (Fun f)
      | Symbol () -> Result.return (Symbol ())
      | App1 (f, x) ->
        let* f' = instantiate env f in
        let* x' = instantiate env x in
        Result.return @@ App1 (f', x')
      | App2 (f, x, y) ->
        let* f' = instantiate env f in
        let* x' = instantiate env x in
        let* y' = instantiate env y in
        Result.return @@ App2 (f', x', y')
      | App3 (f, x, y, z) ->
        let* f' = instantiate env f in
        let* x' = instantiate env x in
        let* y' = instantiate env y in
        let* z' = instantiate env z in
        Result.return @@ App3 (f', x', y', z')
      | Data ty ->
        let* ty' = instantiate env ty in
        Result.return @@ Data ty'
      | ContextModifier (arg, f, x) ->
        let* ty' = instantiate env x in
        let* arg' = instantiate env arg in
        Result.return @@ ContextModifier (arg', f, ty')
      | Indicator ty ->
        let* ty' = instantiate env ty in
        Result.return @@ Indicator ty'

  (* let apply_subst (x : 'a expr) = *)
  (*   match collect_variables x with *)
  (*   | [] -> *)
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
  let res = Template.run (Builder.top strategy) bars options mutices in
  res

(* Pretty printer for GADT expressions *)
let rec pp_expr : type a. Format.formatter -> a expr -> unit =
 fun fmt expr ->
  match expr with
  | Const _ -> Format.fprintf fmt "Const(?)"
  | Fun _ -> Format.fprintf fmt "Fun(?)"
  | Symbol () -> Format.fprintf fmt "Symbol()"
  | ContextModifier _ -> Format.fprintf fmt "ContextModifier()"
  | Var (id, ty) ->
    let ty_str =
      match ty with
      | Type.Float -> "Float"
      | Type.Int -> "Int"
    in
    Format.fprintf fmt "Var(%s:%s)" (Uuidm.to_string id) ty_str
  | Data e -> Format.fprintf fmt "Data(@[%a@])" pp_expr e
  | Indicator e -> Format.fprintf fmt "Indicator(@[%a@])" pp_expr e
  | App1 (f, x) -> Format.fprintf fmt "App1(@[%a,@ %a@])" pp_expr f pp_expr x
  | App2 (f, x, y) ->
    Format.fprintf fmt "App2(@[%a,@ %a,@ %a@])" pp_expr f pp_expr x pp_expr y
  | App3 (f, x, y, z) ->
    Format.fprintf fmt "App3(@[%a,@ %a,@ %a,@ %a@])" pp_expr f pp_expr x pp_expr
      y pp_expr z

(* Helper to print expression to string *)
let expr_to_string : type a. a expr -> string =
 fun expr -> Format.asprintf "%a" pp_expr expr

(* Helper to print variables found in expression *)
let debug_variables : type a. a expr -> unit =
 fun expr ->
  let vars = Subst.collect_variables expr in
  Eio.traceln "=== VARIABLES IN EXPRESSION ===";
  Eio.traceln "Expression: %s" (expr_to_string expr);
  Eio.traceln "Variables found: %d" (List.length vars);
  List.iteri
    (fun i (id, Type.A ty) ->
      let ty_str =
        match ty with
        | Type.Float -> "Float"
        | Type.Int -> "Int"
      in
      Eio.traceln "  [%d] %s: %s" i (Uuidm.to_string id) ty_str)
    vars;
  Eio.traceln "================================"

(* Convenience operators *)
let ( >. ) e1 e2 = App2 (Fun ( >. ), e1, e2)
let ( <. ) e1 e2 = App2 (Fun ( <. ), e1, e2)
let ( >=. ) e1 e2 = App2 (Fun ( >=. ), e1, e2)
let ( <=. ) e1 e2 = App2 (Fun ( <=. ), e1, e2)
let ( =. ) e1 e2 = App2 (Fun Float.equal, e1, e2)
let ( &&. ) e1 e2 = App2 (Fun ( && ), e1, e2)
let ( ||. ) e1 e2 = App2 (Fun ( || ), e1, e2)
let ( +. ) e1 e2 = App2 (Fun ( +. ), e1, e2)
let ( -. ) e1 e2 = App2 (Fun ( -. ), e1, e2)
let ( *. ) e1 e2 = App2 (Fun ( *. ), e1, e2)
let ( /. ) e1 e2 = App2 (Fun ( /. ), e1, e2)

(* Additional operators *)
let not_ e = App1 (Fun not, e)

(* Domain-specific functions using ContextModifier *)

(* Lag function: access data N periods ago *)
let lag expr periods =
  ContextModifier
    ( Const periods,
      (fun ctx periods -> { ctx with index = ctx.index - periods }),
      expr )

(* Crossover detection functions *)
let cross_up e1 e2 =
  (* Current: e1 > e2 *)
  e1 >. e2
  &&.
  (* Previous: e1 <= e2 *)
  (lag e1 1 <=. lag e2 1)

let cross_down e1 e2 =
  (* Current: e1 < e2 *)
  e1 <. e2
  &&.
  (* Previous: e1 >= e2 *)
  (lag e1 1 >=. lag e2 1)

(* Placeholder functions for options-specific expressions *)
exception SpecialFunction of string

let moneyness_fn = fun _ _ -> raise (SpecialFunction "moneyness")
let days_to_expiry_fn = fun _ -> raise (SpecialFunction "days_to_expiry")
let moneyness underlying option = App2 (Fun moneyness_fn, underlying, option)
let days_to_expiry option = App1 (Fun days_to_expiry_fn, option)
