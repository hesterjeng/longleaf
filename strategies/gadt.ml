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

type const = VFloat of float | VInt of int
(* type env = (Uuidm.t, _ const) List.Assoc.t *)

module Type = struct
  type _ t = Float : float t | Int : int t
  type shadow = A : _ t -> shadow

  let shadow x = A x
end

(* GADT AST with phantom types for compile-time type safety *)
type _ expr =
  (* Literals *)
  | Float : float -> float expr
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Const : 'a -> 'a expr
  (* Type-safe data access *)
  | Data : Data.Type.t expr -> float expr
  | Indicator : Tacaml.Indicator.t expr -> float expr
  | App1 : ('a -> 'b) expr * 'a expr -> 'b expr
  | App2 : ('a -> 'b -> 'c) expr * 'a expr * 'b expr -> 'c expr
  | App3 : ('a -> 'b -> 'c -> 'd) expr * 'a expr * 'b expr * 'c expr -> 'd expr
  | Fun : ('a -> 'b) -> ('a -> 'b) expr
  | Var : Uuidm.t * 'a Type.t -> 'a expr
  | Symbol : unit -> Instrument.t expr
  (* Comparisons *)
  | GT : float expr * float expr -> bool expr
  | LT : float expr * float expr -> bool expr
  | GTE : float expr * float expr -> bool expr
  | LTE : float expr * float expr -> bool expr
  | EQ : float expr * float expr -> bool expr
  (* Logical operations *)
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  (* Basic arithmetic *)
  | Add : float expr * float expr -> float expr
  | Sub : float expr * float expr -> float expr
  | Mul : float expr * float expr -> float expr
  | Div : float expr * float expr -> float expr
  (* Options-specific expressions *)
  | Moneyness :
      Instrument.t * Instrument.t
      -> float expr (* underlying * option *)
  | Days_to_expiry : Instrument.t -> int expr (* option *)
  (* Lag expressions for historical data access *)
  | Lag : 'a expr * int -> 'a expr (* Access data N periods ago *)
  (* Crossover detection *)
  | CrossUp :
      float expr * float expr
      -> bool expr (* line1 crosses above line2 *)
  | CrossDown :
      float expr * float expr
      -> bool expr (* line1 crosses below line2 *)

let data x = Data x
let const x = Const x
let close = data @@ const @@ Data.Type.Close
let volume = data @@ const @@ Data.Type.Volume
let index = data @@ const @@ Data.Type.Index
let last = data @@ const @@ Data.Type.Last
let open_ = data @@ const @@ Data.Type.Open
let high = data @@ const @@ Data.Type.High
let low = data @@ const @@ Data.Type.Low

(* let of_const (type a) (x : const) = *)
(*   match x with *)
(*   | VInt i -> Int i *)
(*   | VFloat f -> Float f *)

(* Strategy structure *)
type strategy = {
  name : string;
  buy_trigger : bool expr;
  sell_trigger : bool expr;
  max_positions : int;
  position_size : float;
}

(* Type-safe evaluation *)
let rec eval : type a.
    Instrument.t -> a expr -> Data.t -> int -> (a, Error.t) result =
 fun symbol expr data index ->
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
    | App1 (f, x) ->
      let* f = eval symbol f data index in
      let* arg = eval symbol x data index in
      let res = f arg in
      Result.return res
    | App2 (f, x, y) ->
      let* f = eval symbol f data index in
      let* x = eval symbol x data index in
      let* y = eval symbol y data index in
      let res = f x y in
      Result.return res
    | App3 (f, x, y, z) ->
      let* f = eval symbol f data index in
      let* x = eval symbol x data index in
      let* y = eval symbol y data index in
      let* z = eval symbol z data index in
      let res = f x y z in
      Result.return res
    | Symbol () -> Result.return symbol
    | Float f -> Result.return f
    | Var _ -> invalid_arg "Cannot evalute gadts with variables in them"
    | Int i -> Result.return i
    | Bool b -> Result.return b
    | Data ty ->
      let* ty = eval symbol ty data index in
      Error.guard (Error.fatal "Error in GADT evaluation at Data node")
      @@ fun () -> Data.get data ty index
    | Indicator ty ->
      let* ty = eval symbol ty data index in
      Error.guard (Error.fatal "Error in GADT evaluation at Data node")
      @@ fun () -> Data.get data (Tacaml ty) index
    | GT (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 >. v2)
    | LT (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 <. v2)
    | GTE (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 >=. v2)
    | LTE (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 <=. v2)
    | EQ (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (Float.equal v1 v2)
    | And (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 && v2)
    | Or (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 || v2)
    | Not e ->
      let* v = eval symbol e data index in
      Result.return (not v)
    | Add (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 +. v2)
    | Sub (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 -. v2)
    | Mul (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 *. v2)
    | Div (e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      if Float.equal v2 0.0 then Error.fatal "Division by zero in GADT.eval"
      else Result.return (v1 /. v2)
    | Moneyness (underlying, option) -> (
      Error.guard (Error.fatal "GADT.eval moneyness") @@ fun () ->
      match (underlying, option) with
      | Instrument.Security _, Instrument.Contract contract ->
        (* Get underlying price from current data *)
        let underlying_price = Data.get data Data.Type.Close index in
        (* Use contract strike price *)
        let strike_price = contract.strike_price in
        underlying_price /. strike_price
      | _ -> failwith "Moneyness requires Security and Contract instruments")
    | Days_to_expiry option -> (
      Error.guard (Error.fatal "GADT.eval days_to_expiry") @@ fun () ->
      match option with
      | Instrument.Contract contract ->
        (* Parse expiration date and get current time from data *)
        let expiration_date = Time.of_ymd contract.expiration_date in
        let current_time_float = Data.get data Data.Type.Time index in
        let current_date =
          Ptime.of_float_s current_time_float
          |> Option.get_exn_or "Invalid timestamp in days_to_expiry"
        in
        let diff_seconds =
          Ptime.diff expiration_date current_date |> Ptime.Span.to_float_s
        in
        let diff_days = diff_seconds /. 86400.0 |> Float.to_int in
        max 0 diff_days (* Ensure non-negative *)
      | _ -> failwith "Days_to_expiry requires Contract instrument")
    | Lag (expr, periods) ->
      let lag_index = index - periods in
      if lag_index < 0 then
        Error.fatal
          (Printf.sprintf
             "GADT.eval: lag index %d out of bounds (periods: %d, current \
              index: %d)"
             lag_index periods index)
      else eval symbol expr data lag_index
    | CrossUp (e1, e2) ->
      if index = 0 then
        Error.fatal "GADT.eval: CrossUp requires at least 1 historical period"
      else
        let* current_e1 = eval symbol e1 data index in
        let* current_e2 = eval symbol e2 data index in
        let* prev_e1 = eval symbol e1 data (index - 1) in
        let* prev_e2 = eval symbol e2 data (index - 1) in
        Result.return (prev_e1 <=. prev_e2 && current_e1 >. current_e2)
    | CrossDown (e1, e2) ->
      if index = 0 then
        Error.fatal "GADT.eval: CrossDown requires at least 1 historical period"
      else
        let* current_e1 = eval symbol e1 data index in
        let* current_e2 = eval symbol e2 data index in
        let* prev_e1 = eval symbol e1 data (index - 1) in
        let* prev_e2 = eval symbol e2 data (index - 1) in
        Result.return (prev_e1 >=. prev_e2 && current_e1 <. current_e2)

let rec eval_simple : type a. a expr -> (a, Error.t) result =
 fun expr ->
  let eval = eval_simple in
  let ( let* ) = Result.( let* ) in
  (* Bounds checking *)
  match expr with
  | Const x -> Result.return x
  | Fun f -> Result.return f
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
  | Float f -> Result.return f
  | Var _ -> invalid_arg "Cannot evalute gadts with variables in them"
  | Int i -> Result.return i
  | Bool b -> Result.return b
  | Data _ -> Error.fatal "Cannot evaluate Data in eval_simple"
  | Indicator _ -> Error.fatal "Cannot evaluate Indicator in eval_simple"
  (* | Indicator _ -> Error.fatal "Cannot evaluate Indicator in eval_simple" *)
  | GT (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 >. v2)
  | LT (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 <. v2)
  | GTE (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 >=. v2)
  | LTE (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 <=. v2)
  | EQ (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (Float.equal v1 v2)
  | And (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 && v2)
  | Or (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 || v2)
  | Not e ->
    let* v = eval e in
    Result.return (not v)
  | Add (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 +. v2)
  | Sub (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 -. v2)
  | Mul (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    Result.return (v1 *. v2)
  | Div (e1, e2) ->
    let* v1 = eval e1 in
    let* v2 = eval e2 in
    if Float.equal v2 0.0 then Error.fatal "Division by zero in GADT.eval"
    else Result.return (v1 /. v2)
  | Moneyness _
  | Days_to_expiry _
  | Lag _
  | CrossUp _
  | CrossDown _ ->
    Error.fatal "Unable to evalute complex constructor in eval_simple"

(* Convenience operators *)
let ( >. ) e1 e2 = GT (e1, e2)
let ( <. ) e1 e2 = LT (e1, e2)
let ( >=. ) e1 e2 = GTE (e1, e2)
let ( <=. ) e1 e2 = LTE (e1, e2)
let ( =. ) e1 e2 = EQ (e1, e2)
let ( &&. ) e1 e2 = And (e1, e2)
let ( ||. ) e1 e2 = Or (e1, e2)
let ( +. ) e1 e2 = Add (e1, e2)
let ( -. ) e1 e2 = Sub (e1, e2)
let ( *. ) e1 e2 = Mul (e1, e2)
let ( /. ) e1 e2 = Div (e1, e2)

module CollectIndicators : sig
  val top : strategy -> Tacaml.t list
end = struct
  exception InvalidGADT

  (* Collect all t from GADT expressions *)
  let rec collect_data_types : type a. a expr -> Data.Type.t list = function
    | Var _ -> raise InvalidGADT
    | Const _
    | App1 _
    | App2 _
    | App3 _
    | Fun _
    | Symbol _
    | Float _
    | Int _
    | Bool _ ->
      []
    | Data data_type ->
      [
        ( eval_simple data_type |> function
          | Ok x -> x
          | Error e ->
            Eio.traceln "%a" Error.pp e;
            raise InvalidGADT );
      ]
    | Indicator data_type ->
      [
        ( eval_simple data_type |> function
          | Ok x -> Data.Type.Tacaml x
          | Error e ->
            Eio.traceln "%a" Error.pp e;
            raise InvalidGADT );
      ]
    | GT (e1, e2)
    | LT (e1, e2)
    | GTE (e1, e2)
    | LTE (e1, e2)
    | EQ (e1, e2) ->
      collect_data_types e1 @ collect_data_types e2
    | And (e1, e2)
    | Or (e1, e2) ->
      collect_data_types e1 @ collect_data_types e2
    | Not e -> collect_data_types e
    | Add (e1, e2)
    | Sub (e1, e2)
    | Mul (e1, e2)
    | Div (e1, e2) ->
      collect_data_types e1 @ collect_data_types e2
    | Moneyness (_, _) ->
      [ Data.Type.Close ] (* Uses Close price for moneyness calculation *)
    | Days_to_expiry _ ->
      [ Data.Type.Time ] (* Uses Time for expiry calculation *)
    | Lag (expr, _) ->
      collect_data_types expr (* Lag inherits types from wrapped expr *)
    | CrossUp (e1, e2)
    | CrossDown (e1, e2) ->
      collect_data_types e1 @ collect_data_types e2

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
  val top : strategy -> Strategy.builder
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
    match eval symbol strategy_expr data current_index with
    | Ok should_signal -> Result.return @@ Signal.make symbol should_signal
    | Error e -> Error e

  (* Convert GADT strategy to Template-compatible modules *)
  let gadt_to_buy_trigger (buy_expr : bool expr) (max_positions : int) =
    let module Buy_input : Template.Buy_trigger.INPUT = struct
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
    (module StrategyBuilder : Strategy.BUILDER)
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
    | Float _ -> []
    | Int _ -> []
    | Bool _ -> []
    | Const _ -> []
    | Symbol _ -> []
    | Data x -> collect_variables x
    | Indicator x -> collect_variables x
    | App1 (f, x) -> collect_variables f @ collect_variables x
    | App2 (f, x, y) ->
      collect_variables f @ collect_variables x @ collect_variables y
    | App3 (f, x, y, z) ->
      collect_variables f @ collect_variables x @ collect_variables y
      @ collect_variables z
    | Fun _ -> []
    | LT (x, y)
    | GT (x, y)
    | GTE (x, y)
    | LTE (x, y)
    | Add (x, y)
    | Sub (x, y)
    | Mul (x, y)
    | Div (x, y)
    | EQ (x, y) ->
      collect_variables x @ collect_variables y
    | And (x, y)
    | Or (x, y) ->
      collect_variables x @ collect_variables y
    | Not x -> collect_variables x
    | Moneyness _ -> []
    | Days_to_expiry _ -> []
    | Lag (a, _) -> collect_variables a
    | CrossUp (x, y) -> collect_variables x @ collect_variables y
    | CrossDown (x, y) -> collect_variables x @ collect_variables y

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

  let rec instantiate : type a. env -> a expr -> (a expr, Error.t) result =
    let ( let* ) = Result.( let* ) in
    fun env -> function
      | Var (id, ty) -> (
        match ty with
        | Float ->
          let* res = Bindings.get id env.float_map in
          Result.return @@ Float res
        | Int ->
          let* res = Bindings.get id env.int_map in
          Result.return @@ Int res)
      (* Literals - no variables, return as-is *)
      | Float f -> Result.return (Float f)
      | Int i -> Result.return (Int i)
      | Bool b -> Result.return (Bool b)
      | Const c -> Result.return (Const c)
      | Fun f -> Result.return (Fun f)
      | Symbol () -> Result.return (Symbol ())
      (* Application nodes - recursively instantiate arguments *)
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
      (* Data access - recursively instantiate the type expression *)
      | Data ty ->
        let* ty' = instantiate env ty in
        Result.return @@ Data ty'
      | Indicator ty ->
        let* ty' = instantiate env ty in
        Result.return @@ Indicator ty'
      (* Comparison operations - recursively instantiate both operands *)
      | GT (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ GT (e1', e2')
      | LT (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ LT (e1', e2')
      | GTE (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ GTE (e1', e2')
      | LTE (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ LTE (e1', e2')
      | EQ (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ EQ (e1', e2')
      (* Logical operations *)
      | And (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ And (e1', e2')
      | Or (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ Or (e1', e2')
      | Not e ->
        let* e' = instantiate env e in
        Result.return @@ Not e'
      (* Arithmetic operations *)
      | Add (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ Add (e1', e2')
      | Sub (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ Sub (e1', e2')
      | Mul (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ Mul (e1', e2')
      | Div (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ Div (e1', e2')
      (* Options-specific expressions - no variables to instantiate *)
      | Moneyness (underlying, option) ->
        Result.return @@ Moneyness (underlying, option)
      | Days_to_expiry option -> Result.return @@ Days_to_expiry option
      (* Lag expression - recursively instantiate the wrapped expression *)
      | Lag (expr, periods) ->
        let* expr' = instantiate env expr in
        Result.return @@ Lag (expr', periods)
      (* Crossover detection - recursively instantiate both expressions *)
      | CrossUp (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ CrossUp (e1', e2')
      | CrossDown (e1, e2) ->
        let* e1' = instantiate env e1 in
        let* e2' = instantiate env e2 in
        Result.return @@ CrossDown (e1', e2')

  (* let apply_subst (x : 'a expr) = *)
  (*   match collect_variables x with *)
  (*   | [] -> *)
end

let run bars (options : Options.t) mutices strategy =
  let tacaml_indicators = CollectIndicators.top strategy in
  let options =
    {
      options with
      flags = { options.flags with strategy_arg = strategy.name };
      tacaml_indicators;
    }
  in
  (* Collect custom indicators from the strategy *)
  let res = Strategy.run (Builder.top strategy) bars options mutices in
  res

exception OptimizationException

let opt bars options mutices (strategy : strategy) =
  let vars =
    Subst.collect_variables strategy.buy_trigger
    @ Subst.collect_variables strategy.sell_trigger
    |> Array.of_list
  in
  let len = Array.length vars in
  let opt = Nlopt.create Nlopt.neldermead len in
  let f (l : float array) _grad =
    let env =
      let open Subst in
      Array.foldi
        (fun env i (id, Type.A ty) ->
          match ty with
          | Type.Float ->
            { env with float_map = Bindings.add id l.(i) env.float_map }
          | Type.Int ->
            {
              env with
              int_map = Bindings.add id (Int.of_float l.(i)) env.int_map;
            })
        { float_map = Bindings.empty; int_map = Bindings.empty }
        vars
    in
    let strategy =
      {
        strategy with
        buy_trigger =
          ( Subst.instantiate env strategy.buy_trigger |> function
            | Ok x -> x
            | Error e ->
              Eio.traceln "%a" Error.pp e;
              raise OptimizationException );
        sell_trigger =
          ( Subst.instantiate env strategy.sell_trigger |> function
            | Ok x -> x
            | Error e ->
              Eio.traceln "%a" Error.pp e;
              raise OptimizationException );
      }
    in
    let res =
      run bars options mutices strategy |> function
      | Ok x -> x
      | Error e ->
        Eio.traceln "%a" Error.pp e;
        raise OptimizationException
    in
    Float.sub 1.0 res
  in
  Nlopt.set_lower_bounds opt @@ Array.init len (fun _ -> -100.0);
  Nlopt.set_upper_bounds opt @@ Array.init len (fun _ -> 100.0);
  Nlopt.set_maxeval opt 10;
  Nlopt.set_min_objective opt f;
  let start = Array.init len (fun _ -> 0.0) in
  let res, xopt, fopt = Nlopt.optimize opt start in
  Eio.traceln "optimization res: %s" (Nlopt.string_of_result res);
  Eio.traceln "%a : %f" (Array.pp Float.pp) xopt fopt;
  res
