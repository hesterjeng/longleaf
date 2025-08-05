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

(* Phantom types for data return types *)
type _ data_type =
  | Float_type : Data.Type.t -> float data_type
  | Int_type : Data.Type.t -> int data_type

(* Function type GADT for De Bruijn lambda expressions *)
type (_, _) fn_type =
  | FloatToInt : (float, int) fn_type
  | FloatToFloat : (float, float) fn_type
  | IntToFloat : (int, float) fn_type
  | IntToInt : (int, int) fn_type
  | FloatIntToFloat : (float * int, float) fn_type
  | IntIntToFloat : (int * int, float) fn_type
  | BoolToBool : (bool, bool) fn_type
  | FloatToBool : (float, bool) fn_type
  | IntToBool : (int, bool) fn_type

(* Environment for De Bruijn index evaluation *)
type env_entry = Float_val of float | Int_val of int | Bool_val of bool
type env = env_entry list (* Stack: head is index 0 *)

(* GADT AST with phantom types for compile-time type safety *)
type _ expr =
  (* Literals *)
  | Float : float -> float expr
  | Int : int -> int expr
  | Bool : bool -> bool expr
  (* Type-safe data access *)
  | Data : 'a data_type -> 'a expr
  | Symbol : unit -> Instrument.t expr
  (* Comparisons *)
  | GT : float expr * float expr -> bool expr
  | LT : float expr * float expr -> bool expr
  | GTE : float expr * float expr -> bool expr
  | LTE : float expr * float expr -> bool expr
  | EQ : float expr * float expr -> bool expr
  | IntGT : int expr * int expr -> bool expr
  | IntLT : int expr * int expr -> bool expr
  | IntEQ : int expr * int expr -> bool expr
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
  (* Custom indicator expressions *)
  | CustomIndicator :
      Tacaml.Indicator.t
      -> float expr (* custom tacaml indicator *)
  (* Lag expressions for historical data access *)
  | Lag : 'a expr * int -> 'a expr (* Access data N periods ago *)
  (* Crossover detection *)
  | CrossUp :
      float expr * float expr
      -> bool expr (* line1 crosses above line2 *)
  | CrossDown :
      float expr * float expr
      -> bool expr (* line1 crosses below line2 *)
  (* De Bruijn lambda expressions *)
  | Var : int -> 'a expr (* De Bruijn index - 0 is innermost binding *)
  | Lambda :
      ('a, 'b) fn_type * 'b expr
      -> ('a -> 'b) expr (* Lambda abstraction *)
  | App : ('a -> 'b) expr * 'a expr -> 'b expr (* Function application *)
  | ExtFun :
      ('a, 'b) fn_type * string * ('a -> 'b)
      -> ('a -> 'b) expr (* External function *)

(* Strategy structure *)
type strategy = {
  name : string;
  buy_trigger : bool expr;
  sell_trigger : bool expr;
  max_positions : int;
  position_size : float;
}

(* De Bruijn index shifting for lambda scope management *)
let rec shift_expr : type a. int -> int -> a expr -> a expr =
 fun cutoff delta expr ->
  match expr with
  | Var n -> Var (if n >= cutoff then n + delta else n)
  | Lambda (fn_type, body) ->
    Lambda (fn_type, shift_expr (cutoff + 1) delta body)
  | App (fn_expr, arg_expr) ->
    App (shift_expr cutoff delta fn_expr, shift_expr cutoff delta arg_expr)
  | ExtFun (fn_type, name, f) ->
    ExtFun (fn_type, name, f) (* External functions are closed *)
  (* All other cases just recurse *)
  | Symbol () -> Symbol ()
  | Float f -> Float f
  | Int i -> Int i
  | Bool b -> Bool b
  | Data dt -> Data dt
  | GT (e1, e2) -> GT (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | LT (e1, e2) -> LT (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | GTE (e1, e2) -> GTE (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | LTE (e1, e2) -> LTE (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | EQ (e1, e2) -> EQ (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | IntGT (e1, e2) ->
    IntGT (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | IntLT (e1, e2) ->
    IntLT (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | IntEQ (e1, e2) ->
    IntEQ (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | And (e1, e2) -> And (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | Or (e1, e2) -> Or (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | Not e -> Not (shift_expr cutoff delta e)
  | Add (e1, e2) -> Add (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | Sub (e1, e2) -> Sub (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | Mul (e1, e2) -> Mul (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | Div (e1, e2) -> Div (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | Moneyness (underlying, option) -> Moneyness (underlying, option)
  | Days_to_expiry option -> Days_to_expiry option
  | CustomIndicator indicator -> CustomIndicator indicator
  | Lag (expr, periods) -> Lag (shift_expr cutoff delta expr, periods)
  | CrossUp (e1, e2) ->
    CrossUp (shift_expr cutoff delta e1, shift_expr cutoff delta e2)
  | CrossDown (e1, e2) ->
    CrossDown (shift_expr cutoff delta e1, shift_expr cutoff delta e2)

(* Substitution for beta reduction - this is tricky with GADTs *)
(* We'll implement a simpler version that works for our use case *)
let substitute_var : type a b. int -> a expr -> b expr -> b expr =
 fun _var_index _replacement expr ->
  (* For now, we'll use a simple approach that doesn't do full substitution *)
  (* This will be expanded when we actually need complex lambda calculus *)
  expr

(* Type-safe evaluation *)
let rec eval : type a.
    env -> Instrument.t -> a expr -> Data.t -> int -> (a, Error.t) result =
 fun env symbol expr data index ->
  let ( let* ) = Result.( let* ) in
  (* Bounds checking *)
  if index < 0 || index >= Data.length data then
    Error.fatal
      (Printf.sprintf "GADT.eval: index %d out of bounds (data length: %d)"
         index (Data.length data))
  else
    match expr with
    | Symbol () -> Result.return symbol
    | Float f -> Result.return f
    | Int i -> Result.return i
    | Bool b -> Result.return b
    | Data (Float_type data_type) ->
      Error.guard (Error.fatal "GADT.eval float data") @@ fun () ->
      Data.get data data_type index
    | Data (Int_type data_type) ->
      Error.guard (Error.fatal "GADT.eval int data") @@ fun () ->
      Int.of_float (Data.get data data_type index)
    | GT (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 >. v2)
    | LT (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 <. v2)
    | GTE (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 >=. v2)
    | LTE (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 <=. v2)
    | EQ (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (Float.equal v1 v2)
    | IntGT (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 > v2)
    | IntLT (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 < v2)
    | IntEQ (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 = v2)
    | And (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 && v2)
    | Or (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 || v2)
    | Not e ->
      let* v = eval env symbol e data index in
      Result.return (not v)
    | Add (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 +. v2)
    | Sub (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 -. v2)
    | Mul (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
      Result.return (v1 *. v2)
    | Div (e1, e2) ->
      let* v1 = eval env symbol e1 data index in
      let* v2 = eval env symbol e2 data index in
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
    | CustomIndicator indicator ->
      Error.guard (Error.fatal "GADT.eval custom_indicator") @@ fun () ->
      Data.get data (Data.Type.Tacaml indicator) index
    | Lag (expr, periods) ->
      let lag_index = index - periods in
      if lag_index < 0 then
        Error.fatal
          (Printf.sprintf
             "GADT.eval: lag index %d out of bounds (periods: %d, current \
              index: %d)"
             lag_index periods index)
      else eval env symbol expr data lag_index
    | CrossUp (e1, e2) ->
      if index = 0 then
        Error.fatal "GADT.eval: CrossUp requires at least 1 historical period"
      else
        let* current_e1 = eval env symbol e1 data index in
        let* current_e2 = eval env symbol e2 data index in
        let* prev_e1 = eval env symbol e1 data (index - 1) in
        let* prev_e2 = eval env symbol e2 data (index - 1) in
        Result.return (prev_e1 <=. prev_e2 && current_e1 >. current_e2)
    | CrossDown (e1, e2) ->
      if index = 0 then
        Error.fatal "GADT.eval: CrossDown requires at least 1 historical period"
      else
        let* current_e1 = eval env symbol e1 data index in
        let* current_e2 = eval env symbol e2 data index in
        let* prev_e1 = eval env symbol e1 data (index - 1) in
        let* prev_e2 = eval env symbol e2 data (index - 1) in
        Result.return (prev_e1 >=. prev_e2 && current_e1 <. current_e2)
    (* De Bruijn lambda expression cases *)
    | Var db_index -> (
      match List.get_at_idx db_index env with
      | Some entry -> (
        match entry with
        | Float_val f -> Result.return (Obj.magic f : a)
        | Int_val i -> Result.return (Obj.magic i : a)
        | Bool_val b -> Result.return (Obj.magic b : a))
      | None ->
        Error.fatal
          (Printf.sprintf "Unbound variable at De Bruijn index %d" db_index))
    | Lambda (_fn_type, _body) ->
      (* Return a closure - for now we'll represent it as an OCaml function *)
      (* This is complex with GADTs - we'll use a simpler approach *)
      Error.fatal "Lambda evaluation not yet implemented"
    | App (_fn_expr, _arg_expr) ->
      (* Function application - also complex with our current setup *)
      Error.fatal "Application evaluation not yet implemented"
    | ExtFun (_fn_type, _name, f) ->
      (* External functions return the OCaml function directly *)
      Result.return f

(* Smart constructors for OHLCV data - these are always floats *)
let close = Data (Float_type Data.Type.Close)
let open_ = Data (Float_type Data.Type.Open)
let high = Data (Float_type Data.Type.High)
let low = Data (Float_type Data.Type.Low)
let volume = Data (Float_type Data.Type.Volume)

(* Smart constructors for float indicators *)
let rsi = Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.rsi ())))
let sma = Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.sma ())))
let ema = Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.ema ())))
let adx = Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.adx ())))
let atr = Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.atr ())))

let volume_sma =
  Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.sma ~timeperiod:20 ())))

let macd = Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.macd_macd ())))

let macd_signal =
  Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.macd_signal ())))

let macd_hist =
  Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.macd_hist ())))

let bb_upper =
  Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.upper_bband ())))

let bb_lower =
  Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.lower_bband ())))

let bb_middle =
  Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.middle_bband ())))

let willr = Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.willr ())))

let stoch_k =
  Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.stoch_slow_k ())))

let stoch_d =
  Data (Float_type (Data.Type.Tacaml (Tacaml.Indicator.stoch_slow_d ())))

(* Smart constructors for integer indicators - candlestick patterns *)
let hammer = Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_hammer ())))
let doji = Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_doji ())))

let engulfing =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_engulfing ())))

let morning_star =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_morningstar ())))

let evening_star =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_eveningstar ())))

let shooting_star =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_shootingstar ())))

let hanging_man =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_hangingman ())))

let piercing =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_piercing ())))

let dark_cloud =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_darkcloudcover ())))

(* Additional candlestick patterns for testing *)
let inverted_hammer =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_invertedhammer ())))

let dragonfly_doji =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_dragonflydoji ())))

let gravestone_doji =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_gravestonedoji ())))

let three_white_soldiers =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_3whitesoldiers ())))

let three_black_crows =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_3blackcrows ())))

let belt_hold =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_belthold ())))

let abandoned_baby =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_abandonedbaby ())))

let harami = Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_harami ())))

let harami_cross =
  Data (Int_type (Data.Type.Tacaml (Tacaml.Indicator.cdl_haramicross ())))

(* Options smart constructors *)
let moneyness underlying option = Moneyness (underlying, option)
let days_to_expiry option = Days_to_expiry option

(* Custom indicator smart constructor *)
let custom_indicator indicator = CustomIndicator indicator

(* Lag and crossover smart constructors *)
let lag expr periods = Lag (expr, periods)
let cross_up e1 e2 = CrossUp (e1, e2)
let cross_down e1 e2 = CrossDown (e1, e2)

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

(* Example strategies *)
let rsi_mean_reversion =
  {
    name = "RSI Mean Reversion";
    buy_trigger = rsi <. Float 30.0 &&. (close >. sma);
    sell_trigger = rsi >. Float 70.0 ||. (close <. sma *. Float 0.95);
    max_positions = 3;
    position_size = 0.33;
  }

let macd_bollinger_momentum =
  {
    name = "MACD Bollinger Momentum";
    buy_trigger =
      macd >. macd_signal
      &&. (close <. bb_lower +. ((bb_upper -. bb_lower) *. Float 0.2))
      &&. (adx >. Float 25.0) &&. (macd_hist >. Float 0.0);
    sell_trigger =
      macd <. macd_signal ||. (close >. bb_upper)
      ||. (close <. sma *. Float 0.92);
    max_positions = 5;
    position_size = 0.2;
  }

let candlestick_patterns_strategy =
  {
    name = "Candlestick Patterns";
    buy_trigger =
      IntGT (hammer, Int 0)
      &&. (rsi <. Float 40.0)
      &&. (volume >. volume_sma *. Float 1.3);
    sell_trigger = IntLT (engulfing, Int 0) ||. (close >. sma *. Float 1.1);
    max_positions = 6;
    position_size = 0.16;
  }

(* Helper function to evaluate strategy triggers *)
let eval_strategy_signal (strategy_expr : bool expr) (state : _ State.t) symbol
    =
  let ( let* ) = Result.( let* ) in
  let* data =
    State.data state symbol
    (* State.get_bars state symbol *)
  in
  let current_index = State.tick state in
  let empty_env = [] in
  (* Start with empty environment for now *)
  match eval empty_env symbol strategy_expr data current_index with
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

let gadt_to_strategy_builder (strategy : strategy) =
  let buy_trigger =
    gadt_to_buy_trigger strategy.buy_trigger strategy.max_positions
  in
  let sell_trigger = gadt_to_sell_trigger strategy.sell_trigger in
  let module StrategyBuilder =
    Template.Make ((val buy_trigger)) ((val sell_trigger))
  in
  (module StrategyBuilder : Strategy.BUILDER)

(* Collect all Data.Type.t from GADT expressions *)
let rec collect_data_types : type a. a expr -> Data.Type.t list = function
  | Symbol _
  | Float _
  | Int _
  | Bool _ ->
    []
  | Data (Float_type data_type) -> [ data_type ]
  | Data (Int_type data_type) -> [ data_type ]
  | GT (e1, e2)
  | LT (e1, e2)
  | GTE (e1, e2)
  | LTE (e1, e2)
  | EQ (e1, e2) ->
    collect_data_types e1 @ collect_data_types e2
  | IntGT (e1, e2)
  | IntLT (e1, e2)
  | IntEQ (e1, e2) ->
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
  | CustomIndicator indicator -> [ Data.Type.Tacaml indicator ]
  | Lag (expr, _) ->
    collect_data_types expr (* Lag inherits types from wrapped expr *)
  | CrossUp (e1, e2)
  | CrossDown (e1, e2) ->
    collect_data_types e1 @ collect_data_types e2
  (* De Bruijn lambda expressions don't contribute data types directly *)
  | Var _ -> [] (* Variables get their types from the environment at runtime *)
  | Lambda (_, body) -> collect_data_types body
  | App (fn_expr, arg_expr) ->
    collect_data_types fn_expr @ collect_data_types arg_expr
  | ExtFun (_, _, _) -> [] (* External functions don't access data directly *)

let collect_strategy_data_types (strategy : strategy) : Data.Type.t list =
  let buy_data_types = collect_data_types strategy.buy_trigger in
  let sell_data_types = collect_data_types strategy.sell_trigger in
  (* Remove duplicates using sort_uniq with polymorphic compare *)
  let all_data_types = buy_data_types @ sell_data_types in
  List.sort_uniq ~cmp:Stdlib.compare all_data_types

let collect_tacaml_data_types (strategy : strategy) =
  let res = collect_strategy_data_types strategy in
  List.filter_map
    (fun x ->
      match x with
      | Data.Type.Tacaml x -> Some x
      | _ -> None)
    res

(* Collect all custom Tacaml.t indicators from GADT expressions *)
let rec collect_custom_indicators : type a. a expr -> Tacaml.Indicator.t list =
  function
  | Symbol _
  | Float _
  | Int _
  | Bool _ ->
    []
  | Data _ -> []
  | GT (e1, e2)
  | LT (e1, e2)
  | GTE (e1, e2)
  | LTE (e1, e2)
  | EQ (e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  | IntGT (e1, e2)
  | IntLT (e1, e2)
  | IntEQ (e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  | And (e1, e2)
  | Or (e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  | Not e -> collect_custom_indicators e
  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  | Moneyness (_, _) -> [] (* Options don't use custom indicators directly *)
  | Days_to_expiry _ -> [] (* Options don't use custom indicators directly *)
  | CustomIndicator indicator -> [ indicator ]
  | Lag (expr, _) ->
    collect_custom_indicators
      expr (* Lag inherits indicators from wrapped expr *)
  | CrossUp (e1, e2)
  | CrossDown (e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  (* De Bruijn lambda expressions *)
  | Var _ -> [] (* Variables don't contain custom indicators directly *)
  | Lambda (_, body) -> collect_custom_indicators body
  | App (fn_expr, arg_expr) ->
    collect_custom_indicators fn_expr @ collect_custom_indicators arg_expr
  | ExtFun (_, _, _) ->
    [] (* External functions don't use custom indicators directly *)

let collect_strategy_custom_indicators (strategy : strategy) :
    Tacaml.Indicator.t list =
  let buy_indicators = collect_custom_indicators strategy.buy_trigger in
  let sell_indicators = collect_custom_indicators strategy.sell_trigger in
  (* Remove duplicates using sort_uniq with polymorphic compare *)
  let all_indicators = buy_indicators @ sell_indicators in
  List.sort_uniq ~cmp:Stdlib.compare all_indicators

let indicators strategy =
  collect_strategy_data_types strategy
  |> List.filter_map (function
       | Data.Type.Tacaml x -> Some x
       | _ -> None)
  |> List.map Tacaml.Conv.indicator_to_safe
  |> List.uniq ~eq:Equal.poly

let run bars (options : Options.t) mutices strategy =
  let tacaml_indicators = indicators strategy in
  let options =
    {
      options with
      flags = { options.flags with strategy_arg = strategy.name };
      tacaml_indicators;
    }
  in
  (* Collect custom indicators from the strategy *)
  let res =
    Strategy.run (gadt_to_strategy_builder strategy) bars options mutices
  in
  res
