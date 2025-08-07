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
type env = (Uuidm.t, const) List.Assoc.t
(* type _ arg = None : unit arg | Int : int arg *)

(* GADT AST with phantom types for compile-time type safety *)
type _ expr =
  (* Literals *)
  | Float : float -> float expr
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Const : 'a -> 'a expr
  (* Type-safe data access *)
  | Data : Data.Type.t expr -> float expr
  (* | Indicator : Tacaml.Indicator.t expr -> float expr *)
  | App : ('a -> 'b) expr * 'a expr -> 'b expr
  | Fun : ('a -> 'b) -> ('a -> 'b) expr
  | Var : Uuidm.t -> _ expr
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
  (* Custom indicator expressions *)
  (* | CustomIndicator : *)
  (*     Tacaml.Indicator.t *)
  (*     -> float expr (\* custom tacaml indicator *\) *)
  (* Lag expressions for historical data access *)
  | Lag : 'a expr * int -> 'a expr (* Access data N periods ago *)
  (* Crossover detection *)
  | CrossUp :
      float expr * float expr
      -> bool expr (* line1 crosses above line2 *)
  | CrossDown :
      float expr * float expr
      -> bool expr (* line1 crosses below line2 *)

(* let indicator f x = Indicator (App (Fun f, x)) *)

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
    | App (f, x) ->
      let* f = eval symbol f data index in
      let* arg = eval symbol x data index in
      let res = f arg in
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
    (* | Indicator ty -> *)
    (*   let* ty = eval symbol ty data index in *)
    (*   Error.guard (Error.fatal "Error in GADT evaluation at Data node") *)
    (*   @@ fun () -> Data.get data (Tacaml ty) index *)
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
    (* | CustomIndicator indicator -> *)
    (*   Error.guard (Error.fatal "GADT.eval custom_indicator") @@ fun () -> *)
    (*   Data.get data (Data.Type.Tacaml indicator) index *)
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
  | App (f, x) ->
    let* f = eval f in
    let* arg = eval x in
    let res = f arg in
    Result.return res
  | Symbol () -> Error.fatal "Symbol unknown in eval_simple"
  | Float f -> Result.return f
  | Var _ -> invalid_arg "Cannot evalute gadts with variables in them"
  | Int i -> Result.return i
  | Bool b -> Result.return b
  | Data _ -> Error.fatal "Cannot evaluate Data in eval_simple"
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

(* module Defaults = struct *)
(*   module I = Tacaml.Indicator *)

(*   let data x = Data (Const x) *)

(*   open Data.Type *)
(*   open Tacaml.Indicator *)

(*   (\* Smart constructors for OHLCV data - these are always floats *\) *)
(*   let close = data Close *)
(*   let open_ = data Open *)
(*   let high = data High *)
(*   let low = data Low *)
(*   let volume = data Volume *)

(*   (\* Smart constructors for float indicators *\) *)
(*   let rsi = data (Tacaml (rsi ())) *)
(*   let sma = data (Tacaml (sma ())) *)
(*   let ema = data (Tacaml (ema ())) *)
(*   let adx = data (Tacaml (adx ())) *)
(*   let atr = data (Tacaml (atr ())) *)
(*   let volume_sma = data (Tacaml (I.sma ~timeperiod:20 ())) *)
(*   let macd = data (Tacaml (macd_macd ())) *)
(*   let macd_signal = data (Tacaml (macd_signal ())) *)
(*   let macd_hist = data (Tacaml (macd_hist ())) *)
(*   let bb_upper = data (Tacaml (upper_bband ())) *)
(*   let bb_lower = data (Tacaml (lower_bband ())) *)
(*   let bb_middle = data (Tacaml (middle_bband ())) *)
(*   let willr = data (Tacaml (willr ())) *)
(*   let stoch_k = data (Tacaml (stoch_slow_k ())) *)
(*   let stoch_d = data (Tacaml (stoch_slow_d ())) *)

(*   (\* Smart constructors for integer indicators - candlestick patterns *\) *)
(*   let hammer = data (Tacaml (cdl_hammer ())) *)
(*   let doji = data (Tacaml (cdl_doji ())) *)
(*   let engulfing = data (Tacaml (cdl_engulfing ())) *)
(*   let morning_star = data (Tacaml (cdl_morningstar ())) *)
(*   let evening_star = data (Tacaml (cdl_eveningstar ())) *)
(*   let shooting_star = data (Tacaml (cdl_shootingstar ())) *)
(*   let hanging_man = data (Tacaml (cdl_hangingman ())) *)
(*   let piercing = data (Tacaml (cdl_piercing ())) *)
(*   let dark_cloud = data (Tacaml (cdl_darkcloudcover ())) *)

(*   (\* Additional candlestick patterns for testing *\) *)
(*   let inverted_hammer = data (Tacaml (cdl_invertedhammer ())) *)
(*   let dragonfly_doji = data (Tacaml (cdl_dragonflydoji ())) *)
(*   let gravestone_doji = data (Tacaml (cdl_gravestonedoji ())) *)
(*   let three_white_soldiers = data (Tacaml (cdl_3whitesoldiers ())) *)
(*   let three_black_crows = data (Tacaml (cdl_3blackcrows ())) *)
(*   let belt_hold = data (Tacaml (cdl_belthold ())) *)
(*   let abandoned_baby = data (Tacaml (cdl_abandonedbaby ())) *)
(*   let harami = data (Tacaml (cdl_harami ())) *)
(*   let harami_cross = data (Tacaml (cdl_haramicross ())) *)
(* end *)

(* include Defaults *)

(* Options smart constructors *)
let moneyness underlying option = Moneyness (underlying, option)
let days_to_expiry option = Days_to_expiry option

(* Custom indicator smart constructor *)
(* let custom_indicator indicator = CustomIndicator indicator *)

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

(* (\* Example strategies *\) *)
(* let rsi_mean_reversion = *)
(*   { *)
(*     name = "RSI Mean Reversion"; *)
(*     buy_trigger = rsi <. Float 30.0 &&. (close >. sma); *)
(*     sell_trigger = rsi >. Float 70.0 ||. (close <. sma *. Float 0.95); *)
(*     max_positions = 3; *)
(*     position_size = 0.33; *)
(*   } *)

(* let macd_bollinger_momentum = *)
(*   { *)
(*     name = "MACD Bollinger Momentum"; *)
(*     buy_trigger = *)
(*       macd >. macd_signal *)
(*       &&. (close <. bb_lower +. ((bb_upper -. bb_lower) *. Float 0.2)) *)
(*       &&. (adx >. Float 25.0) &&. (macd_hist >. Float 0.0); *)
(*     sell_trigger = *)
(*       macd <. macd_signal ||. (close >. bb_upper) *)
(*       ||. (close <. sma *. Float 0.92); *)
(*     max_positions = 5; *)
(*     position_size = 0.2; *)
(*   } *)

(* let candlestick_patterns_strategy = *)
(*   { *)
(*     name = "Candlestick Patterns"; *)
(*     buy_trigger = *)
(*       hammer >. Float 0.0 &&. (rsi <. Float 40.0) *)
(*       &&. (volume >. volume_sma *. Float 1.3); *)
(*     sell_trigger = engulfing <. Float 0.0 ||. (close >. sma *. Float 1.1); *)
(*     max_positions = 6; *)
(*     position_size = 0.16; *)
(*   } *)

module CollectIndicators : sig
  val top : strategy -> Tacaml.t list
end = struct
  exception InvalidGADT

  (* Collect all t from GADT expressions *)
  let rec collect_data_types : type a. a expr -> Data.Type.t list = function
    | Var _
    | Const _
    | App _
    | Fun _
    | Symbol _
    | Float _
    | Int _
    | Bool _ ->
      []
    | Data data_type -> (
      match eval_simple data_type with
      | Ok x -> [ x ]
      | _ -> raise InvalidGADT)
    (* | Indicator i -> ( *)
    (*   match eval_simple i with *)
    (*   | Ok x -> [ Tacaml x ] *)
    (*   | _ -> raise InvalidGADT) *)
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
    (* | CustomIndicator indicator -> [ Data.Type.Tacaml indicator ] *)
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
  val top : strategy -> (module Strategy.BUILDER)
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
