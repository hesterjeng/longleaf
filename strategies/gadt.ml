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

(* Type witness for value types *)
type _ ty =
  | Float_type : float ty
  | Int_type : int ty

(* GADT AST with phantom types for compile-time type safety *)
type _ t =
  (* Literals *)
  | Float : float -> float t
  | Int : int -> int t
  | Bool : bool -> bool t
  (* Type-safe data access *)
  | Data : 'a ty * Data.Type.t -> 'a t
  | Symbol : unit -> Instrument.t t
  (* Comparisons *)
  | GT : 'a ty * 'a t * 'a t -> bool t
  | LT : 'a ty * 'a t * 'a t -> bool t
  | GTE : 'a ty * 'a t * 'a t -> bool t
  | LTE : 'a ty * 'a t * 'a t -> bool t
  | EQ : 'a ty * 'a t * 'a t -> bool t
  (* Logical operations *)
  | And : bool t * bool t -> bool t
  | Or : bool t * bool t -> bool t
  | Not : bool t -> bool t
  (* Basic arithmetic *)
  | Add : float t * float t -> float t
  | Sub : float t * float t -> float t
  | Mul : float t * float t -> float t
  | Div : float t * float t -> float t
  (* Options-specific expressions *)
  | Moneyness :
      Instrument.t * Instrument.t
      -> float t (* underlying * option *)
  | Days_to_expiry : Instrument.t -> int t (* option *)
  (* Custom indicator expressions *)
  | CustomIndicator :
      Tacaml.Indicator.t
      -> float t (* custom tacaml indicator *)
  (* Lag expressions for historical data access *)
  | Lag : 'a t * int -> 'a t (* Access data N periods ago *)
  (* Crossover detection *)
  | CrossUp :
      float t * float t
      -> bool t (* line1 crosses above line2 *)
  | CrossDown :
      float t * float t
      -> bool t (* line1 crosses below line2 *)

(* Strategy structure *)
type strategy = {
  name : string;
  buy_trigger : bool t;
  sell_trigger : bool t;
  max_positions : int;
  position_size : float;
}

(* Type-safe evaluation *)
let rec eval : type a.
    Instrument.t -> a t -> Data.t -> int -> (a, Error.t) result =
 fun symbol expr data index ->
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
    | Data (Float_type, data_type) ->
      Error.guard (Error.fatal "GADT.eval float data") @@ fun () ->
      Data.get data data_type index
    | Data (Int_type, data_type) ->
      Error.guard (Error.fatal "GADT.eval int data") @@ fun () ->
      Int.of_float (Data.get data data_type index)
    | GT (Float_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 >. v2)
    | GT (Int_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 > v2)
    | LT (Float_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 <. v2)
    | LT (Int_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 < v2)
    | GTE (Float_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 >=. v2)
    | GTE (Int_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 >= v2)
    | LTE (Float_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 <=. v2)
    | LTE (Int_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 <= v2)
    | EQ (Float_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (Float.equal v1 v2)
    | EQ (Int_type, e1, e2) ->
      let* v1 = eval symbol e1 data index in
      let* v2 = eval symbol e2 data index in
      Result.return (v1 = v2)
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

(* Smart constructors for OHLCV data - these are always floats *)
let close = Data (Float_type, Data.Type.Close)
let open_ = Data (Float_type, Data.Type.Open)
let high = Data (Float_type, Data.Type.High)
let low = Data (Float_type, Data.Type.Low)
let volume = Data (Float_type, Data.Type.Volume)

(* Smart constructors for float indicators *)
let rsi = Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.rsi ()))
let sma = Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.sma ()))
let ema = Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.ema ()))
let adx = Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.adx ()))
let atr = Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.atr ()))

let volume_sma =
  Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.sma ~timeperiod:20 ()))

let macd = Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.macd_macd ()))

let macd_signal =
  Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.macd_signal ()))

let macd_hist =
  Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.macd_hist ()))

let bb_upper =
  Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.upper_bband ()))

let bb_lower =
  Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.lower_bband ()))

let bb_middle =
  Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.middle_bband ()))

let willr = Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.willr ()))

let stoch_k =
  Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.stoch_slow_k ()))

let stoch_d =
  Data (Float_type, Data.Type.Tacaml (Tacaml.Indicator.stoch_slow_d ()))

(* Smart constructors for integer indicators - candlestick patterns *)
let hammer = Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_hammer ()))
let doji = Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_doji ()))

let engulfing =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_engulfing ()))

let morning_star =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_morningstar ()))

let evening_star =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_eveningstar ()))

let shooting_star =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_shootingstar ()))

let hanging_man =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_hangingman ()))

let piercing =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_piercing ()))

let dark_cloud =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_darkcloudcover ()))

(* Additional candlestick patterns for testing *)
let inverted_hammer =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_invertedhammer ()))

let dragonfly_doji =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_dragonflydoji ()))

let gravestone_doji =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_gravestonedoji ()))

let three_white_soldiers =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_3whitesoldiers ()))

let three_black_crows =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_3blackcrows ()))

let belt_hold =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_belthold ()))

let abandoned_baby =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_abandonedbaby ()))

let harami = Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_harami ()))

let harami_cross =
  Data (Int_type, Data.Type.Tacaml (Tacaml.Indicator.cdl_haramicross ()))

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
let ( >. ) e1 e2 = GT (Float_type, e1, e2)
let ( <. ) e1 e2 = LT (Float_type, e1, e2)
let ( >=. ) e1 e2 = GTE (Float_type, e1, e2)
let ( <=. ) e1 e2 = LTE (Float_type, e1, e2)
let ( =. ) e1 e2 = EQ (Float_type, e1, e2)
let ( > ) e1 e2 = GT (Int_type, e1, e2)
let ( < ) e1 e2 = LT (Int_type, e1, e2)
let ( >= ) e1 e2 = GTE (Int_type, e1, e2)
let ( <= ) e1 e2 = LTE (Int_type, e1, e2)
let ( = ) e1 e2 = EQ (Int_type, e1, e2)
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
      (hammer > Int 0)
      &&. (rsi <. Float 40.0)
      &&. (volume >. volume_sma *. Float 1.3);
    sell_trigger = (engulfing < Int 0) ||. (close >. sma *. Float 1.1);
    max_positions = 6;
    position_size = 0.16;
  }

(* Helper function to evaluate strategy triggers *)
let eval_strategy_signal (strategy_expr : bool t) (state : _ State.t) symbol
    =
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
let gadt_to_buy_trigger (buy_expr : bool t) (max_positions : int) =
  let module Buy_input : Template.Buy_trigger.INPUT = struct
    let pass state symbol = eval_strategy_signal buy_expr state symbol
    let score _state _symbol = Result.return 1.0
    let num_positions = max_positions
  end in
  let module Buy_trigger = Template.Buy_trigger.Make (Buy_input) in
  (module Buy_trigger : Template.Buy_trigger.S)

let gadt_to_sell_trigger (sell_expr : bool t) =
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
let rec collect_data_types : type a. a t -> Data.Type.t list = function
  | Symbol _
  | Float _
  | Int _
  | Bool _ ->
    []
  | Data (_, data_type) -> [ data_type ]
  | GT (_, e1, e2) ->
    collect_data_types e1 @ collect_data_types e2
  | LT (_, e1, e2) ->
    collect_data_types e1 @ collect_data_types e2
  | GTE (_, e1, e2) ->
    collect_data_types e1 @ collect_data_types e2
  | LTE (_, e1, e2) ->
    collect_data_types e1 @ collect_data_types e2
  | EQ (_, e1, e2) ->
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
let rec collect_custom_indicators : type a. a t -> Tacaml.Indicator.t list =
  function
  | Symbol _
  | Float _
  | Int _
  | Bool _ ->
    []
  | Data _ -> []
  | GT (_, e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  | LT (_, e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  | GTE (_, e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  | LTE (_, e1, e2) ->
    collect_custom_indicators e1 @ collect_custom_indicators e2
  | EQ (_, e1, e2) ->
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
