open Longleaf_lib
module Data = Bars.Data

(* Phantom types for data return types *)
type _ data_type =
  | Float_type : Data.Type.t -> float data_type
  | Int_type : Data.Type.t -> int data_type

(* GADT AST with phantom types for compile-time type safety *)
type _ expr =
  (* Literals *)
  | Float : float -> float expr
  | Int : int -> int expr
  | Bool : bool -> bool expr
  (* Type-safe data access *)
  | Data : 'a data_type -> 'a expr
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

(* Strategy structure *)
type strategy = {
  name : string;
  buy_trigger : bool expr;
  sell_trigger : bool expr;
  max_positions : int;
  position_size : float;
}

(* Type-safe evaluation *)
let rec eval : type a. a expr -> Data.t -> int -> (a, Error.t) result =
 fun expr data index ->
  let ( let* ) = Result.( let* ) in
  match expr with
  | Float f -> Result.return f
  | Int i -> Result.return i
  | Bool b -> Result.return b
  | Data (Float_type data_type) ->
    Error.guard (Error.fatal "GADT.eval float data") @@ fun () ->
    Data.get data data_type index
  | Data (Int_type data_type) ->
    Error.guard (Error.fatal "GADT.eval int data") @@ fun () ->
    Data.get_top_int data data_type
  | GT (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 >. v2)
  | LT (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 <. v2)
  | GTE (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 >=. v2)
  | LTE (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 <=. v2)
  | EQ (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (Float.equal v1 v2)
  | IntGT (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 > v2)
  | IntLT (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 < v2)
  | IntEQ (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 = v2)
  | And (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 && v2)
  | Or (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 || v2)
  | Not e ->
    let* v = eval e data index in
    Result.return (not v)
  | Add (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 +. v2)
  | Sub (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 -. v2)
  | Mul (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    Result.return (v1 *. v2)
  | Div (e1, e2) ->
    let* v1 = eval e1 data index in
    let* v2 = eval e2 data index in
    if Float.equal v2 0.0 then Error.fatal "Division by zero in GADT.eval"
    else Result.return (v1 /. v2)

(* Smart constructors for OHLCV data - these are always floats *)
let close = Data (Float_type Data.Type.Close)
let open_ = Data (Float_type Data.Type.Open)
let high = Data (Float_type Data.Type.High)
let low = Data (Float_type Data.Type.Low)
let volume = Data (Float_type Data.Type.Volume)

(* Smart constructors for float indicators *)
let rsi = Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Rsi)))
let sma = Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Sma)))
let ema = Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Ema)))
let adx = Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Adx)))
let atr = Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Atr)))

let macd =
  Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Macd_MACD)))

let macd_signal =
  Data
    (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Macd_MACDSignal)))

let macd_hist =
  Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Macd_MACDHist)))

let bb_upper =
  Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.UpperBBand)))

let bb_lower =
  Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.LowerBBand)))

let bb_middle =
  Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.MiddleBBand)))

let willr =
  Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Willr)))

let stoch_k =
  Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Stoch_SlowK)))

let stoch_d =
  Data (Float_type (Data.Type.Tacaml (F Tacaml.Indicator.Float.Stoch_SlowD)))

(* Smart constructors for integer indicators - candlestick patterns *)
let hammer =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlHammer)))

let doji = Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlDoji)))

let engulfing =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlEngulfing)))

let morning_star =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlMorningStar)))

let evening_star =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlEveningStar)))

let shooting_star =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlShootingStar)))

let hanging_man =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlHangingMan)))

let piercing =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlPiercing)))

let dark_cloud =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlDarkCloudCover)))

(* Additional candlestick patterns for testing *)
let inverted_hammer =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlInvertedHammer)))

let dragonfly_doji =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlDragonflyDoji)))

let gravestone_doji =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlGravestoneDoji)))

let three_white_soldiers =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.Cdl3WhiteSoldiers)))

let three_black_crows =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.Cdl3BlackCrows)))

let belt_hold =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlBeltHold)))

let abandoned_baby =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlAbandonedBaby)))

let harami =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlHarami)))

let harami_cross =
  Data (Int_type (Data.Type.Tacaml (I Tacaml.Indicator.Int.CdlHaramiCross)))

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
    sell_trigger = rsi >. Float 70.0 ||. (close <. close *. Float 0.95);
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
      ||. (close <. close *. Float 0.92);
    max_positions = 5;
    position_size = 0.2;
  }

let candlestick_patterns_strategy =
  {
    name = "Candlestick Patterns";
    buy_trigger =
      IntGT (hammer, Int 0)
      &&. (rsi <. Float 40.0)
      &&. (volume >. volume *. Float 1.3);
    sell_trigger = IntLT (engulfing, Int 0) ||. (close >. close *. Float 1.1);
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
  match eval strategy_expr data current_index with
  | Ok should_signal -> Result.return @@ Signal.make symbol should_signal
  | Error e -> Error e

(* Convert GADT strategy to Template-compatible modules *)
let gadt_to_buy_trigger (buy_expr : bool expr) =
  let module Buy_input : Template.Buy_trigger.INPUT = struct
    let pass state symbol = eval_strategy_signal buy_expr state symbol
    let score _state _symbol = Result.return 1.0
    let num_positions = 5
  end in
  let module Buy_trigger = Template.Buy_trigger.Make (Buy_input) in
  (module Buy_trigger : Template.Buy_trigger.S)

let gadt_to_sell_trigger (sell_expr : bool expr) =
  let module Sell_impl : Template.Sell_trigger.S = struct
    let make state ~buying_order =
      let symbol = buying_order.Order.symbol in
      eval_strategy_signal sell_expr state symbol
  end in
  (module Sell_impl : Template.Sell_trigger.S)

let gadt_to_strategy_builder (strategy : strategy) =
  let buy_trigger = gadt_to_buy_trigger strategy.buy_trigger in
  let sell_trigger = gadt_to_sell_trigger strategy.sell_trigger in
  let module StrategyBuilder =
    Template.Make ((val buy_trigger)) ((val sell_trigger))
  in
  (module StrategyBuilder : Strategy.BUILDER)

let run (options : Options.t) strategy =
  let options =
    { options with flags = { options.flags with strategy_arg = strategy.name } }
  in
  let res = Strategy.run (gadt_to_strategy_builder strategy) options in
  res
