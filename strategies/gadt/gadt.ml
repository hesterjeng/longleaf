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

type const = VFloat of float | VInt of int
(* type env = (Uuidm.t, _ const) List.Assoc.t *)

(* Bounds for optimizer variables *)
type bounds = { lower : float; upper : float }

module Type = struct
  type _ t =
    | Bool : bool t
    | Float : float t
    | Int : int t
    | Data : Data.Type.t t
    | Tacaml : Tacaml.Indicator.t t

  type shadow = A : _ t -> shadow

  let shadow x = A x
end

type context = {
  instrument : Instrument.t;
  data : Data.t; (* Convenience: data for the current instrument *)
  bars : Bars.t;
      (* All symbols' data - enables pair trading and reliable timestamps *)
  index : int;
  orders : Order.t list;
      (* Order history for this instrument - for position risk management *)
  tick_time : float;
      (* Cached timestamp for current tick - computed once instead of per-symbol *)
  is_market_open : bool;
      (* Cached market open status - computed once per tick instead of per-symbol *)
  minutes_until_close : float;
      (* Cached minutes until close - computed once per tick instead of per-symbol *)
  (* Intraday session tracking - computed once at day boundary *)
  day_start_index : int;
      (* Index of first bar of current trading day *)
  prev_day_close : float;
      (* Previous trading day's closing price *)
  prev_day_high : float;
      (* Previous trading day's high *)
  prev_day_low : float;
      (* Previous trading day's low *)
  opening_range_minutes : int;
      (* Minutes for opening range calculation (e.g., 30) *)
  opening_range_high : float;
      (* High of first N minutes - NaN until opening range complete *)
  opening_range_low : float;
      (* Low of first N minutes - NaN until opening range complete *)
  day_high : float;
      (* Running high for current trading day - updated incrementally *)
  day_low : float;
      (* Running low for current trading day - updated incrementally *)
  minutes_since_open : float;
      (* Cached minutes since market open - computed once per tick *)
}

(* GADT AST with phantom types for compile-time type safety *)
type _ t =
  | Const : 'a * 'a Type.t -> 'a t
  (* Type-safe data access *)
  | Data : Data.Type.t t -> float t
  (* | Indicator : Tacaml.Indicator.t t -> float t *)
  | App1 : ('a -> 'b) t * 'a t -> 'b t
  | App2 : ('a -> 'b -> 'c) t * 'a t * 'b t -> 'c t
  | App3 : ('a -> 'b -> 'c -> 'd) t * 'a t * 'b t * 'c t -> 'd t
  | Fun : string * ('a -> 'b) -> ('a -> 'b) t
  | ContextModifier : 'a t * (context -> 'a -> context) * 'b t -> 'b t
  | Var : Uuidm.t * 'a Type.t * bounds -> 'a t (* Added bounds field *)
  | Symbol : unit -> Instrument.t t
  (* Position risk management nodes *)
  | EntryPrice : float t
  | EntryTick : int t
  | TicksHeld : int t
  | HasPosition : bool t
  | TickTime : float t (* Current tick timestamp as Unix time *)
  (* Intraday session nodes - for ORB, gap, and intraday strategies *)
  | DayOpen : float t (* Today's opening price *)
  | DayHigh : float t (* Today's high so far *)
  | DayLow : float t (* Today's low so far *)
  | DayChangePct : float t (* (current - day_open) / day_open * 100 *)
  | PrevDayClose : float t (* Yesterday's closing price *)
  | PrevDayHigh : float t (* Yesterday's high *)
  | PrevDayLow : float t (* Yesterday's low *)
  | GapPct : float t (* (day_open - prev_close) / prev_close * 100 *)
  | OpeningRangeHigh : float t (* High of first N minutes *)
  | OpeningRangeLow : float t (* Low of first N minutes *)
  | MinutesSinceOpen : float t (* Minutes since market open (cached) *)

let data x = Data (Const (x, Data))
let close = data Data.Type.Close
let volume = data @@ Data.Type.Volume
let index = data @@ Data.Type.Index
let last = data @@ Data.Type.Last
let open_ = data @@ Data.Type.Open
let high = data @@ Data.Type.High
let low = data @@ Data.Type.Low

(* Helper: Find entry (Buy) order from the order list *)
let find_entry_order orders =
  List.find_opt
    (fun (order : Order.t) ->
      match order.side with
      | Buy -> true
      | Sell -> false)
    orders

(* Type-safe evaluation *)
let rec eval : type a. context -> a t -> (a, Error.t) result =
 fun (ctx : context) t ->
  let { instrument; data; bars; index; orders; _ } = ctx in
  let ( let* ) = Result.( let* ) in
  (* Bounds checking *)
  if index < 0 || index >= Data.length data then
    Error.fatal
      (Printf.sprintf "GADT.eval: index %d out of bounds (data length: %d)"
         index (Data.length data))
  else
    match t with
    | Const (x, _) -> Result.return x
    | Fun (_, f) -> Result.return f
    | ContextModifier (x, f, expr) ->
      let* arg = eval ctx x in
      let ctx' = f ctx arg in
      let* res = eval ctx' expr in
      Result.return res
    | App1 (f, x) ->
      (* Optimize specific time functions to use cached values *)
      (match f with
      | Fun ("is_open", _) ->
        let* _timestamp = eval ctx x in
        (* Evaluate but ignore - we use cached value *)
        (* Safe: is_open always returns bool, and this pattern only matches is_open *)
        Result.return (Obj.magic ctx.is_market_open : a)
      | _ ->
        let* f = eval ctx f in
        let* arg = eval ctx x in
        let res = f arg in
        Result.return res)
    | App2 (f, x, y) ->
      (* Optimize specific time functions to use cached values *)
      (match f with
      | Fun ("is_close", _) ->
        let* _timestamp = eval ctx x in
        (* Evaluate but ignore - we use cached value *)
        let* threshold = eval ctx y in
        (* Safe: is_close takes float threshold and returns bool *)
        let threshold_float = (Obj.magic threshold : float) in
        let res =
          ctx.minutes_until_close >=. 0.0
          && ctx.minutes_until_close <=. threshold_float
        in
        (* Safe: is_close always returns bool, and this pattern only matches is_close *)
        Result.return (Obj.magic res : a)
      | _ ->
        let* f = eval ctx f in
        let* x = eval ctx x in
        let* y = eval ctx y in
        let res = f x y in
        Result.return res)
    | App3 (f, x, y, z) ->
      let* f = eval ctx f in
      let* x = eval ctx x in
      let* y = eval ctx y in
      let* z = eval ctx z in
      let res = f x y z in
      Result.return res
    | Symbol () -> Result.return instrument
    | Var _ -> invalid_arg "Cannot evaluate gadts with variables in them"
    | Data ty ->
      let* ty = eval ctx ty in
      Error.guard (Error.fatal "Error in GADT evaluation at Data node")
      @@ fun () -> Data.get data ty index
    | EntryPrice ->
      (match find_entry_order orders with
      | Some order -> Result.return order.price
      | None -> Error.fatal "No entry order found for EntryPrice")
    | EntryTick ->
      (match find_entry_order orders with
      | Some order -> Result.return order.tick
      | None -> Error.fatal "No entry order found for EntryTick")
    | TicksHeld ->
      (match find_entry_order orders with
      | Some order -> Result.return (index - order.tick)
      | None -> Result.return 0)
      (* No position = 0 ticks held *)
    | HasPosition -> Result.return (not (List.is_empty orders))
    | TickTime ->
      Result.return ctx.tick_time
    | DayOpen ->
      if ctx.day_start_index >= 0 && ctx.day_start_index < Data.length data
      then
        Error.guard (Error.fatal "Error getting DayOpen") @@ fun () ->
        Data.get data Data.Type.Last ctx.day_start_index
      else Error.fatal "Invalid day_start_index for DayOpen"
    | DayHigh -> Result.return ctx.day_high
    | DayLow -> Result.return ctx.day_low
    | DayChangePct ->
      if ctx.day_start_index >= 0 && ctx.day_start_index < Data.length data
      then
        Error.guard (Error.fatal "Error computing DayChangePct") @@ fun () ->
        let day_open = Data.get data Data.Type.Last ctx.day_start_index in
        let current = Data.get data Data.Type.Last index in
        (current -. day_open) /. day_open *. 100.0
      else Error.fatal "Invalid day_start_index for DayChangePct"
    | PrevDayClose -> Result.return ctx.prev_day_close
    | PrevDayHigh -> Result.return ctx.prev_day_high
    | PrevDayLow -> Result.return ctx.prev_day_low
    | GapPct ->
      if ctx.day_start_index >= 0 && Float.(ctx.prev_day_close > 0.0) then
        Error.guard (Error.fatal "Error computing GapPct") @@ fun () ->
        let day_open = Data.get data Data.Type.Last ctx.day_start_index in
        (day_open -. ctx.prev_day_close) /. ctx.prev_day_close *. 100.0
      else Result.return 0.0
    | OpeningRangeHigh -> Result.return ctx.opening_range_high
    | OpeningRangeLow -> Result.return ctx.opening_range_low
    | MinutesSinceOpen ->
      Result.return ctx.minutes_since_open
(* | Indicator ty -> *)
(*   let* ty = eval context ty in *)
(*   Error.guard (Error.fatal "Error in GADT evaluation at Data node") *)
(*   @@ fun () -> Data.get data (Tacaml ty) index *)

module Subst = struct
  module Bindings = struct
    include Map.Make (Uuidm)

    let get id map =
      get id map |> function
      | Some x -> Ok x
      | None ->
        Eio.traceln "BINDINGS GET FAILED: Variable UUID %a not found in map"
          Uuidm.pp id;
        Eio.traceln "Map contains %d bindings" (cardinal map);
        Error.fatal (Format.asprintf "No binding for variable %a" Uuidm.pp id)
  end

  let rec collect_variables : type a. a t -> 'b list = function
    | Var (id, ty, bounds) -> [ (id, (Type.A ty, bounds)) ]
    | Const _ -> []
    | Symbol _ -> []
    | Data x -> collect_variables x
    | ContextModifier (expr, _, x) ->
      collect_variables expr @ collect_variables x
    (* | Indicator x -> collect_variables x *)
    | App1 (f, x) -> collect_variables f @ collect_variables x
    | App2 (f, x, y) ->
      collect_variables f @ collect_variables x @ collect_variables y
    | App3 (f, x, y, z) ->
      collect_variables f @ collect_variables x @ collect_variables y
      @ collect_variables z
    | Fun _ -> []
    | EntryPrice -> []
    | EntryTick -> []
    | TicksHeld -> []
    | HasPosition -> []
    | TickTime -> []
    | DayOpen -> []
    | DayHigh -> []
    | DayLow -> []
    | DayChangePct -> []
    | PrevDayClose -> []
    | PrevDayHigh -> []
    | PrevDayLow -> []
    | GapPct -> []
    | OpeningRangeHigh -> []
    | OpeningRangeLow -> []
    | MinutesSinceOpen -> []

  let collect_variables : 'a t -> (Uuidm.t * (Type.shadow * bounds)) list =
   fun x ->
    let vars = collect_variables x in
    List.uniq ~eq:(fun (id0, _) (id1, _) -> Uuidm.equal id0 id1) vars

  type env = { float_map : float Bindings.t; int_map : int Bindings.t }

  (* let of_l l = *)
  (*   let init = { *)
  (*     float_map = Bindings.empty; *)
  (*     int_map = Bindings.empty; *)
  (*   } in *)

  let env_of_arr params (arr : (Uuidm.t * (Type.shadow * bounds)) array) =
    Array.foldi
      (fun env i (id, (Type.A ty, _bounds)) ->
        match ty with
        | Type.Bool -> invalid_arg "Gadt.env_of_arr Type.Bool"
        | Type.Data -> invalid_arg "Type.Data NYI (gadt.ml)"
        | Type.Tacaml -> invalid_arg "Type.Tacaml NYI (gadt.ml)"
        | Type.Float ->
          { env with float_map = Bindings.add id params.(i) env.float_map }
        | Type.Int ->
          let int_val = Int.of_float params.(i) in
          { env with int_map = Bindings.add id int_val env.int_map })
      { float_map = Bindings.empty; int_map = Bindings.empty }
      arr

  let rec instantiate : type a. env -> a t -> (a t, Error.t) result =
    let ( let* ) = Result.( let* ) in
    fun env -> function
      | Var (id, ty, _bounds) ->
        (match ty with
        | Bool -> invalid_arg "NYI Instantiate bool"
        | Data -> invalid_arg "NYI Instantiate Data variable"
        | Tacaml -> invalid_arg "NYI Instantiate Tacaml variable"
        | Float ->
          (match Bindings.get id env.float_map with
          | Ok res -> Result.return @@ Const (res, Float)
          | Error e -> Error e)
        | Int ->
          (match Bindings.get id env.int_map with
          | Ok res -> Result.return @@ Const (res, Int)
          | Error e -> Error e))
      | Const _ as x -> Result.return x
      | Fun _ as x -> Result.return x
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
      | EntryPrice -> Result.return EntryPrice
      | EntryTick -> Result.return EntryTick
      | TicksHeld -> Result.return TicksHeld
      | HasPosition -> Result.return HasPosition
      | TickTime -> Result.return TickTime
      | DayOpen -> Result.return DayOpen
      | DayHigh -> Result.return DayHigh
      | DayLow -> Result.return DayLow
      | DayChangePct -> Result.return DayChangePct
      | PrevDayClose -> Result.return PrevDayClose
      | PrevDayHigh -> Result.return PrevDayHigh
      | PrevDayLow -> Result.return PrevDayLow
      | GapPct -> Result.return GapPct
      | OpeningRangeHigh -> Result.return OpeningRangeHigh
      | OpeningRangeLow -> Result.return OpeningRangeLow
      | MinutesSinceOpen -> Result.return MinutesSinceOpen
  (* | Indicator ty -> *)
  (*   let* ty' = instantiate env ty in *)
  (*   Result.return @@ Indicator ty' *)

  (* let apply_subst (x : 'a expr) = *)
  (*   match collect_variables x with *)
  (*   | [] -> *)
end

(* Pretty printer for GADT expressions *)
let rec pp : type a. Format.formatter -> a t -> unit =
 fun fmt expr ->
  match expr with
  | Const (x, s) ->
    (match s with
    | Type.Bool -> Format.fprintf fmt "%b" x
    | Type.Data -> Format.fprintf fmt "%a" Data.Type.pp x
    | Type.Tacaml -> Format.fprintf fmt "%a" Tacaml.Indicator.pp x
    | Type.Float -> Format.fprintf fmt "%f" x
    | Type.Int -> Format.fprintf fmt "%d" x (* Format.fprintf fmt "Const(?)" *))
  | Fun (f, _) -> Format.fprintf fmt "%s" f
  | Symbol () -> Format.fprintf fmt "Symbol()"
  | ContextModifier (f, _, z) ->
    Format.fprintf fmt "ContextModifier(%a,%a)" pp f pp z
  | Var (id, ty, bounds) ->
    let ty_str =
      match ty with
      | Type.Bool -> "Bool Var"
      | Type.Data -> "Data.Type.t Var"
      | Type.Tacaml -> "Tacaml.Indicator.t Var"
      | Type.Float -> "Float Var"
      | Type.Int -> "Int Var"
    in
    Format.fprintf fmt "Var(%s:%s[%.1f,%.1f])" (Uuidm.to_string id) ty_str
      bounds.lower bounds.upper
  | Data e -> Format.fprintf fmt "(@[%a@])" pp e
  (* | Indicator e -> Format.fprintf fmt "Indicator(@[%a@])" pp e *)
  | App1 (Fun ("tacaml", _), x) -> Format.fprintf fmt "@[%a@]" pp x
  | App1 (Fun ("time_of_day_et", _), x) ->
    Format.fprintf fmt "time_of_day_et(@[%a@])" pp x
  | App1 (Fun ("minutes_since_open", _), x) ->
    Format.fprintf fmt "minutes_since_open(@[%a@])" pp x
  | App1 (Fun ("minutes_until_close", _), x) ->
    Format.fprintf fmt "minutes_until_close(@[%a@])" pp x
  | App1 (Fun ("is_open", _), x) -> Format.fprintf fmt "is_open(@[%a@])" pp x
  | App2 (Fun ("is_close", _), x, y) ->
    Format.fprintf fmt "is_close(@[%a,@ %a@])" pp x pp y
  | App2 (Fun ("is_near_open", _), x, y) ->
    Format.fprintf fmt "is_near_open(@[%a,@ %a@])" pp x pp y
  | App2 (Fun ("&&.", _), x, y) ->
    Format.fprintf fmt "(@[%a@ &&.@ %a@])" pp x pp y
  | App2 (Fun ("||", _), x, y) ->
    Format.fprintf fmt "(@[%a@ ||.@ %a@])" pp x pp y
  | App2 (Fun ("<.", _), x, y) ->
    Format.fprintf fmt "(@[%a@ <.@ %a@])" pp x pp y
  | App2 (Fun (">.", _), x, y) ->
    Format.fprintf fmt "(@[%a@ >.@ %a@])" pp x pp y
  | EntryPrice -> Format.fprintf fmt "EntryPrice"
  | EntryTick -> Format.fprintf fmt "EntryTick"
  | TicksHeld -> Format.fprintf fmt "TicksHeld"
  | HasPosition -> Format.fprintf fmt "HasPosition"
  | TickTime -> Format.fprintf fmt "TickTime"
  | DayOpen -> Format.fprintf fmt "DayOpen"
  | DayHigh -> Format.fprintf fmt "DayHigh"
  | DayLow -> Format.fprintf fmt "DayLow"
  | DayChangePct -> Format.fprintf fmt "DayChangePct"
  | PrevDayClose -> Format.fprintf fmt "PrevDayClose"
  | PrevDayHigh -> Format.fprintf fmt "PrevDayHigh"
  | PrevDayLow -> Format.fprintf fmt "PrevDayLow"
  | GapPct -> Format.fprintf fmt "GapPct"
  | OpeningRangeHigh -> Format.fprintf fmt "OpeningRangeHigh"
  | OpeningRangeLow -> Format.fprintf fmt "OpeningRangeLow"
  | MinutesSinceOpen -> Format.fprintf fmt "MinutesSinceOpen"
  | App1 (f, x) -> Format.fprintf fmt "@[%a@ %a@]" pp f pp x
  | App2 (f, x, y) -> Format.fprintf fmt "@[%a@ %a@ %a@]" pp f pp x pp y
  | App3 (f, x, y, z) ->
    Format.fprintf fmt "@[%a@ %a@ %a@ %a@]" pp f pp x pp y pp z

(* Helper to print expression to string *)
let to_string : type a. a t -> string = fun expr -> Format.asprintf "%a" pp expr

(* Helper to print variables found in expression *)
let debug_variables : type a. a t -> unit =
 fun expr ->
  let vars = Subst.collect_variables expr in
  Eio.traceln "=== VARIABLES IN EXPRESSION ===";
  Eio.traceln "Expression: %s" (to_string expr);
  Eio.traceln "Variables found: %d" (List.length vars);
  List.iteri
    (fun i (id, (Type.A ty, bounds)) ->
      let ty_str =
        match ty with
        | Type.Float -> "Float"
        | Type.Int -> "Int"
        | Type.Bool -> "Data"
        | Type.Data -> "Data"
        | Type.Tacaml -> "Tacaml"
      in
      Eio.traceln "  [%d] %s: %s [%.1f, %.1f]" i (Uuidm.to_string id) ty_str
        bounds.lower bounds.upper)
    vars;
  Eio.traceln "================================"

(* Convenience operators *)
let ( >. ) e1 e2 = App2 (Fun (">.", ( >. )), e1, e2)
let ( <. ) e1 e2 = App2 (Fun ("<.", ( <. )), e1, e2)
let ( >=. ) e1 e2 = App2 (Fun (">=.", ( >=. )), e1, e2)
let ( <=. ) e1 e2 = App2 (Fun ("<=.", ( <=. )), e1, e2)
let ( =. ) e1 e2 = App2 (Fun ("=.", Float.equal), e1, e2)
let ( &&. ) e1 e2 = App2 (Fun ("&&.", ( && )), e1, e2)
let ( ||. ) e1 e2 = App2 (Fun ("||.", ( || )), e1, e2)
let ( +. ) e1 e2 = App2 (Fun ("+.", ( +. )), e1, e2)
let ( -. ) e1 e2 = App2 (Fun ("-.", ( -. )), e1, e2)
let ( *. ) e1 e2 = App2 (Fun ("*.", ( *. )), e1, e2)
let ( /. ) e1 e2 = App2 (Fun ("/.", ( /. )), e1, e2)

let conjunction (l : bool t list) =
  match l with
  | [] -> Const (true, Type.Bool)
  | x :: xs -> List.fold_left (fun acc gadt -> acc &&. gadt) x xs

let disjunction (l : bool t list) =
  match l with
  | [] -> Const (false, Type.Bool)
  | x :: xs -> List.fold_left (fun acc gadt -> acc &&. gadt) x xs

(* Additional operators *)
let not_ e = App1 (Fun ("not", not), e)

(* Domain-specific functions using ContextModifier *)

(* Lag function: access data N periods ago *)
let lag expr periods =
  ContextModifier
    ( Const (periods, Int),
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

let moneyness underlying option =
  App2 (Fun ("moneyness", moneyness_fn), underlying, option)

let days_to_expiry option = App1 (Fun ("dte", days_to_expiry_fn), option)

(* Market timing functions (timezone-aware, DST-aware) *)

(* Get time of day in Eastern Time as minutes since midnight (0-1440) *)
(* Useful for time-of-day comparisons: time_of_day_et(tick_time()) >. 960.0 means "after 4 PM ET" *)
let time_of_day_et timestamp =
  App1 (Fun ("time_of_day_et", Time.time_of_day_et), timestamp)

(* Get minutes since market open (9:30 AM ET) *)
(* Negative values indicate before market open *)
let minutes_since_open timestamp =
  App1 (Fun ("minutes_since_open", Time.minutes_since_open), timestamp)

(* Get minutes until market close (4:00 PM ET) *)
(* Negative values indicate after market close *)
let minutes_until_close timestamp =
  App1 (Fun ("minutes_until_close", Time.minutes_until_close), timestamp)

(* Check if currently within market hours (9:30 AM - 4:00 PM ET) *)
let is_open timestamp = App1 (Fun ("is_open", Time.is_open), timestamp)

(* Check if within threshold minutes of market close *)
(* Example: is_close(tick_time(), 30.0) returns true if less than 30 mins until close *)
let is_close timestamp threshold =
  App2 (Fun ("is_close", Time.is_close), timestamp, threshold)

(* Check if within threshold minutes of market open *)
(* Example: is_near_open(tick_time(), 15.0) returns true if less than 15 mins since open *)
let is_near_open timestamp threshold =
  App2 (Fun ("is_near_open", Time.is_near_open), timestamp, threshold)

(* Intraday session accessors - for ORB, gap, and intraday strategies *)
let day_open = DayOpen
let day_high = DayHigh
let day_low = DayLow
let day_change_pct = DayChangePct
let prev_day_close = PrevDayClose
let prev_day_high = PrevDayHigh
let prev_day_low = PrevDayLow
let gap_pct = GapPct
let opening_range_high = OpeningRangeHigh
let opening_range_low = OpeningRangeLow
let minutes_since_open_cached = MinutesSinceOpen

(* Check if opening range is complete (past first N minutes) *)
let opening_range_complete threshold =
  MinutesSinceOpen >. Const (threshold, Float)

(* Check if price broke above opening range high *)
let broke_orb_high = last >. OpeningRangeHigh

(* Check if price broke below opening range low *)
let broke_orb_low = last <. OpeningRangeLow

(* Check if stock gapped up by at least threshold percent *)
let gapped_up threshold = GapPct >. Const (threshold, Float)

(* Check if stock gapped down by at least threshold percent *)
let gapped_down threshold = GapPct <. Const (Float.neg threshold, Float)

(* Check if price is extended from day open by threshold percent *)
let extended_up threshold = DayChangePct >. Const (threshold, Float)
let extended_down threshold = DayChangePct <. Const (Float.neg threshold, Float)

(* Buy window: only allow buys during a specific 1-minute window after open *)
let buy_window_at minute =
  let upper = Float.add minute 1.0 in
  (MinutesSinceOpen >=. Const (minute, Float)) &&.
  (MinutesSinceOpen <. Const (upper, Float))
