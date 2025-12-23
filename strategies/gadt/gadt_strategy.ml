module Data = Longleaf_bars.Data
module State = Longleaf_state

exception InvalidGADT of Error.t
exception OptimizationException

(* Strategy structure *)
type t = {
  name : string;
  buy_trigger : bool Gadt.t;
  sell_trigger : bool Gadt.t;
  score : float Gadt.t; (* Ranking: higher score = better opportunity *)
  max_positions : int;
  position_size : float;
}

let buy_conj y (x : t) = { x with buy_trigger = Gadt.(x.buy_trigger &&. y) }
let sell_conj y (x : t) = { x with sell_trigger = Gadt.(x.sell_trigger &&. y) }
let buy_disj y (x : t) = { x with buy_trigger = Gadt.(x.buy_trigger ||. y) }
let sell_disj y (x : t) = { x with sell_trigger = Gadt.(x.sell_trigger ||. y) }

(* Collect all variables from a strategy (buy, sell, score) with deduplication *)
let collect_all_variables (strategy : t) :
    (Uuidm.t * (Gadt.Type.shadow * Gadt.bounds)) list =
  Gadt.Subst.collect_variables strategy.buy_trigger
  @ Gadt.Subst.collect_variables strategy.sell_trigger
  @ Gadt.Subst.collect_variables strategy.score
  |> List.uniq ~eq:(fun (id0, _) (id1, _) -> Uuidm.equal id0 id1)

let random () =
  let buy_trigger, sell_trigger = (Groups.rand (), Groups.rand ()) in
  let name =
    "Sealab_" ^ (Uuidm.v4_gen Longleaf_util.random_state () |> Uuidm.to_string)
  in
  let max_positions = 4 in
  let position_size = 0.25 in
  let score = Gadt.Const (1.0, Float) in
  (* Default score: all signals equal *)
  { name; buy_trigger; sell_trigger; score; max_positions; position_size }

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
    | EntryPrice -> Error.fatal "Cannot evaluate EntryPrice in eval_simple"
    | EntryTick -> Error.fatal "Cannot evaluate EntryTick in eval_simple"
    | TicksHeld -> Error.fatal "Cannot evaluate TicksHeld in eval_simple"
    | HasPosition -> Error.fatal "Cannot evaluate HasPosition in eval_simple"
    | TickTime -> Error.fatal "Cannot evaluate TickTime in eval_simple"
    | DayOpen -> Error.fatal "Cannot evaluate DayOpen in eval_simple"
    | DayHigh -> Error.fatal "Cannot evaluate DayHigh in eval_simple"
    | DayLow -> Error.fatal "Cannot evaluate DayLow in eval_simple"
    | DayChangePct -> Error.fatal "Cannot evaluate DayChangePct in eval_simple"
    | PrevDayClose -> Error.fatal "Cannot evaluate PrevDayClose in eval_simple"
    | PrevDayHigh -> Error.fatal "Cannot evaluate PrevDayHigh in eval_simple"
    | PrevDayLow -> Error.fatal "Cannot evaluate PrevDayLow in eval_simple"
    | GapPct -> Error.fatal "Cannot evaluate GapPct in eval_simple"
    | OpeningRangeHigh -> Error.fatal "Cannot evaluate OpeningRangeHigh in eval_simple"
    | OpeningRangeLow -> Error.fatal "Cannot evaluate OpeningRangeLow in eval_simple"
    | MinutesSinceOpen -> Error.fatal "Cannot evaluate MinutesSinceOpen in eval_simple"

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
  module Time = Longleaf_core.Time
  module Bars = Longleaf_bars

  (* Intraday session tracking - cached per symbol, updated at day boundaries *)
  type session_state = {
    day_start_index : int;
    prev_day_close : float;
    prev_day_high : float;
    prev_day_low : float;
    opening_range_high : float;
    opening_range_low : float;
    opening_range_complete : bool;
    last_tick_date : int;
  }

  let default_session = {
    day_start_index = -1;
    prev_day_close = Float.nan;
    prev_day_high = Float.nan;
    prev_day_low = Float.nan;
    opening_range_high = Float.nan;
    opening_range_low = Float.nan;
    opening_range_complete = false;
    last_tick_date = -1;
  }

  let opening_range_minutes = 30

  let day_of_year tick_time =
    let tm = Unix.gmtime tick_time in
    tm.Unix.tm_yday

  let update_session session data current_index tick_time =
    let current_day = day_of_year tick_time in
    let mins_since_open = Time.minutes_since_open tick_time in

    if current_day <> session.last_tick_date && Float.(mins_since_open >= 0.0) then
      (* New trading day - compute previous day's stats *)
      let prev_day_close, prev_day_high, prev_day_low =
        if session.day_start_index >= 0 then
          let last_prev_idx = current_index - 1 in
          if last_prev_idx >= session.day_start_index then
            let close = Data.get data Data.Type.Last last_prev_idx in
            let rec compute i high low =
              if i > last_prev_idx then (high, low)
              else
                let h = Data.get data Data.Type.High i in
                let l = Data.get data Data.Type.Low i in
                compute (i + 1) (Float.max high h) (Float.min low l)
            in
            let high, low = compute session.day_start_index neg_infinity infinity in
            (close, high, low)
          else (session.prev_day_close, session.prev_day_high, session.prev_day_low)
        else (Float.nan, Float.nan, Float.nan)
      in
      {
        day_start_index = current_index;
        prev_day_close;
        prev_day_high;
        prev_day_low;
        opening_range_high = Float.nan;
        opening_range_low = Float.nan;
        opening_range_complete = false;
        last_tick_date = current_day;
      }
    else if not session.opening_range_complete && Float.(mins_since_open >= of_int opening_range_minutes) then
      (* Opening range complete - compute OR high/low *)
      if session.day_start_index >= 0 then
        let rec compute i high low =
          if i > current_index then (high, low)
          else
            let tick_time_i = tick_time -. (Float.of_int (current_index - i) *. 60.0) in
            let mins_i = Time.minutes_since_open tick_time_i in
            if Float.(mins_i < of_int opening_range_minutes) then
              let h = Data.get data Data.Type.High i in
              let l = Data.get data Data.Type.Low i in
              compute (i + 1) (Float.max high h) (Float.min low l)
            else (high, low)
        in
        let or_high, or_low = compute session.day_start_index neg_infinity infinity in
        { session with opening_range_high = or_high; opening_range_low = or_low; opening_range_complete = true }
      else session
    else session

  (* Helper function to evaluate strategy triggers *)
  let eval_strategy_signal ~tick_time ~is_market_open ~minutes_until_close
      session (strategy_expr : bool Gadt.t) (state : Longleaf_state.t) symbol =
    let ( let* ) = Result.( let* ) in
    let* data =
      State.data state symbol
    in
    let bars = State.bars state in
    let current_index = State.tick state in
    let positions = State.positions state in
    let orders = State.Positions.get positions symbol in
    let context : Gadt.context =
      {
        instrument = symbol;
        data;
        bars;
        index = current_index;
        orders;
        tick_time;
        is_market_open;
        minutes_until_close;
        day_start_index = session.day_start_index;
        prev_day_close = session.prev_day_close;
        prev_day_high = session.prev_day_high;
        prev_day_low = session.prev_day_low;
        opening_range_minutes;
        opening_range_high = session.opening_range_high;
        opening_range_low = session.opening_range_low;
      }
    in
    match Gadt.eval context strategy_expr with
    | Ok should_signal -> Result.return @@ Signal.make symbol should_signal
    | Error e -> Error e

  (* Helper function to evaluate score expressions *)
  let eval_score ~tick_time ~is_market_open ~minutes_until_close
      session (score_expr : float Gadt.t) (state : Longleaf_state.t) symbol =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state symbol in
    let bars = State.bars state in
    let current_index = State.tick state in
    let positions = State.positions state in
    let orders = State.Positions.get positions symbol in
    let context : Gadt.context =
      {
        instrument = symbol;
        data;
        bars;
        index = current_index;
        orders;
        tick_time;
        is_market_open;
        minutes_until_close;
        day_start_index = session.day_start_index;
        prev_day_close = session.prev_day_close;
        prev_day_high = session.prev_day_high;
        prev_day_low = session.prev_day_low;
        opening_range_minutes;
        opening_range_high = session.opening_range_high;
        opening_range_low = session.opening_range_low;
      }
    in
    Gadt.eval context score_expr

  (* Convert GADT strategy to Template-compatible modules *)
  let gadt_to_buy_trigger (buy_expr : bool Gadt.t) (score_expr : float Gadt.t)
      (max_positions : int) (position_size : float) =
    (* Cache for time info computed once per tick *)
    let time_cache : (int * float * bool * float) option ref = ref None in
    (* Session state per symbol *)
    let session_cache : (Longleaf_core.Instrument.t, session_state) Hashtbl.t =
      Hashtbl.create 100
    in

    let get_time_info state =
      let current_tick = State.tick state in
      match !time_cache with
      | Some (cached_tick, tick_time, is_open, mins_until_close)
        when cached_tick = current_tick ->
        Result.return (tick_time, is_open, mins_until_close)
      | _ ->
        let ( let* ) = Result.( let* ) in
        let bars = State.bars state in
        let* tick_time_ptime = Longleaf_bars.timestamp bars current_tick in
        let tick_time = Ptime.to_float_s tick_time_ptime in
        let is_market_open = Longleaf_core.Time.is_open tick_time in
        let minutes_until_close =
          Longleaf_core.Time.minutes_until_close tick_time
        in
        time_cache :=
          Some (current_tick, tick_time, is_market_open, minutes_until_close);
        Result.return (tick_time, is_market_open, minutes_until_close)
    in

    let get_session state symbol tick_time =
      let ( let* ) = Result.( let* ) in
      let* data = State.data state symbol in
      let current_index = State.tick state in
      let session =
        match Hashtbl.find_opt session_cache symbol with
        | Some s -> s
        | None -> default_session
      in
      let updated = update_session session data current_index tick_time in
      Hashtbl.replace session_cache symbol updated;
      Result.return updated
    in

    let module Buy_input : Longleaf_template.Buy_trigger.INPUT = struct
      let pass state symbol =
        let ( let* ) = Result.( let* ) in
        let* tick_time, is_market_open, minutes_until_close =
          get_time_info state
        in
        let* session = get_session state symbol tick_time in
        eval_strategy_signal ~tick_time ~is_market_open ~minutes_until_close
          session buy_expr state symbol

      let score state symbol =
        let ( let* ) = Result.( let* ) in
        let* tick_time, is_market_open, minutes_until_close =
          get_time_info state
        in
        let* session = get_session state symbol tick_time in
        eval_score ~tick_time ~is_market_open ~minutes_until_close session
          score_expr state symbol

      let num_positions = max_positions
      let position_size = position_size
    end in
    let module Buy_trigger = Template.Buy_trigger.Make (Buy_input) in
    (module Buy_trigger : Template.Buy_trigger.S)

  let gadt_to_sell_trigger (sell_expr : bool Gadt.t) =
    let time_cache : (int * float * bool * float) option ref = ref None in
    let session_cache : (Longleaf_core.Instrument.t, session_state) Hashtbl.t =
      Hashtbl.create 100
    in

    let get_time_info state =
      let current_tick = State.tick state in
      match !time_cache with
      | Some (cached_tick, tick_time, is_open, mins_until_close)
        when cached_tick = current_tick ->
        Result.return (tick_time, is_open, mins_until_close)
      | _ ->
        let ( let* ) = Result.( let* ) in
        let bars = State.bars state in
        let* tick_time_ptime = Longleaf_bars.timestamp bars current_tick in
        let tick_time = Ptime.to_float_s tick_time_ptime in
        let is_market_open = Longleaf_core.Time.is_open tick_time in
        let minutes_until_close =
          Longleaf_core.Time.minutes_until_close tick_time
        in
        time_cache :=
          Some (current_tick, tick_time, is_market_open, minutes_until_close);
        Result.return (tick_time, is_market_open, minutes_until_close)
    in

    let get_session state symbol tick_time =
      let ( let* ) = Result.( let* ) in
      let* data = State.data state symbol in
      let current_index = State.tick state in
      let session =
        match Hashtbl.find_opt session_cache symbol with
        | Some s -> s
        | None -> default_session
      in
      let updated = update_session session data current_index tick_time in
      Hashtbl.replace session_cache symbol updated;
      Result.return updated
    in

    let module Sell_impl : Template.Sell_trigger.S = struct
      let make state symbol =
        let ( let* ) = Result.( let* ) in
        let* tick_time, is_market_open, minutes_until_close =
          get_time_info state
        in
        let* session = get_session state symbol tick_time in
        eval_strategy_signal ~tick_time ~is_market_open ~minutes_until_close
          session sell_expr state symbol
    end in
    (module Sell_impl : Template.Sell_trigger.S)

  let top (strategy : t) =
    let buy_trigger =
      gadt_to_buy_trigger strategy.buy_trigger strategy.score
        strategy.max_positions strategy.position_size
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
  Eio.traceln "Entering Longleaf_template.run";
  Longleaf_template.run (Builder.top strategy) bars options mutices

open Gadt

(* Stop-loss: sell if current price is below entry price by stop_loss_pct (as decimal) *)
let stop_loss stop_loss_pct : bool Gadt.t =
  (* current_price < entry_price * (1 - stop_loss) *)
  let multiplier = Float.(1.0 - stop_loss_pct) in
  last <. EntryPrice *. Const (multiplier, Float)

(* Profit target: sell if current price is above entry price by profit_target_pct (as decimal) *)
let profit_target profit_target_pct : bool Gadt.t =
  (* current_price > entry_price * (1 + profit_target) *)
  let multiplier = Float.(1.0 + profit_target_pct) in
  last >. EntryPrice *. Const (multiplier, Float)

(* Max holding time: sell if held for more than max_ticks *)
let max_holding_time max_ticks : bool Gadt.t =
  (* Need to compare TicksHeld (int Gadt.t) with max_ticks - create custom comparison *)
  let max_ticks_expr = Const (max_ticks, Int) in
  (* Create a Fun that compares the two int values *)
  App2 (Fun (">", ( > )), TicksHeld, max_ticks_expr)

(* Intraday trading helpers - avoid overnight positions and closing volatility *)

(* Safe to enter: NOT within close_buffer minutes of market close
   Default 10 minutes avoids closing auction volatility *)
let safe_to_enter ?(close_buffer = 10.0) () : bool Gadt.t =
  App1 (Fun ("not", not), is_close TickTime (Const (close_buffer, Float)))

(* Force exit: within close_buffer minutes of market close *)
let force_exit_eod ?(close_buffer = 10.0) () : bool Gadt.t =
  is_close TickTime (Const (close_buffer, Float))
