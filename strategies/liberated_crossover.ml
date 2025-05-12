module S = Signal

(* module SI = S.Indicator *)
module I = Indicators
module P = I.Point
(* module F = S.Flag *)

(* [@@@warning "-26"] *)

let ( let* ) = Result.( let* )
let ( let+ ) = Result.( let+ )

module Param = struct
  (* let trailing_loss = 0.96 *)
  let stop_loss_multiplier = 0.98
  let holding_period = 40

  (* let profit_multiplier = 1.03 *)
  (* let max_holding_period = 546 *)
end

let bullish_crossover ~(i : P.t) ~(prev : P.t) =
  prev.fso.k <=. prev.fso.d && i.fso.k >=. i.fso.d
(* && i.fso.k -. i.fso.d >. 5.0 *)

let bearish_crossover ~(i : P.t) ~(prev : P.t) =
  prev.fso.k >=. prev.fso.d && i.fso.k <=. i.fso.d
(* && i.fso.d -. i.fso.k >. 5.0 *)

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass (state : 'a State.t) instrument =
    let signal = Signal.make instrument Buy true in
    let ( let&& ) = Signal.let_and signal in
    let ( let$ ) = Signal.let_get_opt signal in
    let+ i = Indicators.get_top state.indicators instrument in
    let$ prev = i.previous in
    (* let&& () = *)
    (*   i.fso.d >. i.fast_stochastic_oscillator_k, "fso mas" *)
    (*   (\* i.fso.d -. i.fso.d68 >. 10.0, "fso mas" *\) *)
    (* in *)
    (* let&& () = *)
    (*   i.relative_strength_index >=. 60.0, "small rsi" *)
    (* in *)
    (* let$ prev_prev = prev.previous in *)
    let&& () = (i.price <>. prev.price, "movement confirm") in
    let&& () = (bullish_crossover ~i ~prev, "prev k <= d") in
    let&& () = (i.price >=. i.sma_233, "high sma") in
    (* let&& () = (i.volume >= prev.volume, "first volume confirm") in *)
    (* let&& () = (i.price >=. prev.price, "price increase confirm") in *)
    (* let&& () = (i.relative_strength_index <=. 30.0, "low rsi confirm") in *)
    (* let&& () = (i.relative_strength_index <=. 40.0, "first volume confirm") in *)
    (* let&& () = (i.cci.cci <=. 50.0, "reasonable cci") in *)
    (* let&& () = (i.volume >= prev_prev.volume, "second volume confirm") in *)
    signal

  let score (state : 'a State.t) symbol =
    let ( let+ ) = Result.( let+ ) in
    let+ i = Indicators.get_top state.indicators symbol in
    -1.0 *. i.relative_strength_index

  let num_positions = 5
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

let refresh_table : (Order.t, int) Hashtbl.t = Hashtbl.create 10

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let signal = Signal.make buying_order.symbol Sell false in
    let ( let$ ) = Signal.let_get_opt signal in
    let ( let|| ) = Signal.let_or signal in
    let* price = State.price state buying_order.symbol in
    let+ i = Indicators.get_top state.indicators buying_order.symbol in
    (* let+ price_history = Bars.get_res state.bars buying_order.symbol in *)
    let$ prev = i.previous in
    let ticks_held = state.tick - buying_order.tick in
    (* let holding_period = ticks_held >= Param.holding_period in *)
    (* let price_decreasing = i.sma_5 <=. i.sma_34 in *)
    let profited = price >. buying_order.price in
    (* let high_fso = i.fast_stochastic_oscillator_d >=. 80.0 in *)
    let stoploss = price <=. Param.stop_loss_multiplier *. buying_order.price in
    (* let|| () = (price_decreasing, "price dip") in *)
    (* let|| () = *)
    let|| () = (bearish_crossover ~i ~prev, "bearish") in
    (* let|| ()  = ticks_held > 12, "timer" in *)
    (* let|| () = i.fast_stochastic_oscillator_k >=. 95.0, "high fso" in *)
    (* let|| () = not profited && ticks_held > 1, "early exit" in *)
    (* let|| () = (stoploss, "stoploss") in *)
    (* let|| () = *)
    (*   (bearish_crossover ~i ~prev && ticks_held > 10, "bearish crossover") *)
    (* in *)
    (* let|| () = (profited, "profited") in *)
    (* ( i.fast_stochastic_oscillator_d <=. prev.fast_stochastic_oscillator_d, *)
    (*   "decreasing k" ) *)
    (* i.fast_stochastic_oscillator_k >. i.fast_stochastic_oscillator_d, "fso mas" *)
    (* i.fast_stochastic_oscillator_d68 -. i.fast_stochastic_oscillator_d >. 10.0, "fso mas" *)
    (* let|| () = ((not profited) && ticks_held > 10, "early exit") in *)
    (* let|| () = *)
    (*   price >=. i.upper_bollinger_100_3, "upper boll" *)
    (* in *)
    (*   ((high_fso && if profited then price_decreasing else true), "high_fso") *)
    (* in *)
    (* let|| () = *)
    (*   ((not profited) && i.ema_12 <=. prev.ema_12, "unprofitable exit") *)
    (* in *)
    signal
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
