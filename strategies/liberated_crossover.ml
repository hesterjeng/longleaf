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
  let stop_loss_multiplier = 0.96
  let holding_period = 40

  (* let profit_multiplier = 1.03 *)
  (* let max_holding_period = 546 *)
end

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass (state : 'a State.t) instrument =
    let signal = Signal.make instrument Buy true in
    let ( let&& ) = Signal.let_and signal in
    let ( let$ ) = Signal.let_get_opt signal in
    let+ i = Indicators.get_top state.indicators instrument in
    let$ prev = i.previous in
    (* let$ prev_prev = prev.previous in *)
    let&& () =
      ( prev.fast_stochastic_oscillator_k <=. prev.fast_stochastic_oscillator_d,
        "prev k <= d" )
    in
    let&& () =
      ( i.fast_stochastic_oscillator_k -. i.fast_stochastic_oscillator_d >=. 20.0,
        " k >= d by 20" )
    in
    let&& () = (i.volume >= prev.volume, "first volume confirm") in
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
    (* let ticks_held = state.tick - buying_order.tick in *)
    (* let holding_period = ticks_held >= Param.holding_period in *)
    let price_decreasing = i.sma_5 <=. i.sma_34 in
    (* let profited = price >=. buying_order.price in *)
    (* let high_fso = i.fast_stochastic_oscillator_d >=. 80.0 in *)
    (* let stoploss = price <=. Param.stop_loss_multiplier *. buying_order.price in *)
    let|| () = (price_decreasing, "price dip") in
    (* let|| () = *)
    (*   ((high_fso && if profited then price_decreasing else true), "high_fso") *)
    (* in *)
    (* let|| () = (stoploss, "stoploss") in *)
    (* let|| () = *)
    (*   ((not profited) && i.ema_12 <=. prev.ema_12, "unprofitable exit") *)
    (* in *)
    signal
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
