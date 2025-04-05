module S = Signal

(* module SI = S.Indicator *)
module I = Indicators
module P = I.Point
(* module F = S.Flag *)

(* [@@@warning "-26"] *)

module Param = struct
  let trailing_loss = 0.96
  let stop_loss_multiplier = 0.99
  let min_holding_period = 40

  (* let profit_multiplier = 1.03 *)
  let max_holding_period = 546
end

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass (state : 'a State.t) instrument =
    (* let price = State.price state symbol in *)
    let ( let$ ) = Signal.( let$ ) in
    let ( let&& ) = Signal.( let&& ) in
    let ( let* ) = Result.( let* ) in
    let* i = Indicators.get_top state.indicators instrument in
    let$ prev = i.previous in
    let$ prev_prev = i.previous in
    let&& () =
      prev.fast_stochastic_oscillator_k <=. prev.fast_stochastic_oscillator_d
      && i.fast_stochastic_oscillator_k -. i.fast_stochastic_oscillator_d
         >=. 20.0
    in
    let&& () = i.volume >= prev.volume && i.volume >= prev_prev.volume in
    Result.return @@ Option.return
    @@ {
         Signal.instrument;
         side = Trading_types.Side.Buy;
         reason = [ "Passed condition in Throwing_crossover.pass" ];
       }

  let score (state : 'a State.t) symbol =
    let ( let* ) = Result.( let* ) in
    let* i = Indicators.get_top state.indicators symbol in
    Result.return @@ (-1.0 *. i.relative_strength_index)

  let num_positions = 5
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let ( let* ) = Result.( let* ) in
    let ( let$ ) = Signal.( let$ ) in
    let ( let&& ) = Signal.( let&& ) in
    let* price = State.price state buying_order.symbol in
    let* i = Indicators.get_top state.indicators buying_order.symbol in
    let* price_history = Bars.get_res state.bars buying_order.symbol in
    let$ prev = i.previous in
    let ticks_held = state.tick - buying_order.tick in
    let high_since_purchase =
      Util.last_n ticks_held price_history |> Math.max_close |> Item.last
    in
    let holding_period = ticks_held >= Param.min_holding_period in
    let price_decreasing =
      prev.sma_5 >=. prev.sma_34 && i.sma_5 <=. prev.sma_34
    in
    let profited = price >=. buying_order.price in
    let high_fso = i.fast_stochastic_oscillator_d >=. 80.0 in
    let stoploss =
      price <=. Param.stop_loss_multiplier *. high_since_purchase
    in
    let&& () = holding_period in
    let&& () =
      (high_fso && if profited then price_decreasing else true)
      || stoploss
      || ((not profited) && i.ema_12 <=. prev.ema_12)
    in
    Result.return @@ Option.return
    @@ {
         Signal.instrument = buying_order.symbol;
         side = Trading_types.Side.Sell;
         reason = [ "Passed sell condition in Throwing_crossover.sell" ];
       }
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
