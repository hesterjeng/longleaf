module S = Signal
module SI = S.Indicator
module I = Indicators
module P = I.Point
module F = S.Flag

[@@@warning "-26"]

module Param = struct
  let trailing_loss = 0.96
  let stop_loss_multiplier = 0.96
  let min_holding_period = 40

  (* let profit_multiplier = 1.03 *)
  let max_holding_period = 546
end

let ( let* ) = F.Infix.( let* )

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let confirmed_crossover (state : 'a State.t) symbol =
    let indicator = Indicators.get_top state.indicators symbol in
    let* prev = indicator.previous in
    let* prev_prev = prev.previous in
    let crossover =
      prev_prev.fast_stochastic_oscillator_k
      <=. prev_prev.fast_stochastic_oscillator_d
      && prev.fast_stochastic_oscillator_k >=. prev.fast_stochastic_oscillator_d
      && indicator.fast_stochastic_oscillator_k
         >=. indicator.fast_stochastic_oscillator_d
    in
    match crossover with
    | true -> F.Pass [ "Bullish Crossover" ]
    | false -> F.Fail [ "No Crossover" ]

  let pass (state : 'a State.t) symbol =
    let price = State.price state symbol in
    let i = Indicators.get_top state.indicators symbol in
    let* prev = i.previous in
    let* prev_prev = prev.previous in
    let conditions =
      [
        (* (match prev.relative_strength_index <=. 40.0 with *)
        (* | true -> F.Pass [ "Small RSI" ] *)
        (* | false -> Fail [ "RSI too large to buy" ]); *)
        (* (match prev.fast_stochastic_oscillator_d <=. 20.0 with *)
        (* | true -> F.Pass [ "FSO %D <= 20" ] *)
        (* | false -> F.Fail [ "FSO %D is too high" ]); *)
        (* (if i.awesome_slow >=. 0.0 then F.Pass [ "awesome" ] *)
        (*  else F.Fail [ "not awesome" ]); *)
        (let crossover =
           prev_prev.fast_stochastic_oscillator_k
           <=. prev_prev.fast_stochastic_oscillator_d
           && prev.fast_stochastic_oscillator_k
              >=. prev.fast_stochastic_oscillator_d
           && i.price >=. prev.price
         in
         match crossover with
         | true -> F.Pass [ "Bullish Crossover" ]
         | false -> F.Fail [ "No Crossover" ]);
      ]
    in
    List.fold_left F.and_fold (Pass []) conditions

  let score (state : 'a State.t) symbol =
    let i = Indicators.get_top state.indicators symbol in
    -1.0 *. i.relative_strength_index

  let num_positions = 3
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let buying_price = buying_order.price in
    let price = State.price state buying_order.symbol in
    let i = Indicators.get_top state.indicators buying_order.symbol in
    let* prev = i.previous in
    let ticks_held = state.tick - buying_order.tick in
    let high_since_purchase =
      Bars.get state.bars buying_order.symbol
      |> Option.get_exn_or "Must have bars in slow crossover"
      |> Util.last_n ticks_held |> Math.max_close |> Item.last
    in
    let conditions =
      [
        (match
           i.fast_stochastic_oscillator_d >=. 80.0
           && ticks_held >= Param.min_holding_period
           &&
           match price >=. buying_price with
           | true -> prev.sma_5 >=. prev.sma_34 && i.sma_5 <=. prev.sma_34
           | false -> true
         with
        | true -> F.Pass [ "high %D" ]
        | false -> F.Fail [ "nope" ]);
        (match
           price <=. Param.stop_loss_multiplier *. high_since_purchase
           && ticks_held >= Param.min_holding_period
         with
        | true -> F.Pass [ "high since purchase" ]
        | false -> F.Fail [ "ok" ]);
      ]
    in
    List.fold_left F.or_fold (Fail []) conditions
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
