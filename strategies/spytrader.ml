module S = Signal
module SI = S.Indicator
module I = Indicators
module P = I.Point
module F = S.Flag

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let num_positions = 1

  let pass (state : 'a State.t) symbol =
    let i = Indicators.get_top state.indicators symbol in
    match symbol with
    | Security "SPY" -> (
        match i.fast_stochastic_oscillator_k <=. 10.0 with
        | true -> F.Pass [ "Small K" ]
        | false -> F.Fail [ "Don't buy" ])
    | _ -> F.Fail []

  let score _ symbol =
    match symbol with Instrument.Security "SPY" -> 1.0 | _ -> 0.0
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let price = State.price state buying_order.symbol in
    let i = Indicators.get_top state.indicators buying_order.symbol in
    let conditions =
      [
        (match
           i.fast_stochastic_oscillator_d >=. 90.0
           && price >=. buying_order.price
         with
        | true -> F.Pass [ "Get out! High FSO %K" ]
        | false -> F.Fail [ "OK" ]);
      ]
    in
    List.fold_left F.or_fold (Fail []) conditions
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
