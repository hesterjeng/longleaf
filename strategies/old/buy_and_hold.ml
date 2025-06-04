module S = Signal
module SI = S.Indicator
module I = Indicators
module P = I.Point
module F = S.Flag

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let num_positions = 1
  let ready_to_buy = ref true

  let pass (state : 'a State.t) symbol =
    let _ = state in
    match symbol with
    | Instrument.Security "SPY" when !ready_to_buy ->
      ready_to_buy := false;
      F.Pass [ "Buy SPY" ]
    | _ -> F.Fail []

  let score _ symbol =
    match symbol with
    | Instrument.Security "SPY" -> 1.0
    | _ -> 0.0
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) ~buying_order =
    let _ = (state, buying_order) in
    F.Fail [ "Never sell" ]
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
