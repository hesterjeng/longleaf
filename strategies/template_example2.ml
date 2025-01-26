(* This is an example of how to use the templates to easily make strategies. *)
(* Once the Make functor is instantiated with a backend, the strategy is ready to run. *)
(* Add a hook for it in longleaf_strategies.ml *)

module S = Signal
module SI = S.Indicator
module I = Indicators
module P = I.Point
module F = S.Flag

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass (state : 'a State.t) symbol =
    let price = State.price state symbol in
    F.conjunction state @@ [ SI.lower_bb symbol Above price ]

  let score (state : 'a State.t) symbol =
    let price = State.price state symbol in
    let lower_bb = I.get_indicator state.indicators symbol P.lower_bollinger in
    price -. lower_bb

  let num_positions = 5
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) symbol =
    let price = State.price state symbol in
    Signal.Flag.disjunction state
    @@ [ Signal.Indicator.upper_bb symbol Below price ]
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
