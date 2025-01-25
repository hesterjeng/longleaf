(* This is an example of how to use the templates to easily make strategies. *)
(* Once the Make functor is instantiated with a backend, the strategy is ready to run. *)
(* Add a hook for it in longleaf_strategies.ml *)

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass (state : 'a State.t) symbol =
    let price = Signal.Indicator.price state symbol in
    Signal.Flag.conjunction state
    @@ [ Signal.Indicator.upper_bb symbol Below price ]

  let score (state : 'a State.t) symbol =
    let price = Signal.Indicator.price state symbol in
    let upper_bb =
      Indicators.get_indicator state.indicators symbol
        Indicators.Point.lower_bollinger
      |> Option.get_exn_or
           "Must be able to get upper_bb in Template_example.Buy.score"
    in
    price -. upper_bb

  let num_positions = 5
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) symbol =
    let price = Signal.Indicator.price state symbol in
    Signal.Flag.disjunction state
    @@ [ Signal.Indicator.lower_bb symbol Above price ]
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
