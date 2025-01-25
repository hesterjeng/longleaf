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
end

module Buy = Template.Buy_trigger.Make (Buy_inp)

module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) symbol =
    let price = Signal.Indicator.price state symbol in
    Signal.Flag.disjunction state
    @@ [ Signal.Indicator.lower_bb symbol Above price ]
end

module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
