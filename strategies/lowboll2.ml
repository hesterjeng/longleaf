module S = Signal
module SI = S.Indicator
module I = Indicators
module P = I.Point
module F = S.Flag

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass (state : 'a State.t) symbol =
    let price = State.price state symbol in
    let conditions =
      [
        ( Indicators.get_indicator state.indicators symbol P.sma_5
        |> fun sma_5 ->
          match price /. sma_5 >=. 0.95 with
          | true -> F.Pass [ "Above SMA(5) (pass crash block)" ]
          | false -> Fail [ "Below SMA(5) (crash block)" ] );
        ( Indicators.get_indicator state.indicators symbol P.lower_bollinger
        |> fun lower_bb ->
          match price <=. lower_bb with
          | true -> F.Pass [ "Below Lower BB" ]
          | false -> F.Fail [ "Not below Lower BB" ] );
        ( Indicators.get_indicator state.indicators symbol P.rsi |> fun rsi ->
          match rsi <=. 40.0 with
          | true -> F.Pass [ "Small RSI" ]
          | false -> Fail [ "RSI too large to buy" ] );
        ( Indicators.get_indicator state.indicators symbol P.awesome
        |> fun awesome ->
          match awesome >=. 1.0 with
          | true -> F.Pass [ "Awesome above 1.0" ]
          | false -> Fail [ "Awesome too low to buy" ] );
        ( Indicators.get_indicator state.indicators symbol P.awesome_slow
        |> fun awesome_slow ->
          match awesome_slow >=. 1.0 with
          | true -> F.Pass [ "Awesome slow above 1.0" ]
          | false -> Fail [ "Awesome slow too low to buy" ] );
        ( Indicators.get_indicator state.indicators symbol P.fso_pk
        |> fun fso_pk ->
          match fso_pk >=. 50.0 with
          | true -> F.Fail [ "FSO %K too high" ]
          | false -> F.Pass [ "FSO %K is low enough" ] );
      ]
    in
    List.fold_left F.and_fold (Pass []) conditions

  let score (state : 'a State.t) symbol =
    let price = State.price state symbol in
    let lower_bb = I.get_indicator state.indicators symbol P.lower_bollinger in
    lower_bb /. price

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
