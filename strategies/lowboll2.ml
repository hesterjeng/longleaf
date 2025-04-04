module S = Signal
module SI = S.Indicator
module I = Indicators
module P = I.Point
module F = S.Flag

module Param = struct
  let stop_loss_multiplier = 0.5

  (* let profit_multiplier = 1.04 *)
  let max_holding_period = 600
end

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass (state : 'a State.t) symbol =
    let ( let** ) = F.Infix.( let** ) in
    let** price = State.price state symbol in
    let i = Indicators.get_top state.indicators symbol in
    let conditions =
      [
        (match price /. i.sma_5 >=. 0.95 with
        | true -> F.Pass [ "Above SMA(5) (pass crash block)" ]
        | false -> Fail [ "Below SMA(5) (crash block)" ]);
        (match price <=. i.lower_bollinger with
        | true -> F.Pass [ "Below Lower BB" ]
        | false -> F.Fail [ "Not below Lower BB" ]);
        (match i.relative_strength_index <=. 40.0 with
        | true -> F.Pass [ "Small RSI" ]
        | false -> Fail [ "RSI too large to buy" ]);
        (match i.awesome_oscillator >=. 1.0 with
        | true -> F.Pass [ "Awesome above 1.0" ]
        | false -> Fail [ "Awesome too low to buy" ]);
        (match i.awesome_slow >=. 1.0 with
        | true -> F.Pass [ "Awesome slow above 1.0" ]
        | false -> Fail [ "Awesome slow too low to buy" ]);
        (match i.fast_stochastic_oscillator_k >=. 50.0 with
        | true -> F.Fail [ "FSO %K too high" ]
        | false -> F.Pass [ "FSO %K is low enough" ]);
      ]
    in
    Result.return @@
    List.fold_left F.and_fold (Pass []) conditions

  let score (state : 'a State.t) symbol =
    let ( let* ) = Result.( let* ) in
    let* price = State.price state symbol in
    let lower_bb = I.get_indicator state.indicators symbol P.lower_bollinger in
    Result.return @@
    lower_bb /. price

  let num_positions = 1
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
        (match i.fast_stochastic_oscillator_d >=. 90.0 with
        | true -> F.Pass [ "High FSO %D!" ]
        | false -> F.Fail [ "FSO %D too low to sell" ]);
        (match state.tick >= buying_order.tick + Param.max_holding_period with
        | true -> F.Pass [ "Holding period exceeded" ]
        | false -> F.Fail [ "Holding period OK" ]);
        (match price <=. Param.stop_loss_multiplier *. buying_order.price with
        | true -> F.Pass [ "Stop loss triggered" ]
        | false -> F.Fail [ "Stop loss not triggered" ]);
      ]
    in
    List.fold_left F.or_fold (Fail []) conditions
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
