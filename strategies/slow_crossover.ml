module S = Signal
module SI = S.Indicator
module I = Indicators
module P = I.Point
module F = S.Flag

[@@@warning "-26"]

module Param = struct
  let stop_loss_multiplier = 0.98

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
    let conditions =
      [
        (match i.relative_strength_index <=. 40.0 with
        | true -> F.Pass [ "Small RSI" ]
        | false -> Fail [ "RSI too large to buy" ]);
        (* confirmed_crossover state symbol; *)
        (* ( *)
        (*   match price >=. i.sma_34 with *)
        (*   | true -> F.Pass [ "Confirm SMA" ] *)
        (*   | false -> F.Fail [ "Low" ] *)
        (* ); *)
        (match i.fast_stochastic_oscillator_d <=. 20.0 with
        | true -> F.Pass [ "FSO %D <= 20" ]
        | false -> F.Fail [ "FSO %D is too high" ]);
        (let crossover =
           prev.fast_stochastic_oscillator_k
           <=. prev.fast_stochastic_oscillator_d
           && i.fast_stochastic_oscillator_k >=. i.fast_stochastic_oscillator_d
         in
         match crossover with
         | true -> F.Pass [ "Bullish Crossover" ]
         | false -> F.Fail [ "No Crossover" ]);
        (* (match price <=. i.sma_233 with *)
        (* | true -> F.Pass [ "Below SMA confirm" ] *)
        (* | false -> F.Fail [ "price above SMA" ]); *)
      ]
    in
    List.fold_left F.and_fold (Pass []) conditions

  let score (state : 'a State.t) symbol =
    let i = Indicators.get_top state.indicators symbol in
    -1.0 *. i.relative_strength_index

  (* let price = State.price state symbol in *)
  (* let lower_bb = I.get_indicator state.indicators symbol P.lower_bollinger in *)
  (* lower_bb /. price *)

  let num_positions = 1
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let price = State.price state buying_order.symbol in
    let i = Indicators.get_top state.indicators buying_order.symbol in
    (* let* prev = i.previous in *)
    (* This strategy also seems to do OK if we sell no matter what after 6 ticks *)
    (* let conditions = *)
    (*   [ *)
    (*     (match state.tick >= buying_order.tick + Param.max_holding_period with *)
    (*     | true -> *)
    (*       ( *)
    (*         match price >=. buying_order.price with *)
    (*         | false -> F.Pass [ "Price hasn't increased, exit" ] *)
    (*         | true -> *)
    (*           ( *)
    (*             match i.fast_stochastic_oscillator_d >=. 20.0 with *)
    (*             | true -> F.Pass [ "FSO %D greater than 20" ] *)
    (*             | false -> F.Fail [ "FSO %K not high enough" ] *)
    (*           ) *)
    (*       ) *)
    (*       (\* F.Pass [ "Holding period exceeded" ] *\) *)
    (*     | false -> F.Fail [ "Holding period OK" ]); *)
    (*   ] *)
    (* in *)
    let conditions =
      [
        (match i.fast_stochastic_oscillator_k >=. 80.0 with
        | true -> F.Pass [ "high %D" ]
        | false -> F.Fail [ "low %D" ]);
        (* (match price <=. Param.stop_loss_multiplier *. buying_order.price with *)
        (* | true -> F.Pass [ "Price when down after a day" ] *)
        (* | false -> F.Fail [ "Price OK" ]); *)
        (match state.tick >= buying_order.tick + Param.max_holding_period with
        | true -> F.Pass [ "Get out! An hour!!" ]
        | false -> F.Fail [ "Holding period OK" ]);
        (* (match state.tick >= buying_order.tick + 35 with *)
        (* | true -> ( *)
        (*     match *)
        (*       price <=. Param.stop_loss_multiplier *. buying_order.price *)
        (*     with *)
        (*     | true -> F.Pass [ "Price when down after a day" ] *)
        (*     | false -> F.Fail [ "Price OK" ]) *)
        (* | false -> F.Fail [ "Holding period OK" ]); *)
      ]
    in
    List.fold_left F.or_fold (Fail []) conditions
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
