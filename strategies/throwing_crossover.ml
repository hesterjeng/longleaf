module S = Signal

(* module SI = S.Indicator *)
module I = Indicators
module P = I.Point
(* module F = S.Flag *)

(* [@@@warning "-26"] *)

let ( let* ) = Result.( let* )
let ( let+ ) = Result.( let+ )

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
    let signal = Signal.make instrument true in
    let ( let&& ) = Signal.let_and signal in
    let ( let$ ) = Signal.let_get_opt signal in
    let+ i = State.indicators state instrument in
    let$ prev = State.indicators_back_n state instrument 1 in
    (* let$ prev_prev = prev.previous in *)
    let&& () = (prev.fso.k <=. prev.fso.d, "prev k <= d") in
    let&& () = (i.fso.k -. i.fso.d >=. 20.0, " k >= d by 20") in
    let&& () = (i.volume >= prev.volume, "first volume confirm") in
    (* let&& () = (i.volume >= prev_prev.volume, "second volume confirm") in *)
    signal

  let score (state : 'a State.t) symbol =
    let ( let+ ) = Result.( let+ ) in
    let+ i = State.indicators state symbol in
    -1.0 *. i.relative_strength_index

  let num_positions = 1
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let signal = Signal.make buying_order.symbol false in
    let ( let$ ) = Signal.let_get_opt signal in
    let ( let|| ) = Signal.let_or signal in
    let* price = State.price state buying_order.symbol in
    let* i = State.indicators state buying_order.symbol in
    let+ price_history =
      match Bars.get state.bars buying_order.symbol with
      | Some x -> Ok x
      | None ->
        Eio.traceln "throwing_crossover: Missing price history for symbol";
        Error.missing_data "missing price history in throwing crossover"
    in
    let$ prev = State.indicators_back_n state buying_order.symbol 1 in
    let ticks_held = state.tick - buying_order.tick in
    let high_since_purchase =
      Util.last_n ticks_held price_history |> Math.max_close |> Item.last
    in
    let holding_period = ticks_held >= Param.min_holding_period in
    let price_decreasing =
      prev.sma_5 >=. prev.sma_34 && i.sma_5 <=. prev.sma_34
    in
    let profited = price >=. buying_order.price in
    let high_fso = i.fso.d >=. 80.0 in
    let stoploss =
      price <=. Param.stop_loss_multiplier *. high_since_purchase
    in
    let|| () =
      ( (holding_period && high_fso
        && if profited then price_decreasing else true),
        "high_fso" )
    in
    let|| () = (holding_period && stoploss, "stoploss") in
    let|| () =
      ( holding_period && (not profited) && i.ema_12 <=. prev.ema_12,
        "unprofitable exit" )
    in
    signal
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
