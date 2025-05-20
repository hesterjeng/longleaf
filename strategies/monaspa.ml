module S = Signal

(* module SI = S.Indicator *)
module I = Indicators
module P = I.Point
(* module F = S.Flag *)

(* [@@@warning "-26"] *)

let ( let* ) = Result.( let* )
let ( let+ ) = Result.( let+ )

module Param = struct
  (* let trailing_loss = 0.96 *)
  let stop_loss_multiplier = 0.98
  let holding_period = 40

  (* let profit_multiplier = 1.03 *)
  (* let max_holding_period = 546 *)
end

let bullish_crossover ~(i : P.t) ~(prev : P.t) =
  prev.fso.k <=. prev.fso.d && i.fso.k >=. i.fso.d
(* && i.fso.k -. i.fso.d >. 5.0 *)

let bearish_crossover ~(i : P.t) ~(prev : P.t) =
  prev.fso.k >=. prev.fso.d && i.fso.k <=. i.fso.d
(* && i.fso.d -. i.fso.k >. 5.0 *)

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass (state : 'a State.t) instrument =
    let signal = Signal.make instrument true in
    let ( let&& ) = Signal.let_and signal in
    let ( let$ ) = Signal.let_get_opt signal in
    let+ i = Indicators.get_top state.indicators instrument in
    let$ prev = i.previous in
    (* let&& ()  = (i.relative_strength_index <=. 30.0, "low rsi") in *)
    let&& () = (prev.relative_strength_index <=. 35.0, "low rsi setup") in
    (* let&& ()  = (i.fso.d >=. 10.0, "minimum fso %d setup") in *)
    (* let&& ()  = (prev.price <=. prev.sma_75, "low rsi setup") in *)
    let&& () = (prev.price <=. prev.lower_bollinger_100_1, "low boll") in
    let&& () = (i.price >=. i.sma_34, "low boll") in
    let&& () = (i.price <>. prev.price, "movement") in
    let&& () = (bullish_crossover ~i ~prev, "bullish crossover") in
    (* let&& () = (i.fso.d <=. 20.0, "small fso d") in *)
    (* let&& () = (i.fso.k <=. 20.0, "small fso k") in *)
    (* let&& () = (i.price <=. i.lower_bollinger_100_1, "low boll") in *)
    (* let&& () = (i.price >=. i.sma_34, "low boll") in *)
    signal

  let score (state : 'a State.t) symbol =
    let ( let+ ) = Result.( let+ ) in
    let+ i = Indicators.get_top state.indicators symbol in
    -1.0 *. i.relative_strength_index

  let num_positions = 3
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

let refresh_table : (Order.t, int) Hashtbl.t = Hashtbl.create 10

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let signal = Signal.make buying_order.symbol false in
    let ( let$ ) = Signal.let_get_opt signal in
    let ( let|| ) = Signal.let_or signal in
    let* price = State.price state buying_order.symbol in
    let+ i = Indicators.get_top state.indicators buying_order.symbol in
    (* let+ price_history = Bars.get_res state.bars buying_order.symbol in *)
    let$ prev = i.previous in
    (* let ticks_held = state.tick - buying_order.tick in *)
    (* let profited = price >. buying_order.price in *)
    (* let stoploss = price <=. Param.stop_loss_multiplier *. buying_order.price in *)
    (* let|| () = (i.relative_strength_index >=. 70.0, "bearish") in *)
    (* let|| ()  = (prev.price >=. 0.99 *. (prev.upper_bollinger_100_3) && i.price <=. prev.price, "upper boll") in *)
    let|| () =
      ( prev.price >=. prev.upper_bollinger_100_3 && i.price <=. prev.price,
        "upper boll" )
    in
    let|| () = (i.price <=. 0.99 *. buying_order.price, "drop") in
    let|| () = (i.fso.d_slow >=. 70.0, "high fso slow") in
    let|| () =
      ( state.tick <= buying_order.tick + 2 && price <=. buying_order.price,
        "init decrease" )
    in
    (* let|| () = (bearish_crossover ~i ~prev, "bearish") in *)
    signal
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
