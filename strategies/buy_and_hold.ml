module S = Signal

(* module SI = S.Indicator *)

(* module P = I.Point *)
(* module F = S.Flag *)

(* We need a module to see what symbols pass our buy filter, and a way to score the passes *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let num_positions = 1
  let ready_to_buy = ref true

  let pass (_state : 'a State.t) symbol =
    let signal = Signal.make symbol true in
    let ( let&& ) = Signal.let_and signal in
    Result.return
    @@
    let&& () =
      ( !ready_to_buy && Instrument.equal symbol (Instrument.security "SPY"),
        "init" )
    in
    ready_to_buy := false;
    signal

  let score _ symbol =
    match symbol with
    | Instrument.Security "SPY" -> Ok 1.0
    | _ -> Ok 0.0
end

(* The functor uses the score to choose the symbol with the highest score *)
module Buy = Template.Buy_trigger.Make (Buy_inp)

(* We will sell any symbol that meets the requirement *)
module Sell : Template.Sell_trigger.S = struct
  let make (_state : 'a State.t) ~(buying_order : Order.t) =
    Result.return @@ Signal.make buying_order.symbol false
end

(* Create a strategy with our parameters *)
module Make : Strategy.BUILDER = Template.Make (Buy) (Sell)
