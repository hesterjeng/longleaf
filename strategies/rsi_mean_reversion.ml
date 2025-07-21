(** RSI Mean Reversion Strategy

    Buy Logic:
    - RSI < 30 (oversold condition)
    - Price above 20-day SMA (trend filter)
    - Score based on how oversold (lower RSI = higher score)

    Sell Logic:
    - RSI > 70 (overbought) OR
    - 5% stop loss OR
    - 10% profit target *)

module Buy_trigger_input : Template.Buy_trigger.INPUT = struct
  let rsi_indicator = Bars.Data.Type.(Tacaml (Tacaml.Indicator.rsi ()))

  let sma_indicator =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.sma ~timeperiod:20 ()))

  let pass (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in

    (* Get current RSI and SMA values *)
    let rsi_value = Bars.Data.get_top data rsi_indicator in
    let sma_value = Bars.Data.get_top data sma_indicator in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in

    (* Buy conditions - check for valid indicator values *)
    let rsi_oversold = rsi_value <. 30.0 && not (Float.is_nan rsi_value) in
    let above_sma =
      current_price >. sma_value && not (Float.is_nan sma_value)
    in
    let conditions_met = rsi_oversold && above_sma in

    let reason =
      if conditions_met then
        [
          Printf.sprintf "RSI oversold: %.2f < 30" rsi_value;
          Printf.sprintf "Price above SMA: %.2f > %.2f" current_price sma_value;
        ]
      else []
    in

    (* Log buy signals *)
    if conditions_met then
      Eio.traceln
        "BUY SIGNAL: %s - RSI: %.2f, SMA: %.2f, Price: %.2f, Score: %.2f"
        (Instrument.symbol instrument)
        rsi_value sma_value current_price (30.0 -. rsi_value);

    Result.return { Signal.instrument; flag = conditions_met; reason }

  let score (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in
    let rsi_value = Bars.Data.get_top data rsi_indicator in

    (* Lower RSI = higher score (more oversold = better buy opportunity) *)
    let score =
      if rsi_value <. 30.0 && not (Float.is_nan rsi_value) then
        30.0 -. rsi_value
      else 0.0
    in
    Result.return score

  let num_positions = 3 (* Hold maximum 3 positions *)
end

module Sell_trigger_impl : Template.Sell_trigger.S = struct
  let rsi_indicator = Bars.Data.Type.(Tacaml (Tacaml.Indicator.rsi ()))

  let make (state : 'a State.t) (symbol : Instrument.t) =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state symbol in

    (* Get current values *)
    let rsi_value = Bars.Data.get_top data rsi_indicator in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
    let qty_held = State.qty state symbol in
    let entry_price =
      if qty_held > 0 then
        -.State.cost_basis state symbol /. float_of_int qty_held
      else 0.0
    in

    (* Calculate profit/loss percentage *)
    let profit_pct = (current_price -. entry_price) /. entry_price *. 100.0 in

    (* Sell conditions *)
    let rsi_overbought = rsi_value >. 70.0 in
    let stop_loss = profit_pct <. -5.0 in
    let profit_target = profit_pct >. 10.0 in

    let should_sell = rsi_overbought || stop_loss || profit_target in

    let reason =
      if should_sell then
        let primary_reason =
          if rsi_overbought then
            Printf.sprintf "RSI overbought: %.2f > 70" rsi_value
          else if stop_loss then
            Printf.sprintf "Stop loss: %.2f%% < -5%%" profit_pct
          else Printf.sprintf "Profit target: %.2f%% > 10%%" profit_pct
        in
        [
          primary_reason;
          Printf.sprintf "Entry: %.2f, Current: %.2f" entry_price current_price;
        ]
      else []
    in

    (* Log sell signals *)
    (if should_sell then
       let sell_reason =
         if rsi_overbought then "RSI_OVERBOUGHT"
         else if stop_loss then "STOP_LOSS"
         else "PROFIT_TARGET"
       in
       Eio.traceln
         "SELL SIGNAL: %s - %s - RSI: %.2f, Entry: %.2f, Current: %.2f, P&L: \
          %.2f%%"
         (Instrument.symbol symbol) sell_reason rsi_value entry_price
         current_price profit_pct);

    Result.return { Signal.instrument = symbol; flag = should_sell; reason }
end

module Buy_trigger = Template.Buy_trigger.Make (Buy_trigger_input)
module Make : Strategy.BUILDER = Template.Make (Buy_trigger) (Sell_trigger_impl)
