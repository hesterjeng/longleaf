(** MACD Bollinger Momentum Strategy

    Buy Logic:
    - MACD line crosses above signal line (bullish momentum)
    - Price is near lower Bollinger Band (potential bounce)
    - ADX > 25 (strong trend confirmation)
    - Score based on MACD histogram strength

    Sell Logic:
    - MACD line crosses below signal line (bearish momentum) OR
    - Price touches upper Bollinger Band (potential resistance) OR
    - 8% stop loss OR
    - 15% profit target *)

module Buy_trigger_input : Template.Buy_trigger.INPUT = struct
  let macd_line = Bars.Data.Type.(Tacaml (Tacaml.Indicator.macd_macd ()))
  let macd_signal = Bars.Data.Type.(Tacaml (Tacaml.Indicator.macd_signal ()))
  let macd_histogram = Bars.Data.Type.(Tacaml (Tacaml.Indicator.macd_hist ()))
  let lower_bb = Bars.Data.Type.(Tacaml (Tacaml.Indicator.lower_bband ()))
  let upper_bb = Bars.Data.Type.(Tacaml (Tacaml.Indicator.upper_bband ()))
  let adx = Bars.Data.Type.(Tacaml (Tacaml.Indicator.adx ()))

  let pass (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in

    (* Get current values *)
    let macd_val = Bars.Data.get_top data macd_line in
    let macd_sig = Bars.Data.get_top data macd_signal in
    let macd_hist = Bars.Data.get_top data macd_histogram in
    let lower_band = Bars.Data.get_top data lower_bb in
    let upper_band = Bars.Data.get_top data upper_bb in
    let adx_val = Bars.Data.get_top data adx in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in

    (* Get previous values for crossover detection *)
    let prev_macd = Bars.Data.get data macd_line (State.tick state - 1) in
    let prev_macd_sig = Bars.Data.get data macd_signal (State.tick state - 1) in

    (* Buy conditions *)
    let macd_bullish_cross =
      macd_val >. macd_sig && prev_macd <=. prev_macd_sig
    in
    let near_lower_bb =
      current_price <=. lower_band +. ((upper_band -. lower_band) *. 0.2)
    in
    let strong_trend = adx_val >. 25.0 in
    let positive_momentum = macd_hist >. 0.0 in

    let conditions_met =
      macd_bullish_cross && near_lower_bb && strong_trend && positive_momentum
    in

    let reason =
      if conditions_met then
        [
          Printf.sprintf "MACD bullish cross: %.4f > %.4f (prev: %.4f <= %.4f)"
            macd_val macd_sig prev_macd prev_macd_sig;
          Printf.sprintf "Near lower BB: %.2f <= %.2f (20%% from bottom)"
            current_price
            (lower_band +. ((upper_band -. lower_band) *. 0.2));
          Printf.sprintf "Strong trend ADX: %.2f > 25" adx_val;
          Printf.sprintf "Positive momentum: %.4f > 0" macd_hist;
        ]
      else []
    in

    (* Log buy signals *)
    if conditions_met then
      Eio.traceln
        "BUY SIGNAL: %s - MACD: %.4f>%.4f, Price: %.2f, Lower BB: %.2f, ADX: \
         %.2f, Hist: %.4f"
        (Instrument.symbol instrument)
        macd_val macd_sig current_price lower_band adx_val macd_hist;

    Result.return { Signal.instrument; flag = conditions_met; reason }

  let score (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in
    let macd_hist = Bars.Data.get_top data macd_histogram in
    let adx_val = Bars.Data.get_top data adx in

    (* Higher score for stronger momentum and trend *)
    let score =
      if macd_hist >. 0.0 && adx_val >. 25.0 then
        (macd_hist *. 1000.0) +. (adx_val *. 0.1)
      else 0.0
    in
    Result.return score

  let num_positions = 5 (* Hold maximum 5 positions *)
end

module Sell_trigger_impl : Template.Sell_trigger.S = struct
  let macd_line = Bars.Data.Type.(Tacaml (Tacaml.Indicator.macd_macd ()))
  let macd_signal = Bars.Data.Type.(Tacaml (Tacaml.Indicator.macd_signal ()))
  let upper_bb = Bars.Data.Type.(Tacaml (Tacaml.Indicator.upper_bband ()))

  let make (state : 'a State.t) (symbol : Instrument.t) =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state symbol in

    (* Get current values *)
    let macd_val = Bars.Data.get_top data macd_line in
    let macd_sig = Bars.Data.get_top data macd_signal in
    let upper_band = Bars.Data.get_top data upper_bb in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
    let qty_held = State.qty state symbol in
    let entry_price =
      if qty_held > 0 then
        -.State.cost_basis state symbol /. float_of_int qty_held
      else 0.0
    in

    (* Get previous values for crossover detection *)
    let prev_macd = Bars.Data.get data macd_line (State.tick state - 1) in
    let prev_macd_sig = Bars.Data.get data macd_signal (State.tick state - 1) in

    (* Calculate profit/loss percentage *)
    let profit_pct = (current_price -. entry_price) /. entry_price *. 100.0 in

    (* Sell conditions *)
    let macd_bearish_cross =
      macd_val <. macd_sig && prev_macd >=. prev_macd_sig
    in
    let at_upper_bb = current_price >=. upper_band *. 0.98 in
    (* Within 2% of upper band *)
    let stop_loss = profit_pct <. -8.0 in
    let profit_target = profit_pct >. 15.0 in

    let should_sell =
      macd_bearish_cross || at_upper_bb || stop_loss || profit_target
    in

    let reason =
      if should_sell then
        let primary_reason =
          if macd_bearish_cross then
            Printf.sprintf
              "MACD bearish cross: %.4f < %.4f (prev: %.4f >= %.4f)" macd_val
              macd_sig prev_macd prev_macd_sig
          else if at_upper_bb then
            Printf.sprintf "At upper BB: %.2f >= %.2f" current_price
              (upper_band *. 0.98)
          else if stop_loss then
            Printf.sprintf "Stop loss: %.2f%% < -8%%" profit_pct
          else Printf.sprintf "Profit target: %.2f%% > 15%%" profit_pct
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
         if macd_bearish_cross then "MACD_BEARISH_CROSS"
         else if at_upper_bb then "UPPER_BB_RESISTANCE"
         else if stop_loss then "STOP_LOSS"
         else "PROFIT_TARGET"
       in
       Eio.traceln
         "SELL SIGNAL: %s - %s - MACD: %.4f<%.4f, Price: %.2f, Upper BB: %.2f, \
          P&L: %.2f%%"
         (Instrument.symbol symbol) sell_reason macd_val macd_sig current_price
         upper_band profit_pct);

    Result.return { Signal.instrument = symbol; flag = should_sell; reason }
end

module Buy_trigger = Template.Buy_trigger.Make (Buy_trigger_input)
module Make : Strategy.BUILDER = Template.Make (Buy_trigger) (Sell_trigger_impl)
