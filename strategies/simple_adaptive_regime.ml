(** Simplified Adaptive Regime Strategy

    Simplified version with fewer indicators:
    - Only ADX, RSI, Bollinger Bands, Volume
    - Same regime detection but simpler conditions **)

module Buy_trigger_input : Template.Buy_trigger.INPUT = struct
  let adx = Bars.Data.Type.(Tacaml (F Adx))
  let rsi = Bars.Data.Type.(Tacaml (F Rsi))
  let lower_bb = Bars.Data.Type.(Tacaml (F LowerBBand))
  let upper_bb = Bars.Data.Type.(Tacaml (F UpperBBand))

  let pass (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = Bars.get state.bars instrument in

    (* Avoid early ticks where indicators aren't stable *)
    if state.tick < 50 then
      Result.return { Signal.instrument; flag = false; reason = [] }
    else
      (* Get current values *)
      let adx_val = Bars.Data.get_top data adx in
      let rsi_val = Bars.Data.get_top data rsi in
      let lower_band = Bars.Data.get_top data lower_bb in
      let upper_band = Bars.Data.get_top data upper_bb in
      let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
      let current_volume = Bars.Data.get_top data Bars.Data.Type.Volume in

      (* Calculate average volume *)
      let avg_volume =
        let volumes =
          List.init 20 (fun i ->
              Bars.Data.get data Bars.Data.Type.Volume (state.tick - i - 1))
        in
        List.fold_left ( +. ) 0.0 volumes /. 20.0
      in

      (* Market regime detection *)
      let is_trending = adx_val >. 30.0 in

      (* Simplified conditions *)
      let trending_conditions =
        if is_trending then
          (* For trending: RSI not overbought, price above middle BB, high volume *)
          let rsi_ok = rsi_val <. 70.0 in
          let bb_middle = (lower_band +. upper_band) /. 2.0 in
          let price_ok = current_price >. bb_middle in
          let volume_ok = current_volume >. avg_volume *. 1.5 in
          rsi_ok && price_ok && volume_ok
        else false
      in

      (* Ranging market conditions *)
      let ranging_conditions =
        if not is_trending then
          (* For ranging: RSI oversold, price near lower BB *)
          let rsi_oversold = rsi_val <. 30.0 in
          let near_lower_bb =
            current_price <=. lower_band +. ((upper_band -. lower_band) *. 0.15)
          in
          rsi_oversold && near_lower_bb
        else false
      in

      let conditions_met = trending_conditions || ranging_conditions in

      let reason =
        if conditions_met then
          let regime = if is_trending then "TRENDING" else "RANGING" in
          [
            Printf.sprintf "Regime: %s (ADX: %.2f)" regime adx_val;
            Printf.sprintf "RSI: %.2f" rsi_val;
            Printf.sprintf "Price vs BB: %.2f (Lower: %.2f, Upper: %.2f)"
              current_price lower_band upper_band;
            Printf.sprintf "Volume: %.0fx avg" (current_volume /. avg_volume);
          ]
        else []
      in

      (* Log buy signals *)
      (if conditions_met then
         let regime = if is_trending then "TRENDING" else "RANGING" in
         Eio.traceln
           "BUY SIGNAL: %s - %s regime - ADX: %.2f, RSI: %.2f, Price: %.2f, \
            Vol: %.0fx"
           (Instrument.symbol instrument)
           regime adx_val rsi_val current_price
           (current_volume /. avg_volume));

      Result.return { Signal.instrument; flag = conditions_met; reason }

  let score (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = Bars.get state.bars instrument in

    if state.tick < 50 then Result.return 0.0
    else
      let adx_val = Bars.Data.get_top data adx in
      let rsi_val = Bars.Data.get_top data rsi in
      let current_volume = Bars.Data.get_top data Bars.Data.Type.Volume in

      (* Calculate average volume *)
      let avg_volume =
        let volumes =
          List.init 20 (fun i ->
              Bars.Data.get data Bars.Data.Type.Volume (state.tick - i - 1))
        in
        List.fold_left ( +. ) 0.0 volumes /. 20.0
      in

      (* Simple scoring based on regime and RSI *)
      let regime_multiplier = if adx_val >. 30.0 then 1.2 else 0.9 in
      let rsi_score =
        if adx_val >. 30.0 then 100.0 -. rsi_val else 30.0 -. rsi_val
      in
      let volume_score = current_volume /. avg_volume *. 5.0 in

      let score = (rsi_score +. volume_score) *. regime_multiplier in
      Result.return (Float.max 0.0 score)

  let num_positions = 6 (* Hold maximum 6 positions *)
end

module Sell_trigger_impl : Template.Sell_trigger.S = struct
  let adx = Bars.Data.Type.(Tacaml (F Adx))
  let rsi = Bars.Data.Type.(Tacaml (F Rsi))
  let upper_bb = Bars.Data.Type.(Tacaml (F UpperBBand))

  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let ( let* ) = Result.( let* ) in
    let* data = Bars.get state.bars buying_order.symbol in

    (* Get current values *)
    let adx_val = Bars.Data.get_top data adx in
    let rsi_val = Bars.Data.get_top data rsi in
    let upper_band = Bars.Data.get_top data upper_bb in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
    let entry_price = buying_order.price in

    (* Calculate holding period *)
    let holding_period = state.tick - buying_order.tick in

    (* Calculate profit/loss percentage *)
    let profit_pct = (current_price -. entry_price) /. entry_price *. 100.0 in

    (* Market regime detection *)
    let is_trending = adx_val >. 30.0 in

    (* Simplified exit conditions *)
    let regime_exit =
      if is_trending then
        (* Trending: RSI overbought or near upper BB *)
        rsi_val >. 70.0 || current_price >=. upper_band *. 0.98
      else
        (* Ranging: RSI overbought *)
        rsi_val >. 70.0
    in

    (* Universal exit conditions *)
    let profit_target = profit_pct >. 8.0 in
    let stop_loss = profit_pct <. -4.0 in
    let max_holding = holding_period > 15 in
    (* Shorter holding period *)

    let should_sell =
      regime_exit || profit_target || stop_loss || max_holding
    in

    let reason =
      if should_sell then
        let regime = if is_trending then "TRENDING" else "RANGING" in
        let primary_reason =
          if regime_exit then
            if is_trending then
              if rsi_val >. 70.0 then "RSI overbought in trending market"
              else "Near upper Bollinger Band"
            else "RSI overbought in ranging market"
          else if profit_target then
            Printf.sprintf "Profit target: %.2f%% > 8%%" profit_pct
          else if stop_loss then
            Printf.sprintf "Stop loss: %.2f%% < -4%%" profit_pct
          else Printf.sprintf "Max holding: %d days > 15" holding_period
        in
        [
          Printf.sprintf "%s regime exit" regime;
          primary_reason;
          Printf.sprintf "Entry: %.2f, Current: %.2f, RSI: %.2f" entry_price
            current_price rsi_val;
        ]
      else []
    in

    (* Log sell signals *)
    (if should_sell then
       let regime = if is_trending then "TRENDING" else "RANGING" in
       let sell_reason =
         if regime_exit then "REGIME_EXIT"
         else if profit_target then "PROFIT_TARGET"
         else if stop_loss then "STOP_LOSS"
         else "MAX_HOLDING"
       in
       Eio.traceln
         "SELL SIGNAL: %s - %s - %s regime - ADX: %.2f, RSI: %.2f, P&L: \
          %.2f%%, Hold: %dd"
         (Instrument.symbol buying_order.symbol)
         sell_reason regime adx_val rsi_val profit_pct holding_period);

    Result.return
      { Signal.instrument = buying_order.symbol; flag = should_sell; reason }
end

module Buy_trigger = Template.Buy_trigger.Make (Buy_trigger_input)
module Make : Strategy.BUILDER = Template.Make (Buy_trigger) (Sell_trigger_impl)
