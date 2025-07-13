(** Volume Breakout Strategy

    Buy Logic:
    - Price breaks above 20-day SMA with volume > 2x average volume
    - Williams %R < -50 (not overbought)
    - ATR increasing (volatility expansion)
    - Score based on volume multiplier and price distance from SMA

    Sell Logic:
    - Price falls below 10-day EMA (trend reversal) OR
    - Williams %R > -20 (overbought) OR
    - 6% stop loss OR
    - 12% profit target *)

module Buy_trigger_input : Template.Buy_trigger.INPUT = struct
  let sma_20 = Bars.Data.Type.(Tacaml (F Sma))
  let ema_10 = Bars.Data.Type.(Tacaml (F Ema))
  let williams_r = Bars.Data.Type.(Tacaml (F Willr))
  let atr = Bars.Data.Type.(Tacaml (F Atr))
  let volume_sma = Bars.Data.Type.(Tacaml (F Sma))
  (* We'll use this for volume average *)

  let pass (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in

    (* Get current values *)
    let sma_val = Bars.Data.get_top data sma_20 in
    let williams_val = Bars.Data.get_top data williams_r in
    let atr_val = Bars.Data.get_top data atr in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
    let current_volume = Bars.Data.get_top data Bars.Data.Type.Volume in

    (* Get previous values *)
    let prev_price =
      Bars.Data.get data Bars.Data.Type.Close (State.tick state - 1)
    in
    let prev_atr = Bars.Data.get data atr (State.tick state - 1) in

    (* Calculate average volume (approximate using recent volume) *)
    let avg_volume =
      let recent_volumes =
        List.init 10 (fun i ->
            Bars.Data.get data Bars.Data.Type.Volume (State.tick state - i - 1))
      in
      List.fold_left ( +. ) 0.0 recent_volumes /. 10.0
    in

    (* Buy conditions *)
    let price_breakout = current_price >. sma_val && prev_price <=. sma_val in
    let high_volume = current_volume >. avg_volume *. 2.0 in
    let not_overbought = williams_val <. -50.0 in
    let expanding_volatility = atr_val >. prev_atr in
    let price_strength = (current_price -. sma_val) /. sma_val >. 0.01 in
    (* > 1% above SMA *)

    let conditions_met =
      price_breakout && high_volume && not_overbought && expanding_volatility
      && price_strength
    in

    let reason =
      if conditions_met then
        [
          Printf.sprintf "Price breakout: %.2f > %.2f (SMA)" current_price
            sma_val;
          Printf.sprintf "High volume: %.0f > %.0f (2x avg)" current_volume
            (avg_volume *. 2.0);
          Printf.sprintf "Not overbought: %.2f < -50" williams_val;
          Printf.sprintf "Expanding volatility: %.4f > %.4f" atr_val prev_atr;
          Printf.sprintf "Price strength: %.2f%% above SMA"
            ((current_price -. sma_val) /. sma_val *. 100.0);
        ]
      else []
    in

    (* Log buy signals *)
    if conditions_met then
      Eio.traceln
        "BUY SIGNAL: %s - Breakout: %.2f>%.2f, Volume: %.0fx, Williams: %.2f, \
         ATR: %.4f>%.4f"
        (Instrument.symbol instrument)
        current_price sma_val
        (current_volume /. avg_volume)
        williams_val atr_val prev_atr;

    Result.return { Signal.instrument; flag = conditions_met; reason }

  let score (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
    let sma_val = Bars.Data.get_top data sma_20 in
    let current_volume = Bars.Data.get_top data Bars.Data.Type.Volume in
    let williams_val = Bars.Data.get_top data williams_r in

    (* Calculate average volume *)
    let avg_volume =
      let recent_volumes =
        List.init 10 (fun i ->
            Bars.Data.get data Bars.Data.Type.Volume (State.tick state - i - 1))
      in
      List.fold_left ( +. ) 0.0 recent_volumes /. 10.0
    in

    (* Score based on breakout strength and volume *)
    let price_strength = (current_price -. sma_val) /. sma_val *. 100.0 in
    let volume_multiplier = current_volume /. avg_volume in
    let williams_factor = -1.0 *. williams_val /. 100.0 in
    (* Convert to positive 0-1 scale *)

    let score =
      if
        current_price >. sma_val
        && current_volume >. avg_volume *. 2.0
        && williams_val <. -50.0
      then price_strength *. volume_multiplier *. williams_factor
      else 0.0
    in
    Result.return score

  let num_positions = 4 (* Hold maximum 4 positions *)
end

module Sell_trigger_impl : Template.Sell_trigger.S = struct
  let ema_10 = Bars.Data.Type.(Tacaml (F Ema))
  let williams_r = Bars.Data.Type.(Tacaml (F Willr))

  let make (state : 'a State.t) (symbol : Instrument.t) =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state symbol in

    (* Get current values *)
    let ema_val = Bars.Data.get_top data ema_10 in
    let williams_val = Bars.Data.get_top data williams_r in
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
    let trend_reversal = current_price <. ema_val in
    let overbought = williams_val >. -20.0 in
    let stop_loss = profit_pct <. -6.0 in
    let profit_target = profit_pct >. 12.0 in

    let should_sell =
      trend_reversal || overbought || stop_loss || profit_target
    in

    let reason =
      if should_sell then
        let primary_reason =
          if trend_reversal then
            Printf.sprintf "Trend reversal: %.2f < %.2f (EMA)" current_price
              ema_val
          else if overbought then
            Printf.sprintf "Overbought: %.2f > -20" williams_val
          else if stop_loss then
            Printf.sprintf "Stop loss: %.2f%% < -6%%" profit_pct
          else Printf.sprintf "Profit target: %.2f%% > 12%%" profit_pct
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
         if trend_reversal then "TREND_REVERSAL"
         else if overbought then "OVERBOUGHT"
         else if stop_loss then "STOP_LOSS"
         else "PROFIT_TARGET"
       in
       Eio.traceln
         "SELL SIGNAL: %s - %s - Price: %.2f, EMA: %.2f, Williams: %.2f, P&L: \
          %.2f%%"
         (Instrument.symbol symbol) sell_reason current_price ema_val
         williams_val profit_pct);

    Result.return { Signal.instrument = symbol; flag = should_sell; reason }
end

module Buy_trigger = Template.Buy_trigger.Make (Buy_trigger_input)
module Make : Strategy.BUILDER = Template.Make (Buy_trigger) (Sell_trigger_impl)
