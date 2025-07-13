(** Adaptive Momentum Regime Strategy

    This is a sophisticated multi-indicator strategy that adapts to market
    regimes:

    Market Regime Detection:
    - ADX > 25: Trending market (use momentum signals)
    - ADX < 25: Ranging market (use mean reversion)
    - ATR percentile: Volatility regime (high vol = reduce position size)

    Buy Logic (Trending Regime):
    - MACD histogram increasing for 3+ periods
    - Price above 50-day EMA and 20-day EMA > 50-day EMA (trend alignment)
    - CCI between -100 and +100 (not extreme)
    - Parabolic SAR below price (trend confirmation)
    - Volume > 1.5x 20-day average

    Buy Logic (Ranging Regime):
    - Stochastic %K < 20 and %D < 20 (oversold)
    - Williams %R < -80 (oversold confirmation)
    - Price near lower Bollinger Band
    - MFI < 30 (money flow oversold)

    Sell Logic:
    - Trending: MACD histogram decreasing for 2+ periods OR Parabolic SAR above
      price
    - Ranging: Stochastic %K > 80 OR upper Bollinger Band
    - Universal: 12% profit target, 6% stop loss, or maximum 20-day holding
      period

    Position Sizing:
    - Base size adjusted by ATR percentile (high volatility = smaller positions)
    - Trending regime: 1.5x base size
    - Ranging regime: 0.8x base size *)

module Buy_trigger_input : Template.Buy_trigger.INPUT = struct
  let adx = Bars.Data.Type.(Tacaml (F Adx))
  let atr = Bars.Data.Type.(Tacaml (F Atr))
  let macd_line = Bars.Data.Type.(Tacaml (F Macd_MACD))
  let macd_signal = Bars.Data.Type.(Tacaml (F Macd_MACDSignal))
  let macd_histogram = Bars.Data.Type.(Tacaml (F Macd_MACDHist))
  let ema_20 = Bars.Data.Type.(Tacaml (F Ema))
  let ema_50 = Bars.Data.Type.(Tacaml (F Ema))
  let cci = Bars.Data.Type.(Tacaml (F Cci))
  let sar = Bars.Data.Type.(Tacaml (F Sar))
  let stoch_k = Bars.Data.Type.(Tacaml (F Stoch_SlowK))
  let stoch_d = Bars.Data.Type.(Tacaml (F Stoch_SlowD))
  let williams_r = Bars.Data.Type.(Tacaml (F Willr))
  let lower_bb = Bars.Data.Type.(Tacaml (F LowerBBand))
  let upper_bb = Bars.Data.Type.(Tacaml (F UpperBBand))
  let mfi = Bars.Data.Type.(Tacaml (F Mfi))

  let pass (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in

    (* Avoid early ticks where indicators aren't stable *)
    if State.tick state < 50 then
      Result.return { Signal.instrument; flag = false; reason = [] }
    else
      (* Get current values *)
      let adx_val = Bars.Data.get_top data adx in
      let atr_val = Bars.Data.get_top data atr in
      let macd_hist = Bars.Data.get_top data macd_histogram in
      let ema_20_val = Bars.Data.get_top data ema_20 in
      let ema_50_val = Bars.Data.get_top data ema_50 in
      let cci_val = Bars.Data.get_top data cci in
      let sar_val = Bars.Data.get_top data sar in
      let stoch_k_val = Bars.Data.get_top data stoch_k in
      let stoch_d_val = Bars.Data.get_top data stoch_d in
      let williams_val = Bars.Data.get_top data williams_r in
      let lower_band = Bars.Data.get_top data lower_bb in
      let upper_band = Bars.Data.get_top data upper_bb in
      let mfi_val = Bars.Data.get_top data mfi in
      let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
      let current_volume = Bars.Data.get_top data Bars.Data.Type.Volume in

      (* Get historical values for momentum analysis *)
      let macd_hist_1 =
        Bars.Data.get data macd_histogram (State.tick state - 1)
      in
      let macd_hist_2 =
        Bars.Data.get data macd_histogram (State.tick state - 2)
      in
      let macd_hist_3 =
        Bars.Data.get data macd_histogram (State.tick state - 3)
      in

      (* Calculate 20-day average volume *)
      let avg_volume =
        let volumes =
          List.init 20 (fun i ->
              Bars.Data.get data Bars.Data.Type.Volume (State.tick state - i - 1))
        in
        List.fold_left ( +. ) 0.0 volumes /. 20.0
      in

      (* Calculate ATR percentile (rough approximation) *)
      let atr_history =
        List.init 20 (fun i ->
            Bars.Data.get data atr (State.tick state - i - 1))
      in
      let atr_sorted = List.sort Float.compare atr_history in
      let atr_percentile =
        let pos = List.find_index (fun x -> x >=. atr_val) atr_sorted in
        match pos with
        | Some idx -> Float.of_int idx /. 20.0
        | None -> 1.0
      in

      (* Market regime detection - Testing ADX threshold of 30 *)
      let is_trending = adx_val >. 30.0 in
      let is_high_vol = atr_percentile >. 0.8 in

      (* Trending market conditions *)
      let trending_conditions =
        if is_trending then
          let macd_momentum =
            macd_hist >. macd_hist_1 && macd_hist_1 >. macd_hist_2
            && macd_hist_2 >. macd_hist_3
          in
          let trend_alignment =
            current_price >. ema_50_val && ema_20_val >. ema_50_val
          in
          let cci_neutral = cci_val >. -100.0 && cci_val <. 100.0 in
          let sar_bullish = sar_val <. current_price in
          let volume_confirmation = current_volume >. avg_volume *. 1.5 in
          macd_momentum && trend_alignment && cci_neutral && sar_bullish
          && volume_confirmation
        else false
      in

      (* Ranging market conditions *)
      let ranging_conditions =
        if not is_trending then
          let stoch_oversold = stoch_k_val <. 20.0 && stoch_d_val <. 20.0 in
          let williams_oversold = williams_val <. -80.0 in
          let near_lower_bb =
            current_price <=. lower_band +. ((upper_band -. lower_band) *. 0.1)
          in
          let mfi_oversold = mfi_val <. 30.0 in
          stoch_oversold && williams_oversold && near_lower_bb && mfi_oversold
        else false
      in

      (* Don't trade in extremely high volatility *)
      let vol_filter = not is_high_vol in

      let conditions_met =
        (trending_conditions || ranging_conditions) && vol_filter
      in

      let reason =
        if conditions_met then
          let regime = if is_trending then "TRENDING" else "RANGING" in
          let base_reasons =
            [
              Printf.sprintf "Regime: %s (ADX: %.2f)" regime adx_val;
              Printf.sprintf "ATR percentile: %.2f" atr_percentile;
              Printf.sprintf "Volume: %.0fx avg" (current_volume /. avg_volume);
            ]
          in
          let specific_reasons =
            if is_trending then
              [
                Printf.sprintf "MACD momentum: %.4f > %.4f > %.4f > %.4f"
                  macd_hist macd_hist_1 macd_hist_2 macd_hist_3;
                Printf.sprintf "Trend alignment: %.2f > %.2f, %.2f > %.2f"
                  current_price ema_50_val ema_20_val ema_50_val;
                Printf.sprintf "CCI neutral: %.2f" cci_val;
                Printf.sprintf "SAR bullish: %.2f < %.2f" sar_val current_price;
              ]
            else
              [
                Printf.sprintf "Stochastic oversold: K=%.2f, D=%.2f" stoch_k_val
                  stoch_d_val;
                Printf.sprintf "Williams oversold: %.2f" williams_val;
                Printf.sprintf "Near lower BB: %.2f <= %.2f" current_price
                  (lower_band +. ((upper_band -. lower_band) *. 0.1));
                Printf.sprintf "MFI oversold: %.2f" mfi_val;
              ]
          in
          base_reasons @ specific_reasons
        else []
      in

      (* Log buy signals *)
      (if conditions_met then
         let regime = if is_trending then "TRENDING" else "RANGING" in
         Eio.traceln
           "BUY SIGNAL: %s - %s regime - ADX: %.2f, ATR%%: %.2f, Vol: %.0fx, \
            Price: %.2f"
           (Instrument.symbol instrument)
           regime adx_val atr_percentile
           (current_volume /. avg_volume)
           current_price);

      Result.return { Signal.instrument; flag = conditions_met; reason }

  let score (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in

    if State.tick state < 50 then Result.return 0.0
    else
      let adx_val = Bars.Data.get_top data adx in
      let atr_val = Bars.Data.get_top data atr in
      let macd_hist = Bars.Data.get_top data macd_histogram in
      let current_volume = Bars.Data.get_top data Bars.Data.Type.Volume in

      (* Calculate average volume *)
      let avg_volume =
        let volumes =
          List.init 20 (fun i ->
              Bars.Data.get data Bars.Data.Type.Volume (State.tick state - i - 1))
        in
        List.fold_left ( +. ) 0.0 volumes /. 20.0
      in

      (* Calculate ATR percentile *)
      let atr_history =
        List.init 20 (fun i ->
            Bars.Data.get data atr (State.tick state - i - 1))
      in
      let atr_sorted = List.sort Float.compare atr_history in
      let atr_percentile =
        let pos = List.find_index (fun x -> x >=. atr_val) atr_sorted in
        match pos with
        | Some idx -> Float.of_int idx /. 20.0
        | None -> 1.0
      in

      (* Score based on regime and strength - Testing ADX threshold of 30 *)
      let regime_multiplier = if adx_val >. 30.0 then 1.5 else 0.8 in
      let momentum_score = Float.abs macd_hist *. 1000.0 in
      let volume_score = current_volume /. avg_volume *. 10.0 in
      let vol_penalty = if atr_percentile >. 0.8 then 0.3 else 1.0 in

      let score =
        (momentum_score +. volume_score) *. regime_multiplier *. vol_penalty
      in
      Result.return score

  let num_positions = 6 (* Hold maximum 6 positions *)
end

module Sell_trigger_impl : Template.Sell_trigger.S = struct
  let adx = Bars.Data.Type.(Tacaml (F Adx))
  let macd_histogram = Bars.Data.Type.(Tacaml (F Macd_MACDHist))
  let sar = Bars.Data.Type.(Tacaml (F Sar))
  let stoch_k = Bars.Data.Type.(Tacaml (F Stoch_SlowK))
  let upper_bb = Bars.Data.Type.(Tacaml (F UpperBBand))
  let ema_10 = Bars.Data.Type.(Tacaml (F Ema))

  let make (state : 'a State.t) ~(buying_order : Order.t) =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state buying_order.symbol in

    (* Get current values *)
    let adx_val = Bars.Data.get_top data adx in
    let macd_hist = Bars.Data.get_top data macd_histogram in
    let sar_val = Bars.Data.get_top data sar in
    let stoch_k_val = Bars.Data.get_top data stoch_k in
    let upper_band = Bars.Data.get_top data upper_bb in
    let ema_10_val = Bars.Data.get_top data ema_10 in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
    let entry_price = buying_order.price in

    (* Get historical MACD histogram for momentum analysis *)
    let macd_hist_1 =
      Bars.Data.get data macd_histogram (State.tick state - 1)
    in
    let macd_hist_2 =
      Bars.Data.get data macd_histogram (State.tick state - 2)
    in

    (* Calculate holding period *)
    let holding_period = State.tick state - buying_order.tick in

    (* Calculate profit/loss percentage *)
    let profit_pct = (current_price -. entry_price) /. entry_price *. 100.0 in

    (* Market regime detection - Testing ADX threshold of 30 *)
    let is_trending = adx_val >. 30.0 in

    (* Trending market exit conditions *)
    let trending_exit =
      if is_trending then
        let macd_weakening =
          macd_hist <. macd_hist_1 && macd_hist_1 <. macd_hist_2
        in
        let sar_bearish = sar_val >. current_price in
        macd_weakening || sar_bearish
      else false
    in

    (* Ranging market exit conditions *)
    let ranging_exit =
      if not is_trending then
        let stoch_overbought = stoch_k_val >. 80.0 in
        let near_upper_bb = current_price >=. upper_band *. 0.98 in
        stoch_overbought || near_upper_bb
      else false
    in

    (* Universal exit conditions - Testing tighter targets *)
    let profit_target = profit_pct >. 8.0 in
    let stop_loss = profit_pct <. -4.0 in
    let max_holding = holding_period > 20 in
    let trend_reversal = current_price <. ema_10_val in

    let should_sell =
      trending_exit || ranging_exit || profit_target || stop_loss || max_holding
      || trend_reversal
    in

    let reason =
      if should_sell then
        let regime = if is_trending then "TRENDING" else "RANGING" in
        let primary_reason =
          if trending_exit then
            if macd_hist <. macd_hist_1 && macd_hist_1 <. macd_hist_2 then
              Printf.sprintf "MACD momentum weakening: %.4f < %.4f < %.4f"
                macd_hist macd_hist_1 macd_hist_2
            else Printf.sprintf "SAR bearish: %.2f > %.2f" sar_val current_price
          else if ranging_exit then
            if stoch_k_val >. 80.0 then
              Printf.sprintf "Stochastic overbought: %.2f" stoch_k_val
            else
              Printf.sprintf "Near upper BB: %.2f >= %.2f" current_price
                (upper_band *. 0.98)
          else if profit_target then
            Printf.sprintf "Profit target: %.2f%% > 8%%" profit_pct
          else if stop_loss then
            Printf.sprintf "Stop loss: %.2f%% < -4%%" profit_pct
          else if max_holding then
            Printf.sprintf "Max holding: %d days > 20" holding_period
          else
            Printf.sprintf "Trend reversal: %.2f < %.2f (EMA10)" current_price
              ema_10_val
        in
        [
          Printf.sprintf "%s regime exit" regime;
          primary_reason;
          Printf.sprintf "Entry: %.2f, Current: %.2f, Holding: %d days"
            entry_price current_price holding_period;
        ]
      else []
    in

    (* Log sell signals *)
    (if should_sell then
       let regime = if is_trending then "TRENDING" else "RANGING" in
       let sell_reason =
         if trending_exit then "MOMENTUM_WEAKENING"
         else if ranging_exit then "MEAN_REVERSION_EXIT"
         else if profit_target then "PROFIT_TARGET"
         else if stop_loss then "STOP_LOSS"
         else if max_holding then "MAX_HOLDING"
         else "TREND_REVERSAL"
       in
       Eio.traceln
         "SELL SIGNAL: %s - %s - %s regime - ADX: %.2f, Price: %.2f, P&L: \
          %.2f%%, Hold: %dd"
         (Instrument.symbol buying_order.symbol)
         sell_reason regime adx_val current_price profit_pct holding_period);

    Result.return
      { Signal.instrument = buying_order.symbol; flag = should_sell; reason }
end

module Buy_trigger = Template.Buy_trigger.Make (Buy_trigger_input)
module Make : Strategy.BUILDER = Template.Make (Buy_trigger) (Sell_trigger_impl)
