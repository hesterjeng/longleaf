(** Candlestick Pattern Strategy

    This strategy uses TA-Lib's candlestick pattern recognition to identify
    reversal and continuation signals.

    Buy Logic (Bullish Patterns):
    - Hammer, Inverted Hammer, Morning Star, Morning Doji Star
    - Piercing Line, Engulfing (bullish), Three White Soldiers
    - Doji patterns in downtrend, Dragonfly Doji
    - Must be near support (lower Bollinger Band) for reversal patterns
    - Volume confirmation required

    Sell Logic (Bearish Patterns):
    - Shooting Star, Hanging Man, Evening Star, Evening Doji Star
    - Dark Cloud Cover, Engulfing (bearish), Three Black Crows
    - Doji patterns in uptrend, Gravestone Doji
    - Must be near resistance (upper Bollinger Band) for reversal patterns

    Additional Filters:
    - RSI confirmation (oversold for buy, overbought for sell)
    - Volume above average for pattern confirmation
    - ADX for trend strength assessment **)

module Buy_trigger_input : Template.Buy_trigger.INPUT = struct
  (* Candlestick Patterns *)
  let hammer = Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_hammer ()))

  let inverted_hammer =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_invertedhammer ()))

  let morning_star =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_morningstar ()))

  let morning_doji_star =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_morningdojistar ()))

  let piercing = Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_piercing ()))
  let engulfing = Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_engulfing ()))

  let three_white_soldiers =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_3whitesoldiers ()))

  let doji = Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_doji ()))

  let dragonfly_doji =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_dragonflydoji ()))

  (* Traditional Indicators for Confirmation *)
  let rsi = Bars.Data.Type.(Tacaml (Tacaml.Indicator.rsi ()))
  let lower_bb = Bars.Data.Type.(Tacaml (Tacaml.Indicator.lower_bband ()))
  let upper_bb = Bars.Data.Type.(Tacaml (Tacaml.Indicator.upper_bband ()))
  let adx = Bars.Data.Type.(Tacaml (Tacaml.Indicator.adx ()))

  let pass (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in

    (* Skip early periods for pattern stability *)
    if State.tick state < 50 then
      Result.return { Signal.instrument; flag = false; reason = [] }
    else
      (* Get candlestick pattern signals *)
      let hammer_val = Bars.Data.get_top_int data hammer in
      let inv_hammer_val = Bars.Data.get_top_int data inverted_hammer in
      let morning_star_val = Bars.Data.get_top_int data morning_star in
      let morning_doji_val = Bars.Data.get_top_int data morning_doji_star in
      let piercing_val = Bars.Data.get_top_int data piercing in
      let engulfing_val = Bars.Data.get_top_int data engulfing in
      let three_white_val = Bars.Data.get_top_int data three_white_soldiers in
      let doji_val = Bars.Data.get_top_int data doji in
      let dragonfly_val = Bars.Data.get_top_int data dragonfly_doji in

      (* Traditional indicators *)
      let rsi_val = Bars.Data.get_top data rsi in
      let lower_band = Bars.Data.get_top data lower_bb in
      let upper_band = Bars.Data.get_top data upper_bb in
      let adx_val = Bars.Data.get_top data adx in
      let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
      let current_volume = Bars.Data.get_top data Bars.Data.Type.Volume in

      (* Calculate average volume *)
      let avg_volume =
        let volumes =
          List.init 20 (fun i ->
              Bars.Data.get data Bars.Data.Type.Volume (State.tick state - i - 1))
        in
        List.fold_left ( +. ) 0.0 volumes /. 20.0
      in

      (* Check for bullish candlestick patterns *)
      let bullish_patterns =
        [
          (hammer_val = 100, "Hammer");
          (inv_hammer_val = 100, "Inverted Hammer");
          (morning_star_val = 100, "Morning Star");
          (morning_doji_val = 100, "Morning Doji Star");
          (piercing_val = 100, "Piercing Line");
          (engulfing_val = 100, "Bullish Engulfing");
          (three_white_val = 100, "Three White Soldiers");
          (doji_val = 100, "Doji");
          (dragonfly_val = 100, "Dragonfly Doji");
        ]
      in

      let detected_patterns =
        List.filter_map
          (fun (detected, name) -> if detected then Some name else None)
          bullish_patterns
      in

      let has_bullish_pattern = List.length detected_patterns > 0 in

      (* Confirmation filters *)
      let rsi_oversold = rsi_val <. 40.0 in
      (* Slightly less strict than 30 *)
      let near_support =
        current_price <=. lower_band +. ((upper_band -. lower_band) *. 0.25)
      in
      let volume_confirmation = current_volume >. avg_volume *. 1.3 in
      let trend_ok = adx_val >. 20.0 in
      (* Some trend strength *)

      (* For reversal patterns, we want to be near support *)
      (* For continuation patterns (like Three White Soldiers), trend strength is key *)
      let reversal_patterns =
        [
          "Hammer";
          "Inverted Hammer";
          "Morning Star";
          "Morning Doji Star";
          "Piercing Line";
          "Doji";
          "Dragonfly Doji";
        ]
      in
      let continuation_patterns =
        [ "Bullish Engulfing"; "Three White Soldiers" ]
      in

      let is_reversal =
        List.exists (fun p -> List.mem p detected_patterns) reversal_patterns
      in
      let is_continuation =
        List.exists
          (fun p -> List.mem p detected_patterns)
          continuation_patterns
      in

      let conditions_met =
        has_bullish_pattern && rsi_oversold && volume_confirmation
        && ((is_reversal && near_support) || (is_continuation && trend_ok))
      in

      let reason =
        if conditions_met then
          [
            Printf.sprintf "Bullish patterns: %s"
              (String.concat ", " detected_patterns);
            Printf.sprintf "RSI oversold: %.2f < 40" rsi_val;
            Printf.sprintf "Volume: %.1fx average" (current_volume /. avg_volume);
            Printf.sprintf "Price: %.2f (Support: %.2f)" current_price
              lower_band;
            Printf.sprintf "ADX trend strength: %.2f" adx_val;
          ]
        else []
      in

      (* Log buy signals *)
      if conditions_met then
        Eio.traceln
          "BUY SIGNAL: %s - Patterns: [%s] - RSI: %.2f, Vol: %.1fx, Price: \
           %.2f, ADX: %.2f"
          (Instrument.symbol instrument)
          (String.concat ", " detected_patterns)
          rsi_val
          (current_volume /. avg_volume)
          current_price adx_val;

      Result.return { Signal.instrument; flag = conditions_met; reason }

  let score (state : _ State.t) instrument =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state instrument in

    if State.tick state < 50 then Result.return 0.0
    else
      (* Score based on pattern strength, RSI oversold level, and volume *)
      let rsi_val = Bars.Data.get_top data rsi in
      let current_volume = Bars.Data.get_top data Bars.Data.Type.Volume in
      let adx_val = Bars.Data.get_top data adx in

      (* Calculate average volume *)
      let avg_volume =
        let volumes =
          List.init 20 (fun i ->
              Bars.Data.get data Bars.Data.Type.Volume (State.tick state - i - 1))
        in
        List.fold_left ( +. ) 0.0 volumes /. 20.0
      in

      (* Count bullish patterns *)
      let pattern_count =
        let patterns =
          [
            Bars.Data.get_top_int data hammer;
            Bars.Data.get_top_int data inverted_hammer;
            Bars.Data.get_top_int data morning_star;
            Bars.Data.get_top_int data morning_doji_star;
            Bars.Data.get_top_int data piercing;
            Bars.Data.get_top_int data engulfing;
            Bars.Data.get_top_int data three_white_soldiers;
            Bars.Data.get_top_int data doji;
            Bars.Data.get_top_int data dragonfly_doji;
          ]
        in
        List.filter (fun x -> x = 100) patterns |> List.length |> Float.of_int
      in

      let rsi_score =
        if rsi_val <. 40.0 then (40.0 -. rsi_val) *. 2.0 else 0.0
      in
      let volume_score = current_volume /. avg_volume *. 10.0 in
      let pattern_score = pattern_count *. 20.0 in
      let trend_score = Float.min adx_val 40.0 in

      let total_score =
        pattern_score +. rsi_score +. volume_score +. trend_score
      in
      Result.return total_score

  let num_positions = 6 (* Hold maximum 6 positions *)
end

module Sell_trigger_impl : Template.Sell_trigger.S = struct
  (* Bearish Candlestick Patterns *)
  let shooting_star =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_shootingstar ()))

  let hanging_man = Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_hangingman ()))

  let evening_star =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_eveningstar ()))

  let evening_doji_star =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_eveningdojistar ()))

  let dark_cloud =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_darkcloudcover ()))

  let engulfing = Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_engulfing ()))

  let three_black_crows =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_3blackcrows ()))

  let doji = Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_doji ()))

  let gravestone_doji =
    Bars.Data.Type.(Tacaml (Tacaml.Indicator.cdl_gravestonedoji ()))

  (* Traditional indicators *)
  let rsi = Bars.Data.Type.(Tacaml (Tacaml.Indicator.rsi ()))
  let upper_bb = Bars.Data.Type.(Tacaml (Tacaml.Indicator.upper_bband ()))
  let lower_bb = Bars.Data.Type.(Tacaml (Tacaml.Indicator.lower_bband ()))

  let make (state : 'a State.t) (symbol : Instrument.t) =
    let ( let* ) = Result.( let* ) in
    let* data = State.data state symbol in

    (* Get bearish pattern signals *)
    let shooting_val = Bars.Data.get_top_int data shooting_star in
    let hanging_val = Bars.Data.get_top_int data hanging_man in
    let evening_star_val = Bars.Data.get_top_int data evening_star in
    let evening_doji_val = Bars.Data.get_top_int data evening_doji_star in
    let dark_cloud_val = Bars.Data.get_top_int data dark_cloud in
    let engulfing_val = Bars.Data.get_top_int data engulfing in
    let three_black_val = Bars.Data.get_top_int data three_black_crows in
    let doji_val = Bars.Data.get_top_int data doji in
    let gravestone_val = Bars.Data.get_top_int data gravestone_doji in

    (* Traditional indicators *)
    let rsi_val = Bars.Data.get_top data rsi in
    let upper_band = Bars.Data.get_top data upper_bb in
    let current_price = Bars.Data.get_top data Bars.Data.Type.Close in
    let qty_held = State.qty state symbol in
    let entry_price =
      if qty_held > 0 then
        -.State.cost_basis state symbol /. float_of_int qty_held
      else 0.0
    in

    (* Calculate holding period and profit *)
    let holding_period = 1 in
    (* placeholder - holding period needs order history *)
    let profit_pct = (current_price -. entry_price) /. entry_price *. 100.0 in

    (* Check for bearish candlestick patterns *)
    let bearish_patterns =
      [
        (shooting_val = -100, "Shooting Star");
        (hanging_val = -100, "Hanging Man");
        (evening_star_val = -100, "Evening Star");
        (evening_doji_val = -100, "Evening Doji Star");
        (dark_cloud_val = -100, "Dark Cloud Cover");
        (engulfing_val = -100, "Bearish Engulfing");
        (three_black_val = -100, "Three Black Crows");
        (doji_val = 100, "Doji in Uptrend");
        (* Doji in uptrend can be bearish *)
        (gravestone_val = -100, "Gravestone Doji");
      ]
    in

    let detected_patterns =
      List.filter_map
        (fun (detected, name) -> if detected then Some name else None)
        bearish_patterns
    in

    let has_bearish_pattern = List.length detected_patterns > 0 in

    (* Confirmation filters *)
    let rsi_overbought = rsi_val >. 60.0 in
    let near_resistance = current_price >=. upper_band *. 0.98 in

    (* Pattern-based exit *)
    let pattern_exit =
      has_bearish_pattern && (rsi_overbought || near_resistance)
    in

    (* Traditional exits *)
    let profit_target = profit_pct >. 10.0 in
    let stop_loss = profit_pct <. -5.0 in
    let max_holding = holding_period > 20 in

    let should_sell =
      pattern_exit || profit_target || stop_loss || max_holding
    in

    let reason =
      if should_sell then
        let primary_reason =
          if pattern_exit then
            Printf.sprintf "Bearish patterns: %s (RSI: %.2f)"
              (String.concat ", " detected_patterns)
              rsi_val
          else if profit_target then
            Printf.sprintf "Profit target: %.2f%% > 10%%" profit_pct
          else if stop_loss then
            Printf.sprintf "Stop loss: %.2f%% < -5%%" profit_pct
          else Printf.sprintf "Max holding: %d days > 20" holding_period
        in
        [
          primary_reason;
          Printf.sprintf "Entry: %.2f, Current: %.2f, Near resistance: %b"
            entry_price current_price near_resistance;
        ]
      else []
    in

    (* Log sell signals *)
    (if should_sell then
       let sell_reason =
         if pattern_exit then
           Printf.sprintf "BEARISH_PATTERNS_%s"
             (String.concat "_" detected_patterns)
         else if profit_target then "PROFIT_TARGET"
         else if stop_loss then "STOP_LOSS"
         else "MAX_HOLDING"
       in
       Eio.traceln
         "SELL SIGNAL: %s - %s - RSI: %.2f, Price: %.2f, P&L: %.2f%%, Hold: %dd"
         (Instrument.symbol symbol) sell_reason rsi_val current_price profit_pct
         holding_period);

    Result.return { Signal.instrument = symbol; flag = should_sell; reason }
end

module Buy_trigger = Template.Buy_trigger.Make (Buy_trigger_input)
module Make : Strategy.BUILDER = Template.Make (Buy_trigger) (Sell_trigger_impl)
