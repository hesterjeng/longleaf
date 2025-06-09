(* module IP = Indicators.Point *)
open Option.Infix

let layout title =
  let ( = ) = fun x y -> (x, y) in
  `Assoc
    [
      "width" = `Int 1400;
      "height" = `Int 900;
      "title" = `String title;
      "hovermode" = `String "x";
      "xaxis"
      = `Assoc
          [
            "title" = `String "Time";
            "type" = `String "category";
            "tickmode" = `String "linear";
            "dtick" = `Int 20;
            "showticklabels" = `Bool false;
          ];
      "yaxis" = `Assoc [ "title" = `String "Value" ];
      (* "yaxis2" *)
      (* = `Assoc *)
      (*     [ *)
      (*       "title" = `String "Value2"; *)
      (*       "overlaying" = `String "y"; *)
      (*       "side" = `String "right"; *)
      (*     ]; *)
    ]

let indicator_trace ?(show = false) ?(drop = 34) ?(yaxis = "y1") indicators_tbl
    indicator_name indicator_get (symbol : Instrument.t) : Yojson.Safe.t option
    =
  (* let* indicators = Option.return @@ Indicators.of_timestampedtbl () *)
  (* in *)
  let ( let* ) = Option.( let* ) in
  let* indicators_vec = Hashtbl.get indicators_tbl symbol in
  assert (not @@ Vector.is_empty indicators_vec);
  let time (p : Indicators.Point.t) : Yojson.Safe.t =
    let timestamp = Ptime.to_rfc3339 p.timestamp in
    `String timestamp
  in
  let value (p : Indicators.Point.t) : Yojson.Safe.t =
    `Float
      (let res = indicator_get p in
       if Float.is_nan res then
         Eio.traceln "ERROR: NaN in data for indicator %s!" indicator_name;
       res)
  in
  let x, y =
    Vector.map
      (fun (p : Indicators.Point.t) -> Pair.make (time p) (value p))
      indicators_vec
    |> Vector.to_list |> List.drop drop |> List.split
  in
  if List.length x <> List.length y then (
    Eio.traceln "ERROR: Indicator length mismatch! x:%d y:%d" (List.length x)
      (List.length y);
    ());
  let visible = if show then "true" else "legendonly" in
  (* if String.equal "SMA 5" indicator_name && String.equal symbol "NVDA" then *)
  (*   Eio.traceln "@[%a@]@." (List.pp ~pp_sep:Format.newline Yojson.Safe.pp) y; *)
  Option.return
  @@ `Assoc
       [
         ("x", `List x);
         ("y", `List y);
         ("text", `String indicator_name);
         ("name", `String indicator_name);
         ("yaxis", `String yaxis);
         ("type", `String "scatter");
         ("visible", `String visible);
         ( "line",
           `Assoc
             [
               (* ("color", `String "red"); *)
               ("dash", `String "dash");
               ("width", `Int 2);
             ] );
       ]

let price_trace (data : Item.t list) (symbol : Instrument.t) : Yojson.Safe.t =
  let x =
    let mk_plotly_x x =
      let time = Item.timestamp x in
      let res = Ptime.to_rfc3339 time in
      `String res
    in
    List.map mk_plotly_x data
  in
  let symbol_str = Instrument.symbol symbol in
  let y = List.map (fun x -> `Float (Item.last x)) data in
  `Assoc
    [
      ("x", `List x);
      ("y", `List y);
      ("text", `String symbol_str);
      ("name", `String symbol_str);
      ("type", `String "scatter");
    ]

let order_trace (side : Trading_types.Side.t) (orders : Order.t list) :
    Yojson.Safe.t =
  let x =
    List.map
      (fun (x : Order.t) ->
        x.timestamp |> Ptime.to_rfc3339 |> fun t -> `String t)
      orders
  in
  let y = List.map (fun (x : Order.t) -> `Float x.price) orders in
  let hovertext =
    List.map
      (fun (x : Order.t) ->
        let symbol = x.symbol in
        `String
          (Format.asprintf "%s<br>%s" (Instrument.symbol symbol)
             (String.concat "<br>" x.reason)))
      orders
  in
  let color = Trading_types.Side.to_color side in
  `Assoc
    [
      ("x", `List x);
      ("y", `List y);
      ("hovertext", `List hovertext);
      ("hoverinfo", `String "text");
      ("type", `String "scatter");
      ("mode", `String "markers");
      ("marker", `Assoc [ ("color", `String color); ("size", `Int 10) ]);
      ("name", `String (Trading_types.Side.to_string side));
    ]

let order_trace_side (side : Trading_types.Side.t) (data : Item.t list) =
  invalid_arg
    "plotly.ml: order_trace_side: FIXME items no longer have order field"
(* let find_side (x : Order.t) = *)
(*   let order_side = x.side in *)
(*   if Trading_types.Side.equal side order_side then Some x else None *)
(* in *)
(* let orders = *)
(*   List.filter_map *)
(*     (fun (item : Item.t) -> *)
(*       let* order = Item.order item in *)
(*       find_side order) *)
(*     data *)
(* in *)
(* order_trace side orders *)

let of_bars (bars : Bars.t) indicators symbol : Yojson.Safe.t option =
  Eio.traceln "Plots.of_bars NYI";
  None
(* let* data_vec = Bars.get bars symbol in *)
(* let data = Vector.to_list data_vec in *)
(* let* ema_12_trace = *)
(*   indicator_trace ~drop:12 indicators "EMA(12)" IP.ema_12 symbol *)
(* in *)
(* let* ema_26_trace = *)
(*   indicator_trace ~drop:26 indicators "EMA(26)" IP.ema_26 symbol *)
(* in *)
(* let* macd_trace = *)
(*   indicator_trace ~show:false ~drop:26 indicators "MACD" IP.macd symbol *)
(* in *)
(* let* sma_5_trace = *)
(*   indicator_trace ~drop:5 indicators "SMA(5)" IP.sma_5 symbol *)
(* in *)
(* let* sma_34_trace = *)
(*   indicator_trace ~drop:34 indicators "SMA(34)" IP.sma_34 symbol *)
(* in *)
(* let* sma_75_trace = *)
(*   indicator_trace ~drop:34 indicators "SMA(75)" IP.sma_75 symbol *)
(* in *)
(* let* sma_233_trace = *)
(*   indicator_trace ~drop:233 ~show:false indicators "SMA(233)" IP.sma_233 *)
(*     symbol *)
(* in *)
(* let* awesome_slow = *)
(*   indicator_trace ~drop:233 ~show:false indicators "Awesome Slow" *)
(*     IP.awesome_slow symbol *)
(* in *)
(* let* upper_bollinger = *)
(*   indicator_trace indicators "Upper BB(34)" IP.upper_bollinger symbol *)
(* in *)
(* let* lower_bollinger = *)
(*   indicator_trace indicators "Lower BB(34)" IP.lower_bollinger symbol *)
(* in *)
(* let* rsi = *)
(*   indicator_trace ~show:false indicators "Relative Strength Index" *)
(*     IP.relative_strength_index symbol *)
(* in *)
(* let* adx = indicator_trace ~show:false indicators "ADX" IP.adx symbol in *)
(* let* cci = indicator_trace ~show:false indicators "CCI" IP.cci symbol in *)
(* let* ema_cci = *)
(*   indicator_trace ~show:true indicators "EMA CCI" IP.ema_cci symbol *)
(* in *)
(* let* ema_adx = *)
(*   indicator_trace ~show:true indicators "EMA ADX" IP.ema_adx symbol *)
(* in *)
(* let* awesome = *)
(*   indicator_trace ~show:false indicators "Awesome Oscillator" *)
(*     IP.awesome_oscillator symbol *)
(* in *)
(* let* fso_pk = *)
(*   indicator_trace ~show:false indicators "FSO %K" IP.fso_k symbol *)
(* in *)
(* let* fso_pd = *)
(*   indicator_trace ~show:false indicators "FSO %D" IP.fso_d symbol *)
(* in *)
(* let* fso_pd_slow = *)
(*   indicator_trace ~show:false indicators "FSO %D-slow" IP.fso_d_slow symbol *)
(* in *)
(* let* fft_thing = *)
(*   indicator_trace ~show:false indicators *)
(*     "Fourier Transform Normalized Magnitude" IP.ft_normalized_magnitude symbol *)
(* in *)
(* let* fft_mse = *)
(*   indicator_trace ~show:false indicators *)
(*     "Fourier Transform Mean Squared Error" IP.fft_mean_squared_error symbol *)
(* in *)
(* let* u3b = *)
(*   indicator_trace ~show:false indicators "Upper 3 Bollinger" *)
(*     IP.upper_bollinger_100_3 symbol *)
(* in *)
(* let* l1b = *)
(*   indicator_trace ~show:false indicators "Lower 1 Bollinger" *)
(*     IP.lower_bollinger_100_1 symbol *)
(* in *)
(* let buy_trace = order_trace_side Buy data in *)
(* let sell_trace = order_trace_side Sell data in *)
(* let price_trace = price_trace data symbol in *)
(* let ( = ) = fun x y -> (x, y) in *)
(* Option.return *)
(* @@ `Assoc *)
(*      [ *)
(*        "traces" *)
(*        = `List *)
(*            [ *)
(*              price_trace; *)
(*              buy_trace; *)
(*              sell_trace; *)
(*              adx; *)
(*              cci; *)
(*              ema_cci; *)
(*              ema_adx; *)
(*              ema_12_trace; *)
(*              ema_26_trace; *)
(*              macd_trace; *)
(*              sma_5_trace; *)
(*              sma_34_trace; *)
(*              sma_75_trace; *)
(*              sma_233_trace; *)
(*              upper_bollinger; *)
(*              lower_bollinger; *)
(*              awesome; *)
(*              awesome_slow; *)
(*              rsi; *)
(*              fso_pk; *)
(*              fso_pd; *)
(*              fso_pd_slow; *)
(*              fft_thing; *)
(*              fft_mse; *)
(*              u3b; *)
(*              l1b; *)
(*            ]; *)
(*        "layout" = layout @@ Instrument.symbol symbol; *)
(*      ] *)

module Stats = struct
  module Side = Trading_types.Side

  type t = {
    x : string;
    y : float;
    cash : float;
    buy_hovertext : string option;
    sell_hovertext : string option;
  }
  [@@deriving show, yojson]

  let of_item (item : Stats.item) =
    let filter (x : Order.t) =
      let hovertext =
        Format.asprintf "* %s<br>%s"
          (Instrument.symbol x.symbol)
          (String.concat "<br>" x.reason)
      in
      match x.side with
      | Buy -> `Left hovertext
      | Sell -> `Right hovertext
    in
    let pair = List.partition_filter_map filter item.orders in
    let buy_hovertext, sell_hovertext =
      Pair.map_same
        (fun ht ->
          match ht with
          | [] -> None
          | l -> Option.return @@ String.concat "<br>" l)
        pair
    in
    {
      x = Ptime.to_rfc3339 item.time;
      y = item.value;
      cash = item.cash;
      buy_hovertext;
      sell_hovertext;
    }

  let order_trace (side : Side.t) (items : t list) =
    let ( = ) = fun x y -> (x, y) in
    let filtered =
      List.filter
        (fun (x : t) ->
          match side with
          | Buy -> Option.is_some x.buy_hovertext
          | Sell -> Option.is_some x.sell_hovertext)
        items
    in
    let x = List.map (fun (x : t) -> `String x.x) filtered in
    let y = List.map (fun (x : t) -> `Float x.y) filtered in
    let hovertext =
      List.map
        (fun (x : t) ->
          `String
            (Option.get_exn_or
               "plotly.ml: impossible, already filtered for side"
               (match side with
               | Buy -> x.buy_hovertext
               | Sell -> x.sell_hovertext)))
        filtered
    in
    let name = Trading_types.Side.to_string side in
    let color = Trading_types.Side.to_color side in
    `Assoc
      [
        "x" = `List x;
        "y" = `List y;
        "hovertext" = `List hovertext;
        "hoverinfo" = `String "text";
        "mode" = `String "markers";
        "type" = `String "scatter";
        "name" = `String name;
        "marker" = `Assoc [ "color" = `String color; "size" = `Int 10 ];
      ]

  let make (stats : Stats.t) : Yojson.Safe.t =
    let ( = ) = fun x y -> (x, y) in
    let l = List.map of_item stats.history in
    let x = List.map (fun x -> `String x.x) l in
    let y = List.map (fun x -> `Float x.y) l in
    let cash = List.map (fun x -> `Float x.cash) l in
    let value_trace : Yojson.Safe.t =
      `Assoc
        [
          "x" = `List x;
          "y" = `List y;
          "text" = `String "Statistics";
          "name" = `String "Statistics";
          "type" = `String "scatter";
        ]
    in
    let cash_trace : Yojson.Safe.t =
      `Assoc
        [
          "x" = `List x;
          "y" = `List cash;
          "text" = `String "Cash";
          "name" = `String "Cash";
          "type" = `String "scatter";
          "visible" = `String "legendonly";
        ]
    in
    let buy_trace : Yojson.Safe.t = order_trace Buy l in
    let sell_trace : Yojson.Safe.t = order_trace Sell l in
    `Assoc
      [
        "traces" = `List [ value_trace; buy_trace; sell_trace; cash_trace ];
        "layout" = layout "Statistics";
      ]
end
