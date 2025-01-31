module IP = Indicators.Point
open Option.Infix

let layout title =
  let ( = ) = fun x y -> (x, y) in
  `Assoc
    [
      "width" = `Int 1400;
      "height" = `Int 900;
      "title" = `String title;
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
      "yaxis2"
      = `Assoc
          [
            "title" = `String "Value2";
            "overlaying" = `String "y";
            "side" = `String "right";
          ];
    ]

let indicator_trace ?(show = true) ?(drop = 34) ?(yaxis = "y1") ~data
    (indicators : Indicators.t) indicator_name indicator_get symbol :
    Yojson.Safe.t option =
  let+ indicators_vec =
    match Indicators.get indicators symbol with
    | Some indicators -> Some indicators
    | None ->
        Eio.traceln "Could not get indicators for %s from mutex?" symbol;
        None
  in
  let x =
    let mk_plotly_x x =
      let time = Item.timestamp x in
      let res = Ptime.to_rfc3339 time in
      `String res
    in
    List.map mk_plotly_x data
  in
  (* let x = *)
  (*   Vector.map *)
  (*     (fun (p : Indicators.Point.t) -> *)
  (*       let time = p.timestamp in *)
  (*       let res = Ptime.to_rfc3339 time in *)
  (*       `String res) *)
  (*     indicators_vec *)
  (*   |> Vector.to_list |> List.drop 1 *)
  (* in *)
  let y =
    Vector.map
      (fun (p : Indicators.Point.t) ->
        `Float
          (let res = indicator_get p in
           if Float.is_nan res then
             Eio.traceln "ERROR: NaN in data for indicator %s!" indicator_name;
           res))
      indicators_vec
    |> Vector.to_list
    |> List.mapi (fun i b -> if i <= drop then `Null else b)
  in
  if List.length x <> List.length y then
    Eio.traceln "ERROR: Indicator length mismatch! x:%d y:%d" (List.length x)
      (List.length y);
  let visible = if show then "true" else "legendonly" in
  (* if String.equal "SMA 5" indicator_name && String.equal symbol "NVDA" then *)
  (*   Eio.traceln "@[%a@]@." (List.pp ~pp_sep:Format.newline Yojson.Safe.pp) y; *)
  `Assoc
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

let price_trace (data : Item.t list) (symbol : string) : Yojson.Safe.t =
  let x =
    let mk_plotly_x x =
      let time = Item.timestamp x in
      let res = Ptime.to_rfc3339 time in
      `String res
    in
    List.map mk_plotly_x data
  in
  let y = List.map (fun x -> `Float (Item.last x)) data in
  `Assoc
    [
      ("x", `List x);
      ("y", `List y);
      ("text", `String symbol);
      ("name", `String symbol);
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
          (Format.asprintf "%s<br>%s" symbol (String.concat "<br>" x.reason)))
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
  let find_side (x : Order.t) =
    let order_side = x.side in
    if Trading_types.Side.equal side order_side then Some x else None
  in
  let orders =
    List.filter_map
      (fun (item : Item.t) ->
        let* order = Item.order item in
        find_side order)
      data
  in
  order_trace side orders

let of_bars bars indicators symbol : Yojson.Safe.t option =
  let* data_vec = Bars.get bars symbol in
  let data = Vector.to_list data_vec in
  let* ema_12_trace =
    indicator_trace ~data ~drop:12 indicators "EMA(12)" IP.ema_12 symbol
  in
  let* ema_26_trace =
    indicator_trace ~data ~drop:26 indicators "EMA(26)" IP.ema_26 symbol
  in
  let* macd_trace =
    indicator_trace ~show:false ~drop:26 ~yaxis:"y2" ~data indicators "MACD"
      IP.macd symbol
  in
  let* sma_5_trace =
    indicator_trace ~drop:5 ~data indicators "SMA(5)" IP.sma_5 symbol
  in
  let* sma_34_trace =
    indicator_trace ~data ~drop:34 indicators "SMA(34)" IP.sma_34 symbol
  in
  let* sma_233_trace =
    indicator_trace ~data ~drop:233 ~show:false indicators "SMA(233)" IP.sma_233
      symbol
  in
  let* awesome_slow =
    indicator_trace ~data ~drop:233 ~show:false indicators "Awesome Slow"
      IP.awesome_slow symbol
  in
  let* upper_bollinger =
    indicator_trace ~data indicators "Upper BB(34)" IP.upper_bollinger symbol
  in
  let* lower_bollinger =
    indicator_trace ~data indicators "Lower BB(34)" IP.lower_bollinger symbol
  in
  let* rsi =
    indicator_trace ~data ~show:false indicators "Relative Strength Index"
      IP.rsi symbol
  in
  let* awesome =
    indicator_trace ~data ~show:false indicators "Awesome Oscillator" IP.awesome
      symbol
  in
  let* fso_pk =
    indicator_trace ~data ~show:false indicators "FSO %K" IP.fso_pk symbol
  in
  let* fso_pd =
    indicator_trace ~data ~show:false indicators "FSO %D" IP.fso_pd symbol
  in
  let* fft_thing =
    indicator_trace ~data ~show:false indicators
      "Fourier Transform Normalized Magnitude" IP.ft_normalized_magnitude symbol
  in
  let* fft_mse =
    indicator_trace ~data ~show:false indicators
      "Fourier Transform Mean Squared Error" IP.fft_mean_squared_error symbol
  in
  let* u3b =
    indicator_trace ~data ~show:false indicators "Upper 3 Bollinger"
      IP.upper_bollinger_100_3 symbol
  in
  let* l1b =
    indicator_trace ~data ~show:false indicators "Lower 1 Bollinger"
      IP.lower_bollinger_100_1 symbol
  in
  let buy_trace = order_trace_side Buy data in
  let sell_trace = order_trace_side Sell data in
  let price_trace = price_trace data symbol in
  let ( = ) = fun x y -> (x, y) in
  Option.return
  @@ `Assoc
       [
         "traces"
         = `List
             [
               price_trace;
               buy_trace;
               sell_trace;
               ema_12_trace;
               ema_26_trace;
               macd_trace;
               sma_5_trace;
               sma_34_trace;
               sma_233_trace;
               upper_bollinger;
               lower_bollinger;
               awesome;
               awesome_slow;
               rsi;
               fso_pk;
               fso_pd;
               fft_thing;
               fft_mse;
               u3b;
               l1b;
             ];
         "layout" = layout symbol;
       ]

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
        Format.asprintf "%s<br>%s" x.symbol (String.concat "<br>" x.reason)
      in
      match x.side with Buy -> `Left hovertext | Sell -> `Right hovertext
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
    let l = List.map of_item stats in
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
