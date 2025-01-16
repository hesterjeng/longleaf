module Order = Trading_types.Order
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
            "title" = `String "X-axis";
            "type" = `String "category";
            "tickmode" = `String "linear";
            "dtick" = `Int 20;
            "showticklabels" = `Bool false;
          ];
      "yaxis" = `Assoc [ "title" = `String "Y-axis" ];
    ]

let indicator_trace ?(show = true) ~data (indicators : Indicators.t)
    indicator_name indicator_get symbol : Yojson.Safe.t option =
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
    |> List.mapi (fun i b -> if i <= 34 then `Null else b)
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
  let* ema_trace =
    indicator_trace ~data indicators "Exponential Moving Average" IP.ema symbol
  in
  let* sma_5_trace = indicator_trace ~data indicators "SMA 5" IP.sma_5 symbol in
  let* sma_34_trace =
    indicator_trace ~data indicators "SMA 34" IP.sma_34 symbol
  in
  let* upper_bollinger =
    indicator_trace ~data indicators "Upper Bollinger" IP.upper_bollinger symbol
  in
  let* lower_bollinger =
    indicator_trace ~data indicators "Lower Bollinger" IP.lower_bollinger symbol
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
               ema_trace;
               sma_5_trace;
               sma_34_trace;
               upper_bollinger;
               lower_bollinger;
               awesome;
               rsi;
               fso_pk;
               fso_pd;
               fft_thing;
               fft_mse;
             ];
         "layout" = layout symbol;
       ]

let of_stats (stats : Stats.t) : Yojson.Safe.t =
  let open Stats in
  let ( = ) = fun x y -> (x, y) in
  let value_x_times = List.map (fun x -> x.time) stats in
  let value_trace : Yojson.Safe.t =
    List.map
      (fun x ->
        let res = Ptime.to_rfc3339 x.time in
        let value = x.value in
        (`String res, `Float value))
      stats
    |> List.split
    |> fun (x, y) ->
    `Assoc
      [
        "x" = `List x;
        "y" = `List y;
        "text" = `String "Statistics";
        "name" = `String "Statistics";
        "type" = `String "scatter";
      ]
  in
  let order_trace side =
    let get_side x =
      match side with
      | Trading_types.Side.Sell -> x.sell_order
      | Buy -> x.buy_order
    in
    let name = Trading_types.Side.to_string side in
    let color = Trading_types.Side.to_color side in
    let orders =
      List.filter_map
        (fun x -> match get_side x with Some _ -> Some x | None -> None)
        stats
    in
    (* Eio.traceln "@[Found %d %a orders.@]@." (List.length orders) *)
    (* Trading_types.Side.pp side; *)
    let x =
      List.map
        (fun x ->
          let closest = Time.find_closest x.time value_x_times in
          assert (List.mem closest value_x_times);
          `String (Ptime.to_rfc3339 closest))
        orders
    in
    let y = List.map (fun x -> `Float x.value) orders in
    let hovertext =
      List.map
        (fun x ->
          match get_side x with
          | Some b ->
              `String
                (Format.asprintf "%s<br>%s" b.symbol
                   (String.concat "<br>" b.reason))
          | None -> invalid_arg "Expected order here...")
        orders
    in
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
  in
  let buy_trace : Yojson.Safe.t = order_trace Buy in
  let sell_trace : Yojson.Safe.t = order_trace Sell in
  `Assoc
    [
      "traces" = `List [ value_trace; buy_trace; sell_trace ];
      "layout" = layout "Statistics";
    ]
