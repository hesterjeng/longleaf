module Order = Trading_types.Order
open Option.Infix

let layout title =
  let ( = ) = fun x y -> (x, y) in
  `Assoc
    [
      "width" = `Int 1000;
      "height" = `Int 700;
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

let ema_trace (indicators : Indicators.t) symbol : Yojson.Safe.t option =
  let+ indicators_vec =
    match Indicators.get indicators symbol with
    | Some indicators -> Some indicators
    | None ->
        Eio.traceln "Could not get indicators for %s from mutex?" symbol;
        None
  in
  let x =
    Vector.map
      (fun (p : Indicators.Point.t) ->
        let time = p.timestamp in
        let res = Ptime.to_rfc3339 time in
        `String res)
      indicators_vec
    |> Vector.to_list |> List.drop 1
  in
  let y =
    Vector.map
      (fun (p : Indicators.Point.t) -> `Float p.exponential_moving_average)
      indicators_vec
    |> Vector.to_list |> List.drop 1
  in
  `Assoc
    [
      ("x", `List x);
      ("y", `List y);
      ("text", `String "Exponential Moving Average");
      ("name", `String "Exponential Moving Average");
      ("type", `String "scatter");
      ( "line",
        `Assoc
          [
            ("color", `String "red"); ("dash", `String "dash"); ("width", `Int 2);
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
  let+ ema_trace = ema_trace indicators symbol in
  let buy_trace = order_trace_side Buy data in
  let sell_trace = order_trace_side Sell data in
  let price_trace = price_trace data symbol in
  let ( = ) = fun x y -> (x, y) in
  `Assoc
    [
      "traces" = `List [ price_trace; buy_trace; sell_trace; ema_trace ];
      "layout" = layout symbol;
    ]

let of_stats (stats : Stats.t) : Yojson.Safe.t =
  let open Stats in
  let ( = ) = fun x y -> (x, y) in
  let value_trace : Yojson.Safe.t =
    List.map
      (fun x ->
        let res = Ptime.to_rfc3339 x.time in
        let value = x.value in
        (`String res, `Float value))
      stats
    |> List.split
    |> fun (x, y) ->
    let ( = ) = fun x y -> (x, y) in
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
    let x = List.map (fun x -> `String (Ptime.to_rfc3339 x.time)) orders in
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
