module Order = Trading_types.Order
open Option.Infix

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

let price_trace (bars : Bars.t) (symbol : string) =
  let+ data_vec = Bars.get bars symbol in
  let data = Vector.to_list data_vec in
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

let buy_trace (bars : Bars.t) (symbol : string) =
  let+ data_vec = Bars.get bars symbol in
  let data = Vector.to_list data_vec in
  let buy_orders =
    List.filter_map
      (fun (item : Item.t) ->
        let* order = Item.order item in
        match order.side with Sell -> None | Buy -> Some order)
      data
  in
  let x =
    List.map
      (fun (x : Order.t) ->
        x.timestamp |> Ptime.to_rfc3339 |> fun t -> `String t)
      buy_orders
  in
  let y = List.map (fun (x : Order.t) -> `Float x.price) in
  invalid_arg "Do reasons!"

let of_bars (x : Bars.t) (indicators : Indicators.t) (symbol : string) :
    Yojson.Safe.t option =
  let* data_vec = Bars.get x symbol in
  let+ ema_trace = ema_trace indicators symbol in
  let data = Vector.to_list data_vec in
  let x_axis =
    let mk_plotly_x x =
      let time = Item.timestamp x in
      let res = Ptime.to_rfc3339 time in
      `String res
    in
    List.map mk_plotly_x data
  in
  let buy_axis =
    List.map
      (fun (x : Item.t) ->
        match Item.order x with
        | None -> `Null
        | Some order -> (
            match order.side with Buy -> `String "buy" | Sell -> `Null))
      data
  in
  let reasons =
    List.map
      (fun (x : Item.t) ->
        Item.order x |> function
        | None -> `Null
        | Some order -> `List (List.map (fun x -> `String x) order.reason))
      data
  in
  let sell_axis =
    List.map
      (fun (x : Item.t) ->
        match Item.order x with
        | None -> `Null
        | Some order -> (
            match order.side with Buy -> `Null | Sell -> `String "sell"))
      data
  in
  let y_axis = List.map (fun x -> `Float (Item.last x)) data in
  `Assoc
    [
      ( "data",
        `List
          [
            `Assoc
              [
                ("x", `List x_axis);
                ("y", `List y_axis);
                ("buy", `List buy_axis);
                ("sell", `List sell_axis);
                ("reasons", `List reasons);
                ("ema", ema_trace);
                ("mode", `String "lines+markers");
                ("name", `String symbol);
              ];
          ] );
      ( "layout",
        `Assoc
          [
            ("title", `String "Sample Plotly Graph");
            ( "xaxis",
              `Assoc [ ("title", `String "X Axis"); ("type", `String "date") ]
            );
            ("yaxis", `Assoc [ ("title", `String "Y Axis") ]);
          ] );
    ]
