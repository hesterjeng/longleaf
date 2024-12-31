module Order = Trading_types.Order
open Option.Infix

let of_bars (x : Bars.t) (indicators : Indicators.t) (symbol : string) :
    Yojson.Safe.t option =
  let* data_vec = Bars.get x symbol in
  let+ indicators_vec =
    match Indicators.get indicators symbol with
    | Some indicators -> Some indicators
    | None ->
        Eio.traceln "Could not get indicators for %s from mutex?" symbol;
        None
  in
  let indicators_x =
    Vector.map
      (fun (p : Indicators.Point.t) ->
        let time = p.timestamp in
        let res = Ptime.to_rfc3339 time in
        `String res)
      indicators_vec
    |> Vector.to_list |> List.drop 1
  in
  let accumulation_distribution_line =
    Vector.map
      (fun (p : Indicators.Point.t) -> `Float p.accumulation_distribution_line)
      indicators_vec
    |> Vector.to_list |> List.drop 1
  in
  let exponential_moving_average_line =
    Vector.map
      (fun (p : Indicators.Point.t) -> `Float p.exponential_moving_average)
      indicators_vec
    |> Vector.to_list |> List.drop 1
  in
  Eio.traceln "@[%a@]" (Vector.pp Indicators.Point.pp) indicators_vec;
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
                ("adl", `List accumulation_distribution_line);
                ("ema", `List exponential_moving_average_line);
                ("indicators_x", `List indicators_x);
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
