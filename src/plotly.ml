module Order = Trading_types.Order
open Option.Infix

let of_bars (x : Bars.t) (symbol : string) : Yojson.Safe.t option =
  let+ data_vec = Bars.get x symbol in
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
