(** Plotly.js JSON generation for trading data visualization *)

module Data = Bars.Data

(** {1 Layout Configuration} *)

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
    ]

(** {1 Core Trace Functions} *)

let direct_price_trace ?(start = 0) ?end_ (data : Data.t) (symbol : Instrument.t) : Yojson.Safe.t =
  let length = Data.length data in
  let end_idx = Option.value end_ ~default:length in
  let end_idx = Int.min end_idx length in
  let symbol_str = Instrument.symbol symbol in
  
  let x = Array.init (end_idx - start) (fun i ->
    let actual_i = start + i in
    let timestamp = Data.get data Time actual_i |> Ptime.of_float_s 
                   |> Option.map Ptime.to_rfc3339
                   |> Option.get_exn_or "Invalid timestamp in direct_price_trace" in
    `String timestamp
  ) |> Array.to_list in
  
  let y = Array.init (end_idx - start) (fun i ->
    let actual_i = start + i in
    `Float (Data.get data Last actual_i)
  ) |> Array.to_list in
  
  `Assoc
    [
      ("x", `List x);
      ("y", `List y);
      ("text", `String symbol_str);
      ("name", `String symbol_str);
      ("type", `String "scatter");
    ]

let indicator_trace ?(show = false) ?(drop = 34) ?(yaxis = "y1") ?(start = 0) ?end_ bars
    (indicator : Data.Type.t) (symbol : Instrument.t) =
  let ( let* ) = Result.( let* ) in
  let* data = Bars.get bars symbol in
  let indicator_name = Data.Type.show indicator in
  let length = Data.length data in
  let end_idx = Option.value end_ ~default:length in
  let end_idx = Int.min end_idx length in
  let effective_start = Int.max start drop in
  
  if effective_start >= end_idx then
    Result.return @@ `Assoc [
      ("x", `List []);
      ("y", `List []);
      ("text", `String indicator_name);
      ("name", `String indicator_name);
      ("yaxis", `String yaxis);
      ("type", `String "scatter");
      ("visible", `String (if show then "true" else "legendonly"));
    ]
  else
    let size = end_idx - effective_start in
    let x = Array.init size (fun i ->
      let actual_i = effective_start + i in
      let timestamp = Data.get data Time actual_i |> Ptime.of_float_s
                     |> Option.map Ptime.to_rfc3339
                     |> Option.get_exn_or "Illegal time stored in data table (plotly.ml)" in
      `String timestamp
    ) |> Array.to_list in
    
    let y = Array.init size (fun i ->
      let actual_i = effective_start + i in
      `Float (Data.get data indicator actual_i)
    ) |> Array.to_list in
    
    let visible = if show then "true" else "legendonly" in
    Result.return
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
                 ("dash", `String "dash");
                 ("width", `Int 2);
               ] );
         ]

let order_trace (side : Trading_types.Side.t) (orders : Order.t list) : Yojson.Safe.t =
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

(** {1 Main API} *)

let of_bars ?(start = 0) ?end_ (bars : Bars.t) symbol : Yojson.Safe.t option =
  let ( let* ) = Result.( let* ) in
  let result =
    let* data = Bars.get bars symbol in
    let price_trace = direct_price_trace ~start ?end_ data symbol in
    let ( = ) = fun x y -> (x, y) in
    Result.return
    @@ `Assoc
         [
           "traces" = `List [ price_trace ];
           "layout" = layout @@ Instrument.symbol symbol;
         ]
  in
  match result with
  | Ok json -> Some json
  | Error _ -> None

(** {1 Statistics Module} *)

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

(** {1 Legacy Functions} *)

module Legacy = struct
  (** Legacy function that uses Item.t list - use direct_price_trace for better performance *)
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
end