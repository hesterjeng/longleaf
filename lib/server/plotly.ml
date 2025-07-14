(** Plotly.js JSON generation for trading data visualization *)

module Data = Bars.Data

(** {1 Layout Configuration} *)

let layout title =
  let ( = ) = fun x y -> (x, y) in
  `Assoc
    [
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
      "yaxis" = `Assoc [ "title" = `String "Price" ];
      "yaxis2"
      = `Assoc
          [
            "title" = `String "Oscillators";
            "overlaying" = `String "y";
            "side" = `String "right";
            "range" = `List [ `Int (-20); `Int 120 ];
            "showgrid" = `Bool false;
            "zeroline" = `Bool false;
          ];
    ]

(** {1 Core Trace Functions} *)

let direct_price_trace ?(start = 0) ?end_ (data : Data.t)
    (symbol : Instrument.t) : Yojson.Safe.t =
  let length = Data.length data in
  let end_idx = Option.value end_ ~default:length in
  let end_idx = Int.min end_idx length in
  let symbol_str = Instrument.symbol symbol in

  let rec build_lists i acc_x acc_y =
    if i >= end_idx then (List.rev acc_x, List.rev acc_y)
    else
      let timestamp =
        Data.get data Time i |> Ptime.of_float_s
        |> Option.map Ptime.to_rfc3339
        |> Option.get_exn_or "Invalid timestamp in direct_price_trace"
      in
      let price = Data.get data Last i in
      build_lists (i + 1) (`String timestamp :: acc_x) (`Float price :: acc_y)
  in
  let x, y = build_lists start [] [] in

  `Assoc
    [
      ("x", `List x);
      ("y", `List y);
      ("text", `String symbol_str);
      ("name", `String symbol_str);
      ("type", `String "scatter");
    ]

let indicator_trace ?(show = false) ?(drop = 34) ?(yaxis = "y1")
    ?(color = "#1f77b4") ?(dash = "solid") ?(width = 1) ?(start = 0) ?end_ bars
    (indicator : Data.Type.t) (symbol : Instrument.t) =
  let ( let* ) = Result.( let* ) in
  let* data = Bars.get bars symbol in
  let indicator_name = Data.Type.show indicator in
  let length = Data.length data in
  let end_idx = Option.value end_ ~default:length in
  let end_idx = Int.min end_idx length in
  let effective_start = Int.max start drop in

  if effective_start >= end_idx then
    Result.return
    @@ `Assoc
         [
           ("x", `List []);
           ("y", `List []);
           ("text", `String indicator_name);
           ("name", `String indicator_name);
           ("yaxis", `String yaxis);
           ("type", `String "scatter");
           ("visible", if show then `Bool true else `String "legendonly");
         ]
  else
    let rec build_lists i acc_x acc_y =
      if i >= end_idx then (List.rev acc_x, List.rev acc_y)
      else
        let timestamp =
          Data.get data Time i |> Ptime.of_float_s
          |> Option.map Ptime.to_rfc3339
          |> Option.get_exn_or "Illegal time stored in data table (plotly.ml)"
        in
        let value = Data.get data indicator i in
        build_lists (i + 1) (`String timestamp :: acc_x) (`Float value :: acc_y)
    in
    let x, y = build_lists effective_start [] [] in

    let visible = if show then `Bool true else `String "legendonly" in
    Result.return
    @@ `Assoc
         [
           ("x", `List x);
           ("y", `List y);
           ("text", `String indicator_name);
           ("name", `String indicator_name);
           ("yaxis", `String yaxis);
           ("type", `String "scatter");
           ("visible", visible);
           ( "line",
             `Assoc
               [
                 ("color", `String color);
                 ("dash", `String dash);
                 ("width", `Int width);
               ] );
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

(** {1 Main API} *)

let of_bars ?(start = 100) ?end_ (bars : Bars.t) symbol : Yojson.Safe.t option =
  let ( let* ) = Result.( let* ) in
  let result =
    let* data = Bars.get bars symbol in
    let price_trace = direct_price_trace ~start ?end_ data symbol in

    (* Create the 10 most common technical indicators *)

    (* Price overlay indicators (main y-axis) *)
    let* sma_20 =
      indicator_trace ~show:true ~drop:20 ~color:"#ff7f0e" ~width:2 ~start ?end_
        bars (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.Sma))
        symbol
    in
    let* ema_20 =
      indicator_trace ~show:true ~drop:20 ~color:"#2ca02c" ~width:2 ~start ?end_
        bars (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.Ema))
        symbol
    in
    let* bb_upper =
      indicator_trace ~show:false ~drop:20 ~color:"#d62728" ~dash:"dot" ~start
        ?end_ bars
        (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.UpperBBand))
        symbol
    in
    let* bb_lower =
      indicator_trace ~show:false ~drop:20 ~color:"#d62728" ~dash:"dot" ~start
        ?end_ bars
        (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.LowerBBand))
        symbol
    in

    (* Oscillators (secondary y-axis) *)
    let* rsi_14 =
      indicator_trace ~show:false ~drop:14 ~yaxis:"y2" ~color:"#9467bd" ~width:2
        ~start ?end_ bars
        (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.Rsi))
        symbol
    in
    let* stoch_k =
      indicator_trace ~show:false ~drop:14 ~yaxis:"y2" ~color:"#8c564b"
        ~dash:"dash" ~start ?end_ bars
        (Data.Type.Tacaml
           (Tacaml.Indicator.F Tacaml.Indicator.Float.Stoch_SlowK)) symbol
    in
    let* cci_14 =
      indicator_trace ~show:false ~drop:14 ~yaxis:"y2" ~color:"#e377c2"
        ~dash:"dashdot" ~start ?end_ bars
        (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.Cci))
        symbol
    in

    (* MACD indicators (main y-axis but separate) *)
    let* macd =
      indicator_trace ~show:false ~drop:26 ~color:"#17becf" ~width:2 ~start
        ?end_ bars
        (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.Macd_MACD))
        symbol
    in
    let* macd_signal =
      indicator_trace ~show:false ~drop:35 ~color:"#bcbd22" ~dash:"dash" ~start
        ?end_ bars
        (Data.Type.Tacaml
           (Tacaml.Indicator.F Tacaml.Indicator.Float.Macd_MACDSignal)) symbol
    in

    (* Volatility indicator *)
    let* atr_14 =
      indicator_trace ~show:false ~drop:14 ~color:"#ff9896" ~width:2 ~start
        ?end_ bars
        (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.Atr))
        symbol
    in

    let ( = ) = fun x y -> (x, y) in
    Result.return
    @@ `Assoc
         [
           "traces"
           = `List
               [
                 price_trace;
                 sma_20;
                 ema_20;
                 rsi_14;
                 macd;
                 macd_signal;
                 bb_upper;
                 bb_lower;
                 stoch_k;
                 atr_14;
                 cci_14;
               ];
           "layout" = layout @@ Instrument.symbol symbol;
         ]
  in
  match result with
  | Ok json -> Some json
  | Error e ->
    Eio.traceln "%a" Error.pp e;
    None

(** {1 Statistics Module} *)
(* TODO: Reimplement stats visualization using Trading_state *)

(* module Stats = struct
   This module has been removed as Stats.t is no longer available.
   Future implementation should use Trading_state for portfolio visualization.
end *)

(** {1 Legacy Functions} *)

module Legacy = struct
  (** Legacy function that uses Item.t list - use direct_price_trace for better
      performance *)
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
