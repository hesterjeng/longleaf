(** Plotly.js JSON generation for trading data visualization *)

module Bars = Longleaf_bars
module Data = Bars.Data

(** {1 Float Sanitization} *)

(** Sanitize float values for Plotly - convert NaN/Inf to null *)
let sanitize_float f =
  match classify_float f with
  | FP_normal
  | FP_subnormal
  | FP_zero ->
    `Float f
  | FP_infinite
  | FP_nan ->
    `Null

(** {1 Color Palette} *)

(* Plotly default color palette with good visual distinction *)
let color_palette =
  [|
    "#1f77b4";
    (* blue *)
    "#ff7f0e";
    (* orange *)
    "#2ca02c";
    (* green *)
    "#d62728";
    (* red *)
    "#9467bd";
    (* purple *)
    "#8c564b";
    (* brown *)
    "#e377c2";
    (* pink *)
    "#7f7f7f";
    (* gray *)
    "#bcbd22";
    (* olive *)
    "#17becf";
    (* cyan *)
    "#aec7e8";
    (* light blue *)
    "#ffbb78";
    (* light orange *)
    "#98df8a";
    (* light green *)
    "#ff9896";
    (* light red *)
    "#c5b0d5";
    (* light purple *)
    "#c49c94";
    (* light brown *)
    "#f7b6d3";
    (* light pink *)
    "#c7c7c7";
    (* light gray *)
    "#dbdb8d";
    (* light olive *)
    "#9edae5";
    (* light cyan *)
  |]

let get_color index = color_palette.(index mod Array.length color_palette)

(** {1 Layout Configuration} *)

let layout title =
  let ( = ) = fun x y -> (x, y) in
  `Assoc
    [
      "title" = `String title;
      "hovermode" = `String "x";
      "dragmode" = `String "zoom";
      "autosize" = `Bool true;
      "height" = `Int 800;
      (* Twice as tall - was ~400px default *)
      "margin"
      = `Assoc
          [
            "l" = `Int 80;
            (* Left margin for y-axis labels *)
            "r" = `Int 100;
            (* Right margin for y2-axis labels *)
            "t" = `Int 100;
            (* Top margin for title *)
            "b" = `Int 80;
            (* Bottom margin for x-axis *)
            "pad" = `Int 4;
          ];
      "legend"
      = `Assoc
          [
            "orientation" = `String "h";
            (* Horizontal legend *)
            "yanchor" = `String "bottom";
            "y" = `Float 1.02;
            (* Position above plot *)
            "xanchor" = `String "left";
            "x" = `Float 0.0;
          ];
      "xaxis" = `Assoc [ "title" = `String "Time"; "type" = `String "date" ];
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

let direct_price_trace ?(start = 0) ?(color = "#1f77b4") ?end_ (data : Data.t)
    (symbol : Instrument.t) : Yojson.Safe.t =
  let length = Data.length data in
  let end_idx = Option.value end_ ~default:length in
  let end_idx = Int.min end_idx length in
  let symbol_str = Instrument.symbol symbol in

  let rec build_lists i acc_x acc_y =
    if i >= end_idx then (List.rev acc_x, List.rev acc_y)
    else
      let timestamp =
        Data.get_unsafe data Time i
        |> Ptime.of_float_s
        |> Option.map Ptime.to_rfc3339
        |> Option.get_exn_or "Invalid timestamp in direct_price_trace"
      in
      let price = Data.get_unsafe data Last i in
      build_lists (i + 1)
        (`String timestamp :: acc_x)
        (sanitize_float price :: acc_y)
  in
  let x, y = build_lists start [] [] in

  `Assoc
    [
      ("x", `List x);
      ("y", `List y);
      ("text", `String symbol_str);
      ("name", `String symbol_str);
      ("type", `String "scatter");
      ("line", `Assoc [ ("color", `String color) ]);
    ]

let indicator_trace ?(show = false) ?(drop = 100) ?(yaxis = "y1")
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
          Data.get_unsafe data Time i
          |> Ptime.of_float_s
          |> Option.map Ptime.to_rfc3339
          |> Option.get_exn_or "Illegal time stored in data table (plotly.ml)"
        in
        let value = Data.get_unsafe data indicator i in
        build_lists (i + 1)
          (`String timestamp :: acc_x)
          (sanitize_float value :: acc_y)
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
  let y = List.map (fun (x : Order.t) -> sanitize_float x.price) orders in
  let hovertext =
    List.map
      (fun (x : Order.t) ->
        let symbol = x.symbol in
        let position_value = float_of_int x.qty *. x.price in
        `String
          (Format.asprintf "%s<br>Qty: %d<br>Price: $%.2f<br>Value: $%.2f<br>%s"
             (Instrument.symbol symbol) x.qty x.price position_value
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

module TI = Tacaml.Indicator

(* Helper function to interpolate portfolio value at a given time *)
let interpolate_portfolio_value_at_time (state : Longleaf_state.t)
    (target_time : Time.t) : float =
  let value_history = Longleaf_state.value_history state |> List.rev in
  match value_history with
  | [] -> 100000.0 (* Default initial value *)
  | (first_time, first_value) :: rest ->
    if Ptime.compare target_time first_time <= 0 then first_value
    else
      let rec find_closest acc = function
        | [] -> acc
        | (_time, _value) :: _ when Ptime.compare target_time _time <= 0 -> acc
        | (_time, value) :: rest -> find_closest value rest
      in
      find_closest first_value rest

(* Create order trace with portfolio values *)
let create_portfolio_order_trace (side : Trading_types.Side.t)
    (orders : Order.t list) (state : Longleaf_state.t) : Yojson.Safe.t list =
  if List.is_empty orders then []
  else
    let order_points =
      List.map
        (fun (order : Order.t) ->
          let portfolio_value =
            interpolate_portfolio_value_at_time state order.timestamp
          in
          (order.timestamp, portfolio_value, order))
        orders
    in

    let x =
      List.map
        (fun (time, _, _) -> `String (Ptime.to_rfc3339 time))
        order_points
    in
    let y = List.map (fun (_, value, _) -> sanitize_float value) order_points in
    let hovertext =
      List.map
        (fun (_, _, (order : Order.t)) ->
          let position_value = float_of_int order.qty *. order.price in
          `String
            (Format.asprintf
               "%s %s<br>Qty: %d<br>Price: $%.2f<br>Value: $%.2f<br>%s"
               (Trading_types.Side.to_string order.side)
               (Instrument.symbol order.symbol)
               order.qty order.price position_value
               (String.concat "<br>" order.reason)))
        order_points
    in

    let color = Trading_types.Side.to_color side in
    let symbol_shape =
      if Trading_types.Side.equal side Buy then "triangle-up"
      else "triangle-down"
    in
    [
      `Assoc
        [
          ("x", `List x);
          ("y", `List y);
          ("hovertext", `List hovertext);
          ("hoverinfo", `String "text");
          ("type", `String "scatter");
          ("mode", `String "markers");
          ( "marker",
            `Assoc
              [
                ("color", `String color);
                ("size", `Int 12);
                ("symbol", `String symbol_shape);
              ] );
          ("name", `String (Trading_types.Side.to_string side ^ " Orders"));
        ];
    ]

let performance_graph_with_orders (state : Longleaf_state.t) : Yojson.Safe.t =
  (* Portfolio performance trace *)
  let performance_trace =
    List.(
      let+ time, value = Longleaf_state.value_history state in
      (Ptime.to_rfc3339 time |> yojson_of_string, sanitize_float value))
    |> List.rev |> List.split
    |> Pair.map_same (yojson_of_list Fun.id)
    |> fun (x, y) ->
    `Assoc
      [
        ("x", x);
        ("y", y);
        ("text", `String "Portfolio Value");
        ("name", `String "Portfolio Value");
        ("type", `String "scatter");
        ("mode", `String "lines");
        ("line", `Assoc [ ("color", `String "#1890ff"); ("width", `Int 3) ]);
      ]
  in

  (* Cash trace *)
  let cash_trace =
    List.(
      let+ time, cash = Longleaf_state.cash_history state in
      (Ptime.to_rfc3339 time |> yojson_of_string, sanitize_float cash))
    |> List.rev |> List.split
    |> Pair.map_same (yojson_of_list Fun.id)
    |> fun (x, y) ->
    `Assoc
      [
        ("x", x);
        ("y", y);
        ("text", `String "Cash");
        ("name", `String "Cash");
        ("type", `String "scatter");
        ("mode", `String "lines");
        ("line", `Assoc [ ("color", `String "#52c41a"); ("width", `Int 2) ]);
      ]
  in

  (* Get ALL orders from state order history *)
  let all_orders_flat = Longleaf_state.order_history state in

  (* Separate buy and sell orders *)
  let buy_orders =
    List.filter
      (fun (o : Order.t) -> Trading_types.Side.equal o.side Buy)
      all_orders_flat
  in
  let sell_orders =
    List.filter
      (fun (o : Order.t) -> Trading_types.Side.equal o.side Sell)
      all_orders_flat
  in

  (* Create order traces *)
  let buy_trace = create_portfolio_order_trace Buy buy_orders state in
  let sell_trace = create_portfolio_order_trace Sell sell_orders state in

  (* Combine all traces *)
  let all_traces = [ performance_trace; cash_trace ] @ buy_trace @ sell_trace in
  `Assoc
    [
      ("traces", `List all_traces);
      ("layout", layout "Portfolio Performance with Orders");
    ]

let of_state ?(start = 100) ?end_ (state : Longleaf_state.t) symbol :
    Yojson.Safe.t option =
  let ( let* ) = Result.( let* ) in
  let result =
    let bars = Longleaf_state.bars state in
    let* data = Bars.get bars symbol in
    let price_trace =
      direct_price_trace ~start ~color:(get_color 0) ?end_ data symbol
    in
    let indicators x = Tacaml.get_indicators x |> List.uniq ~eq:Equal.poly in
    let all_indicators =
      (Longleaf_state.config state).indicator_config.tacaml_indicators
      |> List.flat_map indicators
    in
    let trace_with_color i indicator =
      (* Start color index at 1 to avoid the same color as price trace *)
      let color = get_color (i + 1) in
      indicator_trace ~drop:100 ~color bars (Data.Type.tacaml indicator) symbol
    in
    let* indicators =
      Result.map_l
        (fun (i, indicator) -> trace_with_color i indicator)
        (List.mapi (fun i x -> (i, x)) all_indicators)
    in
    (* Extract orders for this symbol from state order history *)
    let all_orders = Longleaf_state.order_history state in
    let orders_in_range =
      List.filter
        (fun (order : Order.t) -> Instrument.equal order.symbol symbol)
        all_orders
    in
    let buy_orders =
      List.filter
        (fun (order : Order.t) -> Trading_types.Side.equal order.side Buy)
        orders_in_range
    in
    let sell_orders =
      List.filter
        (fun (order : Order.t) -> Trading_types.Side.equal order.side Sell)
        orders_in_range
    in
    let buy_trace =
      if List.is_empty buy_orders then [] else [ order_trace Buy buy_orders ]
    in
    let sell_trace =
      if List.is_empty sell_orders then [] else [ order_trace Sell sell_orders ]
    in
    let ( = ) = fun x y -> (x, y) in
    Result.return
    @@ `Assoc
         [
           "traces"
           = `List ((price_trace :: indicators) @ buy_trace @ sell_trace);
           "layout" = layout @@ Instrument.symbol symbol;
         ]
  in
  match result with
  | Ok json -> Some json
  | Error e ->
    Eio.traceln "%a" Error.pp e;
    None

(* let of_bars_with_custom_indicator ?(start = 100) ?end_ (bars : Bars.t) symbol *)
(*     custom_indicator color yaxis : Yojson.Safe.t option = *)
(*   let ( let* ) = Result.( let* ) in *)
(*   let result = *)
(*     let* data = Bars.get bars symbol in *)
(*     let price_trace = direct_price_trace ~start ?end_ data symbol in *)

(*     (\* Create custom indicator trace *\) *)
(*     (\* let* custom_trace = *\) *)
(*     (\*   indicator_trace ~show:true ~drop:34 ~yaxis ~color ~width:2 ~start ?end_ *\) *)
(*     (\*     bars (Data.Type.CustomTacaml custom_indicator) symbol *\) *)
(*     (\* in *\) *)

(*     (\* (\\* Create some basic indicators for context *\\) *\) *)
(*     (\* let* sma_20 = *\) *)
(*     (\*   indicator_trace ~show:true ~drop:20 ~color:"#ff7f0e" ~width:2 ~start ?end_ *\) *)
(*     (\*     bars (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.Sma)) *\) *)
(*     (\*     symbol *\) *)
(*     (\* in *\) *)
(*     (\* let* ema_20 = *\) *)
(*     (\*   indicator_trace ~show:false ~drop:20 ~color:"#2ca02c" ~width:2 ~start *\) *)
(*     (\*     ?end_ bars *\) *)
(*     (\*     (Data.Type.Tacaml (Tacaml.Indicator.F Tacaml.Indicator.Float.Ema)) *\) *)
(*     (\*     symbol *\) *)
(*     (\* in *\) *)
(*     let ( = ) = fun x y -> (x, y) in *)
(*     Result.return *)
(*     @@ `Assoc *)
(*          [ *)
(*            "traces" = `List [ price_trace ]; *)
(*            (\* Add the custom indicator? *\) *)
(*            "layout" = layout @@ Instrument.symbol symbol; *)
(*          ] *)
(*   in *)
(*   match result with *)
(*   | Ok json -> Some json *)
(*   | Error e -> *)
(*     Eio.traceln "%a" Error.pp e; *)
(*     None *)

(** {1 Statistics Module} *)
(* TODO: Reimplement stats visualization using Trading_state *)

(* module Stats = struct
   This module has been removed as Stats.t is no longer available.
   Future implementation should use Trading_state for portfolio visualization.
end *)
