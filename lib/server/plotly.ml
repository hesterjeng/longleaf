(** Plotly.js JSON generation for trading data visualization *)

module Bars = Longleaf_bars
module Data = Bars.Data

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

module TI = Tacaml.Indicator

let of_state ?(start = 100) ?end_ (state : 'a Longleaf_state.t) symbol :
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
    (* Extract orders for this symbol from the data *)
    let all_orders = Data.get_all_orders data in
    let length = Data.length data in
    let end_idx = Option.value end_ ~default:length in
    let end_idx = Int.min end_idx length in
    let orders_in_range =
      let rec collect_orders acc i =
        if i >= end_idx then acc
        else
          let orders_at_tick =
            if i >= start then
              List.filter
                (fun (order : Order.t) -> Instrument.equal order.symbol symbol)
                all_orders.(i)
            else []
          in
          collect_orders (orders_at_tick @ acc) (i + 1)
      in
      collect_orders [] start
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
