module Longleaf_error = Error
module Longleaf_mutex = Longleaf_mutex
open Piaf
module Promise = Eio.Std.Promise

let prom, resolver = Promise.create ()

let serve_favicon () =
  let favicon_path = "./static/favicon.ico" in
  let body = Util.read_file_as_string favicon_path in
  (* Eio.traceln "@[favicon has length %d@]@." (String.length body); *)
  let headers = Headers.of_list [ ("Content-Type", "image/x-icon") ] in
  Response.of_string ~headers ~body `OK

let plotly_response_of_symbol ~(mutices : Longleaf_mutex.t) target =
  let bars = Pmutex.get mutices.data_mutex in
  let bars_json_opt = Plotly.of_bars bars target in
  match bars_json_opt with
  | Some bars -> Response.of_string ~body:(Yojson.Safe.to_string bars) `OK
  | None ->
    let headers = Headers.of_list [ ("connection", "close") ] in
    Response.of_string ~headers `Not_found
      ~body:
        (Format.asprintf "Could not find bars for symbol: %a" Instrument.pp
           target)

let data_prefix = String.prefix ~pre:"/data/"

let connection_handler ~(mutices : Longleaf_mutex.t)
    (params : Request_info.t Server.ctx) =
  (* Eio.traceln "gui.ml: connection handler"; *)
  match params.request with
  | { Request.meth = `GET; target = "/"; _ } ->
    let body = Util.read_file_as_string "./static/index.html" in
    Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/favicon.ico"; _ } ->
    Eio.traceln "@[Serving favicon.@]@.";
    serve_favicon ()
  | { Request.meth = `GET; target = "/shutdown"; _ } ->
    Promise.resolve resolver true;
    Pmutex.set mutices.shutdown_mutex true;
    Response.of_string ~body:"Shutdown command sent" `OK
  | { Request.meth = `GET; target = "/plotly_latest.js"; _ } ->
    let headers =
      Headers.of_list [ ("Content-Type", "application/javascript") ]
    in
    let body = Util.read_file_as_string "./static/plotly_latest.js" in
    Response.of_string ~body ~headers `OK
  | { Request.meth = `GET; target = "/plotly_graph.js"; _ } ->
    (* Eio.traceln "GET request for my javascript"; *)
    let file_path = "./static/plotly_graph.js" in
    let body = Util.read_file_as_string file_path in
    let headers =
      Headers.of_list [ ("Content-Type", "application/javascript") ]
    in
    Response.of_string ~headers ~body `OK
  | { Request.meth = `GET; target = "/orders"; _ } ->
    let trading_state = Pmutex.get mutices.trading_state_mutex in
    let active_orders = State.Core.get_active_orders trading_state in
    let pending_orders = State.Core.get_pending_orders trading_state in
    let format_order (r : State.Core.Order_record.t) =
      Format.asprintf "%a" Order.pp r.order
    in
    let body =
      `Assoc
        [
          ( "active",
            `List (List.map (fun r -> `String (format_order r)) active_orders)
          );
          ( "pending",
            `List (List.map (fun r -> `String (format_order r)) pending_orders)
          );
        ]
      |> Yojson.Safe.to_string
    in
    Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/stats"; _ } ->
    let trading_state = Pmutex.get mutices.trading_state_mutex in
    let body =
      `Assoc
        [
          ("cash", `Float trading_state.cash);
          ("positions_taken", `Int trading_state.positions_taken);
          ("positions_possible", `Int trading_state.positions_possible);
          ( "active_positions",
            `Int (State.Core.SymbolMap.cardinal trading_state.positions) );
        ]
      |> Yojson.Safe.to_string
    in
    Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/symbols"; _ } ->
    let bars = Pmutex.get mutices.data_mutex in
    let symbols_list =
      Bars.fold bars [] (fun symbol _data acc ->
          Instrument.symbol symbol :: acc)
    in
    let symbols_str = String.concat "," symbols_list in
    let body =
      `Assoc [ ("symbols", `String symbols_str) ] |> Yojson.Safe.to_string
    in
    Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/target_symbol"; _ } ->
    let body =
      Pmutex.get mutices.target_symbol
      |> Option.get_exn_or
           "gui: Must have target symbol to display information..."
      |> fun s -> `Assoc [ ("symbols", `String s) ] |> Yojson.Safe.to_string
    in
    Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/graphs_json"; _ } ->
    let bars = Pmutex.get mutices.data_mutex in
    let body =
      match Bars.yojson_of_t bars with
      | Ok x -> Yojson.Safe.to_string x
      | Error e ->
        Eio.traceln "%a" Longleaf_error.pp e;
        invalid_arg "Error while converting bars to json"
    in
    Response.of_string ~body `OK
  | { Request.meth = `GET; target; _ } when data_prefix target -> (
    let target =
      String.chop_prefix ~pre:"/data" target |> function
      | None -> invalid_arg "Unable to get data target (server.ml)"
      | Some s -> s
    in
    let target = String.filter (fun x -> not @@ Char.equal '/' x) target in
    let instrument = Instrument.of_string_res target in
    match instrument with
    | Ok targ -> plotly_response_of_symbol ~mutices targ
    | Error _ ->
      Response.of_string
        ~body:("Unable to create Instrument.t from " ^ target)
        `Internal_server_error)
  | { Request.meth = `GET; target; _ } ->
    Pmutex.set mutices.target_symbol (Some target);
    let body = Util.read_file_as_string "./static/single.html" in
    Response.of_string ~body `OK
  | r ->
    Eio.traceln "@[Unknown request: %a@]@." Request.pp_hum r;
    let headers = Headers.of_list [ ("connection", "close") ] in
    Response.of_string ~headers `Method_not_allowed ~body:"Unknown endpoint!"

let run ~sw ~host ~port env handler =
  let config =
    Server.Config.create ~buffer_size:0x1000 ~domains:1 (`Tcp (host, port))
  in
  let server = Server.create ~config handler in
  let command = Server.Command.start ~sw env server in
  (* Server.Command.shutdown command *)
  command

let start ~sw ~(mutices : Longleaf_mutex.t) env =
  let host = Eio.Net.Ipaddr.V4.loopback in
  Eio.traceln "Server listening on port 8080";
  run ~sw ~host ~port:8080 env @@ connection_handler ~mutices

(* let setup_log ?style_renderer level = *)
(*   Logs_threaded.enable (); *)
(*   Fmt_tty.setup_std_outputs ?style_renderer (); *)
(*   Logs.set_level ~all:true level; *)
(*   Logs.set_reporter (Logs_fmt.reporter ()) *)

let top ~(mutices : Longleaf_mutex.t) env =
  (* setup_log (Some Info); *)
  Eio.Std.Switch.run (fun sw ->
      (* let openai_response = *)
      (*   Llm.Anthropic.chat ~sw ~env "What is your favorite color?" *)
      (* in *)
      (* Eio.traceln "@[OpenAI response:@]@.@[%a@]@." Yojson.Safe.pp *)
      (*   openai_response; *)
      let command = start ~mutices ~sw env in
      let _ =
        let _ = Promise.await prom in
        Eio.Time.sleep env#clock 1.0;
        Server.Command.shutdown command
      in
      ())
