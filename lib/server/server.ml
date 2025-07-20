module Error = Error
module Longleaf_mutex = Longleaf_mutex

(* open Piaf *)
module Promise = Eio.Std.Promise
module Talib_binding = Indicators.Talib_binding
module Headers = Piaf.Headers
module Response = Piaf.Response
module Server = Piaf.Server
module Request_info = Piaf.Request_info
module Request = Piaf.Request
module Body = Piaf.Body

let prom, resolver = Promise.create ()

let serve_favicon () =
  let favicon_path = "./static/favicon.ico" in
  let body = Util.read_file_as_string favicon_path in
  (* Eio.traceln "@[favicon has length %d@]@." (String.length body); *)
  let headers = Headers.of_list [ ("Content-Type", "image/x-icon") ] in
  Response.of_string ~headers ~body `OK

let plotly_response_of_symbol ~(mutices : Longleaf_mutex.t) target =
  let state = Pmutex.get mutices.state_mutex in
  let bars = State.bars state in
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
let custom_indicator_prefix = String.prefix ~pre:"/custom-indicator/"

let custom_indicator_response ~(mutices : Longleaf_mutex.t) target tacaml_str
    color yaxis =
  let ( let* ) = Result.( let* ) in
  Eio.traceln
    "@[Custom indicator request: target=%a, tacaml=%s, color=%s, yaxis=%s@]@."
    Instrument.pp target tacaml_str color yaxis;
  let state = Pmutex.get mutices.state_mutex in
  let bars = State.bars state in

  let* data =
    match Bars.get bars target with
    | Ok data ->
      Eio.traceln "@[Successfully retrieved data for symbol: %a@]@."
        Instrument.pp target;
      Ok data
    | Error e ->
      let err_msg =
        Format.asprintf "Could not find data for symbol: %a" Error.pp e
      in
      Eio.traceln "@[ERROR: %s@]@." err_msg;
      Error err_msg
  in

  let* tacaml =
    match Tacaml.of_string tacaml_str with
    | Ok tacaml ->
      Eio.traceln "@[Successfully parsed tacaml: %s@]@." tacaml_str;
      Ok tacaml
    | Error e ->
      let err_msg =
        Format.asprintf "Failed to parse tacaml '%s': %s" tacaml_str e
      in
      Eio.traceln "@[ERROR: %s@]@." err_msg;
      Error err_msg
  in

  (* Register the custom indicator *)
  (* let* _slot = *)
  (* match Bars.Data.register_custom_indicator data tacaml with *)
  (* | Ok slot -> *)
  (*   Eio.traceln "@[Successfully registered custom indicator, slot: %d@]@." *)
  (*     slot; *)
  (*   Ok slot *)
  (* | Error e -> *)
  (*   let err_msg = *)
  (*     Format.asprintf "Failed to register custom indicator: %a" Error.pp e *)
  (*   in *)
  (*   Eio.traceln "@[ERROR: %s@]@." err_msg; *)
  (*   Error err_msg *)
  (* in *)

  (* Compute the custom indicator *)
  let* () =
    match Talib_binding.calculate tacaml data with
    | Ok () ->
      Eio.traceln "@[Successfully computed custom indicator %a@]@." Tacaml.pp
        tacaml;
      Ok ()
    | Error e ->
      let err_msg =
        Format.asprintf "Failed to compute custom indicator: %a" Error.pp e
      in
      Eio.traceln "@[ERROR: %s@]@." err_msg;
      Error err_msg
  in

  (* Generate plotly visualization with the custom indicator *)
  let* plotly_json =
    match
      Plotly.of_bars_with_custom_indicator bars target tacaml color yaxis
    with
    | Some json ->
      Eio.traceln "@[Successfully generated plotly visualization@]@.";
      Ok json
    | None ->
      let err_msg = "Failed to generate plotly visualization" in
      Eio.traceln "@[ERROR: %s@]@." err_msg;
      Error err_msg
  in

  Eio.traceln "@[Custom indicator response completed successfully@]@.";
  Ok (Response.of_string ~body:(Yojson.Safe.to_string plotly_json) `OK)

let connection_handler ~(mutices : Longleaf_mutex.t)
    (params : Request_info.t Server.ctx) =
  (* Eio.traceln "gui.ml: connection handler"; *)
  match params.request with
  (* | { Request.meth = `GET; target = "/"; _ } -> *)
  (*   let body = Util.read_file_as_string "./static/index.html" in *)
  (*   Response.of_string ~body `OK *)
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
    invalid_arg "Endpoint orders NYI"
    (* let trading_state = Pmutex.get mutices.state_mutex in *)
    (* let active_orders = State.get_active_orders trading_state in *)
    (* let pending_orders = State.get_pending_orders trading_state in *)
    (* let body = *)
    (*   `Assoc *)
    (*     [ *)
    (*       ( "active", *)
    (*         `List *)
    (*           (List.map *)
    (*              (fun r -> `String (State.Order_record.show r)) *)
    (*              active_orders) ); *)
    (*       ( "pending", *)
    (*         `List *)
    (*           (List.map *)
    (*              (fun r -> `String (State.Order_record.show r)) *)
    (*              pending_orders) ); *)
    (*     ] *)
    (*   |> Yojson.Safe.to_string *)
    (* in *)
    (* Response.of_string ~body `OK *)
  | { Request.meth = `GET; target = "/stats"; _ } ->
    invalid_arg "stats endpoint NYI"
    (* let trading_state = Pmutex.get mutices.state_mutex in *)
    (* let body = *)
    (*   `Assoc *)
    (*     [ *)
    (*       ("cash", `Float (State.get_cash trading_state)); *)
    (*       ("positions_taken", `Int trading_state.trading_state.positions_taken); *)
    (*       ( "positions_possible", *)
    (*         `Int trading_state.trading_state.positions_possible ); *)
    (*       ( "active_positions", *)
    (*         `Int *)
    (*           (State.SymbolMap.cardinal trading_state.trading_state.positions) *)
    (*       ); *)
    (*     ] *)
    (*   |> Yojson.Safe.to_string *)
    (* in *)
    (* Response.of_string ~body `OK *)
  | { Request.meth = `GET; target = "/symbols"; _ } ->
    let state = Pmutex.get mutices.state_mutex in
    let bars = State.bars state in
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
    let state = Pmutex.get mutices.state_mutex in
    let bars = State.bars state in
    let body =
      match Bars.yojson_of_t bars with
      | Ok x -> Yojson.Safe.to_string x
      | Error e ->
        Eio.traceln "%a" Error.pp e;
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
  | { Request.meth = `POST; target; _ } when custom_indicator_prefix target -> (
    let target =
      String.chop_prefix ~pre:"/custom-indicator" target |> function
      | None -> invalid_arg "Unable to get custom indicator target (server.ml)"
      | Some s -> s
    in
    let target = String.filter (fun x -> not @@ Char.equal '/' x) target in
    let instrument = Instrument.of_string_res target in
    match instrument with
    | Ok targ -> (
      try
        Eio.traceln "@[Processing custom indicator request for target: %a@]@."
          Instrument.pp targ;
        (* Read JSON body from request *)
        let body =
          Body.to_string params.request.body |> function
          | Ok x ->
            Eio.traceln "@[Successfully read request body: %s@]@." x;
            x
          | Error e ->
            Eio.traceln "@[ERROR reading request body: %a@]@." Piaf.Error.pp_hum
              e;
            "server.ml: Unable to convert body to string in \
             custom_indicator_prefix endpoint"
        in
        let json = Yojson.Safe.from_string body in
        Eio.traceln "@[Successfully parsed JSON body@]@.";
        let tacaml_str =
          Yojson.Safe.Util.(json |> member "tacaml" |> to_string)
        in
        let color = Yojson.Safe.Util.(json |> member "color" |> to_string) in
        let yaxis = Yojson.Safe.Util.(json |> member "yaxis" |> to_string) in
        Eio.traceln "@[Extracted parameters: tacaml=%s, color=%s, yaxis=%s@]@."
          tacaml_str color yaxis;

        match
          custom_indicator_response ~mutices targ tacaml_str color yaxis
        with
        | Ok response ->
          Eio.traceln "@[Custom indicator request completed successfully@]@.";
          response
        | Error err_msg ->
          Eio.traceln "@[Custom indicator request failed: %s@]@." err_msg;
          Response.of_string ~body:err_msg `Internal_server_error
      with
      | e ->
        let err_msg =
          "Error processing custom indicator request: " ^ Printexc.to_string e
        in
        Eio.traceln "@[EXCEPTION in custom indicator handler: %s@]@." err_msg;
        Response.of_string ~body:err_msg `Internal_server_error)
    | Error _ ->
      Response.of_string
        ~body:("Unable to create Instrument.t from " ^ target)
        `Internal_server_error)
  | { Request.meth = `GET; target = "/health"; _ } ->
    Response.of_string ~body:"Longleaf is OK" `OK
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
