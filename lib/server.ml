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
  let indicators =
    invalid_arg "NYI Server.plotly_response_of_symbol"
    (* Pmutex.get mutices.indicators_mutex |> Indicators.to_vector_table *)
  in
  let bars_json_opt = Plotly.of_bars bars indicators target in
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
    let orders = Pmutex.get mutices.orders_mutex in
    let body = Order.History.yojson_of_t orders |> Yojson.Safe.to_string in
    Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/stats"; _ } ->
    let stats = Pmutex.get mutices.stats_mutex |> Stats.sort in
    let body = Plotly.Stats.make stats |> Yojson.Safe.to_string in
    Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/symbols"; _ } ->
    let body =
      Pmutex.get mutices.symbols_mutex
      |> Option.get_exn_or "gui: Must have symbols to display information..."
      |> fun s -> `Assoc [ ("symbols", `String s) ] |> Yojson.Safe.to_string
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
    let body = Bars.yojson_of_t bars |> Yojson.Safe.to_string in
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
