type mutices = {
  shutdown_mutex : bool Pmutex.t;
  data_mutex : Bars.t Pmutex.t;
  orders_mutex : Order_history.t Pmutex.t;
  symbols_mutex : string option Pmutex.t;
  stats_mutex : Stats.t Pmutex.t;
}

open Piaf
module Promise = Eio.Std.Promise

let prom, resolver = Promise.create ()

let serve_favicon () =
  let favicon_path = "./src/static/favicon.ico" in
  let body = Util.read_file_as_string favicon_path in
  Eio.traceln "@[favicon has length %d@]@." (String.length body);
  let headers = Headers.of_list [ ("Content-Type", "image/x-icon") ] in
  Response.of_string ~headers ~body `OK

let plotly_response_of_symbol ~mutices target =
  let bars = Pmutex.get mutices.data_mutex in
  let orders = Pmutex.get mutices.orders_mutex in
  Vector.iter (fun order -> Bars.add_order order bars) orders;
  let bars_json_opt =
    ( Plotly.of_bars bars target,
      Plotly.of_bars bars @@ String.uppercase_ascii target )
  in
  match bars_json_opt with
  | Some bars, None | None, Some bars | Some bars, _ ->
      Response.of_string ~body:(Yojson.Safe.to_string bars) `OK
  | None, None ->
      let headers = Headers.of_list [ ("connection", "close") ] in
      Response.of_string ~headers `Not_found
        ~body:(Format.asprintf "Could not find bars for symbol: %S" target)

let connection_handler ~(mutices : mutices) (params : Request_info.t Server.ctx)
    =
  match params.request with
  | { Request.meth = `GET; target = "/"; _ } ->
      let body =
        match Pmutex.get mutices.symbols_mutex with
        | None -> "No symbols found"
        | Some symbols -> Html_template.render symbols
      in
      Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/favicon.ico"; _ } ->
      Eio.traceln "@[Serving favicon.@]@.";
      serve_favicon ()
  | { Request.meth = `GET; target = "/shutdown"; _ } ->
      Promise.resolve resolver true;
      Pmutex.set mutices.shutdown_mutex true;
      Response.of_string ~body:"Shutdown command sent" `OK
  | { Request.meth = `GET; target = "/src/javascript/plotly_graph.js"; _ } ->
      Eio.traceln "GET request for my javascript";
      let file_path = "./src/javascript/plotly_graph.js" in
      let body = Util.read_file_as_string file_path in
      let headers =
        Headers.of_list [ ("Content-Type", "application/javascript") ]
      in
      Response.of_string ~headers ~body `OK
  | { Request.meth = `GET; target = "/orders"; _ } ->
      let orders = Pmutex.get mutices.orders_mutex in
      let body = Order_history.yojson_of_t orders |> Yojson.Safe.to_string in
      Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/stats"; _ } ->
      let stats = Pmutex.get mutices.stats_mutex |> Stats.sort in
      (* Eio.traceln "GUI: %a" Stats.pp stats; *)
      let body = Stats.yojson_of_t stats |> Yojson.Safe.to_string in
      Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/graphs_json"; _ } ->
      let bars = Pmutex.get mutices.data_mutex in
      let body = Bars.yojson_of_t bars |> Yojson.Safe.to_string in
      Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/graphs"; _ } ->
      plotly_response_of_symbol ~mutices "NVDA"
  | { Request.meth = `GET; target; _ } ->
      let target = String.filter (fun x -> not @@ Char.equal '/' x) target in
      plotly_response_of_symbol ~mutices target
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

let start ~sw ~(mutices : mutices) env =
  let host = Eio.Net.Ipaddr.V4.loopback in
  Eio.traceln "Server listening on port 8080";
  run ~sw ~host ~port:8080 env @@ connection_handler ~mutices

(* let setup_log ?style_renderer level = *)
(*   Logs_threaded.enable (); *)
(*   Fmt_tty.setup_std_outputs ?style_renderer (); *)
(*   Logs.set_level ~all:true level; *)
(*   Logs.set_reporter (Logs_fmt.reporter ()) *)

let top ~(mutices : mutices) env =
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
        Ticker.OneSecond.tick env;
        Server.Command.shutdown command
      in
      ())
