module Error = Core.Error
module CLI = Core.Options.CLI
module Target = Core.Target
module Instrument = Longleaf_core.Instrument
module Tools = Longleaf_apis.Tools

module Settings = struct
  type status = Ready | Started | Error [@@deriving show, yojson]

  type t = {
    mutable cli_vars : CLI.t;
    mutable target : Target.t;
    mutable last_value : float;
    mutable status : status;
    mutable mutices : (Longleaf_state.Mutex.t option[@yojson.opaque]);
  }
  [@@deriving show, yojson]

  let settings =
    {
      cli_vars = CLI.default;
      target = Download;
      status = Ready;
      last_value = 0.0;
      mutices = None;
    }
end

(* Helper functions for responses *)
let add_cors_headers cors_origin headers =
  match cors_origin with
  | None -> headers
  | Some origin ->
    Cohttp.Header.add_list headers [
      ("Access-Control-Allow-Origin", origin);
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]

(* Content type detection for static files *)
let content_type_of_extension ext =
  match String.lowercase_ascii ext with
  | ".html" | ".htm" -> "text/html"
  | ".css" -> "text/css"
  | ".js" -> "application/javascript"
  | ".json" -> "application/json"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | ".svg" -> "image/svg+xml"
  | ".ico" -> "image/x-icon"
  | ".woff" -> "font/woff"
  | ".woff2" -> "font/woff2"
  | ".ttf" -> "font/ttf"
  | ".map" -> "application/json"
  | _ -> "application/octet-stream"

let get_extension filename =
  match String.rindex_opt filename '.' with
  | Some i -> String.sub filename i (String.length filename - i)
  | None -> ""

(* Serve a static file *)
let serve_static_file ~cors_origin static_dir path =
  (* Prevent directory traversal attacks *)
  let safe_path =
    path
    |> String.split ~by:"/"
    |> List.filter (fun s -> not (String.equal s "..") && not (String.equal s "."))
    |> String.concat "/"
  in
  let file_path = Filename.concat static_dir safe_path in
  if Sys.file_exists file_path && not (Sys.is_directory file_path) then
    let content = In_channel.with_open_bin file_path In_channel.input_all in
    let ext = get_extension file_path in
    let content_type = content_type_of_extension ext in
    let headers = Cohttp.Header.init_with "Content-Type" content_type
      |> add_cors_headers cors_origin in
    Some (Cohttp_eio.Server.respond_string ~headers ~status:`OK ~body:content ())
  else
    None

(* Serve index.html for SPA routing *)
let serve_index ~cors_origin static_dir =
  let index_path = Filename.concat static_dir "index.html" in
  if Sys.file_exists index_path then
    let content = In_channel.with_open_bin index_path In_channel.input_all in
    let headers = Cohttp.Header.init_with "Content-Type" "text/html"
      |> add_cors_headers cors_origin in
    Some (Cohttp_eio.Server.respond_string ~headers ~status:`OK ~body:content ())
  else
    None

let respond_json ?(status = `OK) ?(cors_origin = None) json_string =
  let headers = Cohttp.Header.init_with "Content-Type" "application/json"
    |> add_cors_headers cors_origin in
  Cohttp_eio.Server.respond_string ~headers ~status ~body:json_string ()

let respond_html ?(status = `OK) ?(cors_origin = None) html_string =
  let headers = Cohttp.Header.init_with "Content-Type" "text/html"
    |> add_cors_headers cors_origin in
  Cohttp_eio.Server.respond_string ~headers ~status ~body:html_string ()

let respond_text ?(status = `OK) ?(cors_origin = None) text =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain"
    |> add_cors_headers cors_origin in
  Cohttp_eio.Server.respond_string ~headers ~status ~body:text ()

let respond_404 ?(cors_origin = None) () = respond_text ~cors_origin ~status:`Not_found "404 Not Found"
let respond_500 ?(cors_origin = None) message = respond_text ~cors_origin ~status:`Internal_server_error message

(* Main request handler *)
let handler env cors_origin static_dir _conn request body =
  let ( let* ) = Result.( let* ) in
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in

  Eio.traceln "%s %s" (Cohttp.Code.string_of_method meth) path;

  let result = match (meth, path) with
  (* OPTIONS - CORS preflight *)
  | `OPTIONS, _ ->
    Result.return @@ respond_text ~cors_origin ~status:`OK ""

  (* GET /execute - Start strategy execution *)
  | `GET, "/execute" ->
    let s = Settings.settings in
    s.status <- Started;
    Eio.traceln "running strategy";
    (* Spawn strategy execution in a separate domain to avoid blocking *)
    let mgr = Eio.Stdenv.domain_mgr env in
    let () =
      Eio.Domain_manager.run mgr @@ fun () ->
      let mutices = Option.return @@ Longleaf_state.Mutex.create [] in
      Settings.settings.mutices <- mutices;
      let res =
        Longleaf_strategies.Run.top mutices env s.cli_vars s.target
      in
      match res with
      | Ok k ->
        Eio.traceln "got a result back";
        Settings.settings.last_value <- k;
        Settings.settings.status <- Ready
      | Error e ->
        Eio.traceln "%a" Error.pp e;
        Settings.settings.status <- Error
    in
    Result.return @@
    (`Assoc [ ("message", `String "strategy execution started") ]
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* GET /status - Get current status *)
  | `GET, "/status" ->
    Result.return @@
    (Settings.yojson_of_status Settings.settings.status
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* GET /performance - Get performance data *)
  | `GET, "/performance" ->
    let response =
      Settings.settings.mutices
      |> Option.map (fun (m : Longleaf_state.Mutex.t) ->
             Longleaf_util.Pmutex.get m.state_mutex)
      |> Option.map Longleaf_server__Plotly.performance_graph_with_orders
      |> ( function
      | None ->
        (* Return empty but valid performance data structure *)
        `Assoc
          [
            ("traces", `List []);
            ( "layout",
              `Assoc
                [
                  ("title", `String "Portfolio Performance");
                  ("xaxis", `Assoc [ ("title", `String "Time") ]);
                  ("yaxis", `Assoc [ ("title", `String "Value") ]);
                ] );
          ]
        |> Yojson.Safe.to_string |> respond_json ~cors_origin
      | Some j -> Yojson.Safe.to_string j |> respond_json ~cors_origin )
    in
    Result.return response

  (* GET /settings - Get current settings *)
  | `GET, "/settings" ->
    Result.return @@
    (Settings.yojson_of_t Settings.settings
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* GET /shutdown - Trigger shutdown *)
  | `GET, "/shutdown" ->
    let response =
      Option.Infix.(
        let+ mutices = Settings.settings.mutices in
        Longleaf_util.Pmutex.set mutices.shutdown_mutex true)
      |> ( function
      | None ->
        `Assoc
          [
            ("error", `String "Unable to get shutdown mutex at shutdown endpoint");
          ]
        |> Yojson.Safe.to_string
        |> fun json -> respond_json ~cors_origin ~status:`Bad_request json
      | Some () ->
        `Assoc [ ("message", `String "shutdown mutex set") ]
        |> Yojson.Safe.to_string |> respond_json ~cors_origin )
    in
    Result.return response

  (* GET /data - List available data files *)
  | `GET, "/data" ->
    Result.return @@
    (Bars.files ()
     |> List.filter (fun x -> not @@ String.is_empty x)
     |> List.map (fun x -> `String x)
     |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* GET /options - Show configured options *)
  | `GET, "/options" ->
    Result.return @@
    (`Assoc [ ("message", `String "Show configured options") ]
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* GET /strategies - List all strategies *)
  | `GET, "/strategies" ->
    Result.return @@
    (List.map (fun x -> `String x) (Longleaf_strategies.all_strategy_names ())
     |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* GET /strategy/:name - Get strategy details *)
  | `GET, path when String.starts_with ~prefix:"/strategy/" path ->
    let strategy_name = String.sub path 10 (String.length path - 10) in
    let response = match Longleaf_strategies.find_gadt_strategy strategy_name with
    | Some strategy ->
      let buy_trigger_str = Longleaf_gadt.Gadt.to_string strategy.buy_trigger in
      let sell_trigger_str =
        Longleaf_gadt.Gadt.to_string strategy.sell_trigger
      in
      let strategy_info =
        `Assoc
          [
            ("name", `String strategy.name);
            ("max_positions", `Int strategy.max_positions);
            ("position_size", `Float strategy.position_size);
            ("buy_trigger", `String buy_trigger_str);
            ("sell_trigger", `String sell_trigger_str);
          ]
      in
      Yojson.Safe.to_string strategy_info |> respond_json ~cors_origin
    | None ->
      `Assoc
        [
          ( "error",
            `String (Printf.sprintf "Strategy '%s' not found" strategy_name) );
        ]
      |> Yojson.Safe.to_string |> respond_json ~cors_origin
    in
    Result.return response

  (* GET /stats - Get trade statistics as JSON *)
  | `GET, "/stats" ->
    let response =
      Option.Infix.(
        let* mutices = Settings.settings.mutices in
        let state = Longleaf_util.Pmutex.get mutices.state_mutex in
        let all_orders = Longleaf_state.order_history state in
        Longleaf_state.Stats.TradeStats.compute all_orders)
      |> (function
      | None ->
        `Assoc [("error", `String "No completed trades available")]
        |> Yojson.Safe.to_string |> respond_json ~cors_origin
      | Some stats ->
        Longleaf_state.Stats.TradeStats.yojson_of_t stats
        |> Yojson.Safe.to_string |> respond_json ~cors_origin)
    in
    Result.return response

  (* GET /symbols - Get current symbols *)
  | `GET, "/symbols" ->
    let response =
      Option.Infix.(
        let* mutices = Settings.settings.mutices in
        let+ symbols = Longleaf_util.Pmutex.get mutices.symbols_mutex in
        Eio.traceln "returning symbols %s" symbols;
        symbols)
      |> ( function
      | None ->
        Eio.traceln "mutices not set (symbols)";
        `List [] |> Yojson.Safe.to_string |> respond_json ~cors_origin
      | Some symbols ->
        (String.split ~by:"," symbols |> List.map @@ fun x -> `String x)
        |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json ~cors_origin )
    in
    Result.return response

  (* GET /data/:symbol/json - Get data for symbol *)
  | `GET, path
    when String.starts_with ~prefix:"/data/" path
         && String.ends_with ~suffix:"/json" path ->
    (* Extract symbol from "/data/SYMBOL/json" *)
    let parts = String.split ~by:"/" path in
    let response = match parts with
    | [ ""; "data"; symbol; "json" ] ->
      let instrument = Instrument.of_string symbol in
      Option.Infix.(
        let* mutices = Settings.settings.mutices in
        Eio.traceln "symbol request %s" symbol;
        let state = Longleaf_util.Pmutex.get mutices.state_mutex in
        Longleaf_server__Plotly.of_state state instrument)
      |> ( function
      | None ->
        Eio.traceln "failed to get mutices or plotly json for instrument";
        (* Return empty but valid chart data structure *)
        `Assoc
          [
            ("traces", `List []);
            ( "layout",
              `Assoc
                [
                  ("title", `String ("Chart for " ^ symbol));
                  ("xaxis", `Assoc [ ("title", `String "Time") ]);
                  ("yaxis", `Assoc [ ("title", `String "Price") ]);
                ] );
          ]
        |> Yojson.Safe.to_string |> respond_json ~cors_origin
      | Some j -> Yojson.Safe.to_string j |> respond_json ~cors_origin )
    | _ -> respond_404 ~cors_origin ()
    in
    Result.return response

  (* GET /tearsheet - Generate tearsheet *)
  | `GET, "/tearsheet" ->
    Eio.traceln "generating tearsheet via QuantStats FastAPI service";

    (* Extract actual returns and dates from state/portfolio history *)
    let state_data =
      Option.Infix.(
        let* mutices = Settings.settings.mutices in
        Longleaf_util.Pmutex.get mutices.state_mutex
        |> Longleaf_state.Conv.to_tearsheet_json |> Option.return)
    in
    let* json_response = match state_data with
    | None ->
      Error.fatal "No strategy state available. Please start a strategy first."
    | Some json ->
      (* Add benchmark and title to the JSON from State.Conv *)
      let base_assoc =
        match json with
        | `Assoc assoc -> assoc
        | _ -> []
      in
      let request_json =
        `Assoc
          (base_assoc
          @ [
              ("benchmark", `String "SPY");
              ("title", `String "Longleaf Strategy Tearsheet");
            ])
      in
      let request = Yojson.Safe.to_string request_json in

      (* Make HTTP POST request to Python FastAPI service *)
      let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in
      let headers =
        Cohttp.Header.of_list [ ("Content-Type", "application/json") ]
      in
      let body_json = Yojson.Safe.from_string request in
      (* Use post_cohttp_html since tearsheet returns HTML, not JSON *)
      Tools.post_cohttp_html ~client ~body:body_json ~headers
          ~endpoint:"http://localhost:5000/tearsheet"
    in
    Result.return @@ respond_html ~cors_origin ~status:`OK json_response

  (* GET / - Root endpoint, serve index.html if static_dir is set, else return settings *)
  | `GET, "/" ->
    (match static_dir with
    | Some dir ->
      Eio.traceln "Serving index.html from %s" dir;
      (match serve_index ~cors_origin dir with
      | Some response -> Result.return response
      | None ->
        Eio.traceln "index.html not found, returning settings JSON";
        let json = Settings.yojson_of_t Settings.settings |> Yojson.Safe.to_string in
        Result.return @@ respond_json ~cors_origin json)
    | None ->
      Eio.traceln "=== GET / endpoint called (no static dir) ===";
      let json = Settings.yojson_of_t Settings.settings |> Yojson.Safe.to_string in
      Result.return @@ respond_json ~cors_origin json)

  (* POST /set_status - Set status *)
  | `POST, "/set_status" ->
    let* body_string = Tools.read_body body in
    Eio.traceln "%s" body_string;
    let status =
      Yojson.Safe.from_string body_string |> Settings.status_of_yojson
    in
    Settings.settings.status <- status;
    Result.return @@
    (`Assoc [ ("message", `String "Set status") ]
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* POST /set_target - Set target *)
  | `POST, "/set_target" ->
    let* target_str = Tools.read_body body in
    let* target =
      let* target =
        Result.return @@ Core.Target.t_of_yojson
        @@ Yojson.Safe.from_string target_str
      in
      let* target =
        match target with
        | Download -> Result.return target
        | File s ->
          let files = Bars.files () in
          if List.mem ~eq:String.equal s files then Result.return target
          else Error.fatal "Unable to find target file"
      in
      Result.return target
    in
    Settings.settings.target <- target;
    Result.return @@
    (`Assoc [ ("message", `String "settings.cli_vars.strategy_arg set") ]
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* POST /set_strategy - Set strategy *)
  | `POST, "/set_strategy" ->
    let* body_string = Tools.read_body body in
    let strategy_arg =
      Yojson.Safe.from_string body_string |> Yojson.Safe.Util.to_string
    in
    let strategies_loaded = Longleaf_strategies.all_strategy_names () in
    let* () = match List.mem strategy_arg strategies_loaded with
    | true ->
      let cli = { Settings.settings.cli_vars with strategy_arg } in
      Settings.settings.cli_vars <- cli;
      Result.return ()
    | false ->
      Error.fatal "Could not find strategy in data directory"
    in
    Result.return @@
    (`Assoc [ ("message", `String "settings.cli_vars.strategy_arg set") ]
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* POST /set_runtype - Set runtype *)
  | `POST, "/set_runtype" ->
    let* body_string = Tools.read_body body in
    let r =
      Yojson.Safe.from_string body_string |> Core.Runtype.t_of_yojson
    in
    let cli = { Settings.settings.cli_vars with runtype = r } in
    Settings.settings.cli_vars <- cli;
    Result.return @@
    (`Assoc [ ("message", `String "settings.cli_vars.runtype set") ]
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* POST /set_cli - Set CLI vars *)
  | `POST, "/set_cli" ->
    let* body_string = Tools.read_body body in
    Eio.traceln "=== /set_cli endpoint called ===";
    Eio.traceln "Received JSON: %s" body_string;
    let r =
      Yojson.Safe.from_string body_string |> Core.Options.CLI.t_of_yojson
    in
    Eio.traceln "Parsed CLI settings:";
    Eio.traceln "  print_tick_arg: %b" r.print_tick_arg;
    Eio.traceln "  strategy_arg: %s" r.strategy_arg;
    Eio.traceln "  runtype: %a" Core.Runtype.pp r.runtype;
    Settings.settings.cli_vars <- r;
    Eio.traceln "CLI settings updated in server state";
    Result.return @@
    (`Assoc [ ("message", `String "settings.cli_vars set") ]
     |> Yojson.Safe.to_string |> respond_json ~cors_origin)

  (* Static file serving and SPA fallback for GET requests *)
  | `GET, path ->
    (match static_dir with
    | Some dir ->
      (* Remove leading slash for file path *)
      let file_path = if String.prefix ~pre:"/" path
        then String.sub path 1 (String.length path - 1)
        else path
      in
      Eio.traceln "Trying to serve static file: %s" file_path;
      (match serve_static_file ~cors_origin dir file_path with
      | Some response ->
        Eio.traceln "Served static file: %s" file_path;
        Result.return response
      | None ->
        (* SPA fallback: serve index.html for unknown routes *)
        Eio.traceln "File not found, serving index.html for SPA routing";
        (match serve_index ~cors_origin dir with
        | Some response -> Result.return response
        | None -> Result.return @@ respond_404 ~cors_origin ()))
    | None ->
      Result.return @@ respond_404 ~cors_origin ())

  (* Default - 404 for non-GET methods *)
  | _ -> Result.return @@ respond_404 ~cors_origin ()
  in
  match result with
  | Ok response -> response
  | Error e -> respond_500 ~cors_origin (Error.show e)

module Args = struct
  let default_port =
    match Sys.getenv_opt "LONGLEAF_PORT" with
    | Some p -> (try int_of_string p with _ -> 8080)
    | None -> 8080

  let default_static_dir =
    match Sys.getenv_opt "LONGLEAF_STATIC_DIR" with
    | Some dir -> Some dir
    | None ->
      (* Default to react/build relative to current directory *)
      let default_path = "react/build" in
      if Sys.file_exists default_path && Sys.is_directory default_path
      then Some default_path
      else None

  (* CORS is only needed if serving frontend from a different origin *)
  let default_cors_origin =
    match default_static_dir with
    | Some _ -> None  (* Same origin, no CORS needed *)
    | None ->
      (* No static dir = likely using separate dev server *)
      match Sys.getenv_opt "REACT_PORT" with
      | Some p -> Some (Printf.sprintf "http://localhost:%s" p)
      | None -> Some "http://localhost:3000"

  let port_arg =
    let doc = "Port for HTTP server to listen on (default from LONGLEAF_PORT env var or 8080)." in
    Cmdliner.Arg.(value & opt int default_port & info ["port"; "p"] ~doc)

  let cors_origin_arg =
    let doc = "CORS origin for React frontend (default from REACT_PORT env var). Use --no-cors to disable CORS." in
    Cmdliner.Arg.(value & opt (some string) default_cors_origin & info ["cors-origin"] ~doc)

  let no_cors_arg =
    let doc = "Disable CORS headers entirely." in
    Cmdliner.Arg.(value & flag & info ["no-cors"] ~doc)

  let static_dir_arg =
    let doc = "Directory containing static files to serve (e.g., React build directory). If set, serves static files and falls back to index.html for SPA routing." in
    Cmdliner.Arg.(value & opt (some string) default_static_dir & info ["static-dir"; "s"] ~doc)
end

module Cmd = struct
  let run port cors_origin no_cors static_dir =
    let cors_origin = if no_cors then None else cors_origin in
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Eio.traceln "Starting server on http://localhost:%d" port;
    (match static_dir with
    | Some dir ->
      Eio.traceln "Serving static files from: %s" dir;
      Eio.traceln "CORS: allowing all origins for same-origin requests"
    | None ->
      Eio.traceln "Static file serving: disabled");
    Eio.traceln "CORS origin: %s"
      (match cors_origin with
       | Some origin -> origin
       | None -> "disabled");
    let socket =
      Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true ~reuse_port:true
        (`Tcp (Eio.Net.Ipaddr.V4.any, port))
    in
    let server = Cohttp_eio.Server.make ~callback:(handler env cors_origin static_dir) () in
    Cohttp_eio.Server.run socket server ~on_error:raise

  let top =
    let term =
      Cmdliner.Term.(
        const run $ Args.port_arg $ Args.cors_origin_arg $ Args.no_cors_arg $ Args.static_dir_arg)
    in
    let doc = "Longleaf HTTP server for strategy management and visualization." in
    let info = Cmdliner.Cmd.info ~doc "longleaf_server" in
    Cmdliner.Cmd.v info term
end

let () = Stdlib.exit @@ Cmdliner.Cmd.eval Cmd.top
