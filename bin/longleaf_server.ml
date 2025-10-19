module Error = Core.Error
module Cmd = Cmdliner.Cmd
module CLI = Core.Options.CLI
module Target = Core.Target
module Instrument = Longleaf_core.Instrument

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
let respond_json ?(status = `OK) json_string =
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let response = Cohttp.Response.make ~status ~headers () in
  let body = Cohttp_eio.Body.of_string json_string in
  (response, body)

let respond_html ?(status = `OK) html_string =
  let headers = Cohttp.Header.init_with "Content-Type" "text/html" in
  let response = Cohttp.Response.make ~status ~headers () in
  let body = Cohttp_eio.Body.of_string html_string in
  (response, body)

let respond_text ?(status = `OK) text =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
  let response = Cohttp.Response.make ~status ~headers () in
  let body = Cohttp_eio.Body.of_string text in
  (response, body)

let respond_404 () = respond_text ~status:`Not_found "404 Not Found"
let respond_500 message = respond_text ~status:`Internal_server_error message
let log fmt = Printf.ksprintf (fun s -> Eio.traceln "[server] %s" s) fmt
let warning fmt = Printf.ksprintf (fun s -> Eio.traceln "[warning] %s" s) fmt
let error fmt = Printf.ksprintf (fun s -> Eio.traceln "[error] %s" s) fmt

(* Static file serving *)
let serve_static_file path =
  try
    let full_path = "static" ^ path in
    let ic = open_in full_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;

    (* Determine content type from extension *)
    let content_type =
      if String.ends_with ~suffix:".html" path then "text/html"
      else if String.ends_with ~suffix:".css" path then "text/css"
      else if String.ends_with ~suffix:".js" path then "application/javascript"
      else if String.ends_with ~suffix:".json" path then "application/json"
      else if String.ends_with ~suffix:".png" path then "image/png"
      else if
        String.ends_with ~suffix:".jpg" path
        || String.ends_with ~suffix:".jpeg" path
      then "image/jpeg"
      else if String.ends_with ~suffix:".svg" path then "image/svg+xml"
      else "application/octet-stream"
    in

    let headers = Cohttp.Header.init_with "Content-Type" content_type in
    let response = Cohttp.Response.make ~status:`OK ~headers () in
    let body = Cohttp_eio.Body.of_string content in
    (response, body)
  with
  | Sys_error _
  | End_of_file ->
    respond_404 ()

(* Main request handler *)
let handler env _conn request body =
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in

  log "%s %s" (Cohttp.Code.string_of_method meth) path;

  match (meth, path) with
  (* Static files *)
  | `GET, path when String.starts_with ~prefix:"/static/" path ->
    let static_path = String.sub path 7 (String.length path - 7) in
    serve_static_file ("/" ^ static_path)
  (* GET /execute - Start strategy execution *)
  | `GET, "/execute" ->
    let s = Settings.settings in
    s.status <- Started;
    (try
       log "running strategy";
       (* Spawn strategy execution in a separate domain to avoid blocking *)
       let mgr = Eio.Stdenv.domain_mgr env in
       let _domain_handle : unit Eio.Promise.t =
         Eio.Domain_manager.run_raw mgr @@ fun () ->
         try
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
         with
         | exn ->
           Eio.traceln "strategy execution failed: %s" (Printexc.to_string exn);
           Settings.settings.status <- Error
       in
       `Assoc [ ("message", `String "strategy execution started") ]
       |> Yojson.Safe.to_string |> respond_json
     with
    | exn ->
      log "strategy execution failed with exception";
      s.status <- Error;
      respond_500
      @@ Printf.sprintf "Strategy execution failed: %s" (Printexc.to_string exn))
  (* GET /status - Get current status *)
  | `GET, "/status" ->
    Settings.yojson_of_status Settings.settings.status
    |> Yojson.Safe.to_string |> respond_json
  (* GET /performance - Get performance data *)
  | `GET, "/performance" ->
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
      |> Yojson.Safe.to_string |> respond_json
    | Some j -> Yojson.Safe.to_string j |> respond_json )
  (* GET /settings - Get current settings *)
  | `GET, "/settings" ->
    Settings.yojson_of_t Settings.settings
    |> Yojson.Safe.to_string |> respond_json
  (* GET /shutdown - Trigger shutdown *)
  | `GET, "/shutdown" ->
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
      |> fun json -> respond_json ~status:`Bad_request json
    | Some () ->
      `Assoc [ ("message", `String "shutdown mutex set") ]
      |> Yojson.Safe.to_string |> respond_json )
  (* GET /data - List available data files *)
  | `GET, "/data" ->
    Bars.files ()
    |> List.filter (fun x -> not @@ String.is_empty x)
    |> List.map (fun x -> `String x)
    |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json
  (* GET /options - Show configured options *)
  | `GET, "/options" ->
    `Assoc [ ("message", `String "Show configured options") ]
    |> Yojson.Safe.to_string |> respond_json
  (* GET /strategies - List all strategies *)
  | `GET, "/strategies" ->
    List.map (fun x -> `String x) Longleaf_strategies.all_strategy_names
    |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json
  (* GET /strategy/:name - Get strategy details *)
  | `GET, path when String.starts_with ~prefix:"/strategy/" path ->
    let strategy_name = String.sub path 10 (String.length path - 10) in
    (match Longleaf_strategies.find_gadt_strategy strategy_name with
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
      Yojson.Safe.to_string strategy_info |> respond_json
    | None ->
      `Assoc
        [
          ( "error",
            `String (Printf.sprintf "Strategy '%s' not found" strategy_name) );
        ]
      |> Yojson.Safe.to_string |> respond_json)
  (* GET /symbols - Get current symbols *)
  | `GET, "/symbols" ->
    Option.Infix.(
      let* mutices = Settings.settings.mutices in
      let+ symbols = Longleaf_util.Pmutex.get mutices.symbols_mutex in
      log "returning symbols %s" symbols;
      symbols)
    |> ( function
    | None ->
      warning "mutices not set (symbols)";
      `List [] |> Yojson.Safe.to_string |> respond_json
    | Some symbols ->
      (String.split ~by:"," symbols |> List.map @@ fun x -> `String x)
      |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json )
  (* GET /data/:symbol/json - Get data for symbol *)
  | `GET, path
    when String.starts_with ~prefix:"/data/" path
         && String.ends_with ~suffix:"/json" path ->
    (* Extract symbol from "/data/SYMBOL/json" *)
    let parts = String.split ~by:"/" path in
    (match parts with
    | [ ""; "data"; symbol; "json" ] ->
      let instrument = Instrument.of_string symbol in
      Option.Infix.(
        let* mutices = Settings.settings.mutices in
        log "symbol request %s" symbol;
        let state = Longleaf_util.Pmutex.get mutices.state_mutex in
        Longleaf_server__Plotly.of_state state instrument)
      |> ( function
      | None ->
        error "failed to get mutices or plotly json for instrument";
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
        |> Yojson.Safe.to_string |> respond_json
      | Some j -> Yojson.Safe.to_string j |> respond_json )
    | _ -> respond_404 ())
  (* GET /tearsheet - Generate tearsheet *)
  | `GET, "/tearsheet" ->
    (try
       log "generating tearsheet via QuantStats FastAPI service";

       (* Extract actual returns and dates from state/portfolio history *)
       let state_data =
         Option.Infix.(
           let* mutices = Settings.settings.mutices in
           Longleaf_util.Pmutex.get mutices.state_mutex
           |> Longleaf_state.Conv.to_tearsheet_json |> Option.return)
       in
       match state_data with
       | None ->
         respond_text ~status:`Bad_request
           "No strategy state available. Please start a strategy first."
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
         let client = Cohttp_eio.Client.make (Eio.Stdenv.net env) in
         let headers =
           Cohttp.Header.of_list [ ("Content-Type", "application/json") ]
         in
         let body_json = Yojson.Safe.from_string request in
         let resp =
           Longleaf_apis.Tools.post_cohttp ~client ~body:body_json ~headers
             ~endpoint:"http://localhost:5000/tearsheet"
         in
         let body_str = Cohttp_eio.Body.to_string (Cohttp.Response.body resp) in
         respond_html ~status:`OK body_str
     with
    | exn ->
      log "tearsheet generation failed: %s" (Printexc.to_string exn);
      respond_500
      @@ Printf.sprintf "Tearsheet generation failed: %s"
           (Printexc.to_string exn))
  (* GET / - Root endpoint, return settings *)
  | `GET, "/" ->
    Settings.yojson_of_t Settings.settings
    |> Yojson.Safe.to_string |> respond_json
  (* POST /set_status - Set status *)
  | `POST, "/set_status" ->
    (try
       let body_string = Cohttp_eio.Body.to_string body in
       log "%s" body_string;
       let status =
         Yojson.Safe.from_string body_string |> Settings.status_of_yojson
       in
       Settings.settings.status <- status;
       `Assoc [ ("message", `String "Set status") ]
       |> Yojson.Safe.to_string |> respond_json
     with
    | e ->
      `Assoc [ ("error", `String (Printexc.to_string e)) ]
      |> Yojson.Safe.to_string
      |> fun json -> respond_json ~status:`Not_acceptable json)
  (* POST /set_target - Set target *)
  | `POST, "/set_target" ->
    let target_str = Cohttp_eio.Body.to_string body in
    let target =
      let ( let* ) = Result.( let* ) in
      let* target =
        try
          Result.return @@ Core.Target.t_of_yojson
          @@ Yojson.Safe.from_string target_str
        with
        | _ -> Error.json @@ "Problem converting target string" ^ target_str
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
    (match target with
    | Ok target ->
      Settings.settings.target <- target;
      `Assoc [ ("message", `String "settings.cli_vars.strategy_arg set") ]
      |> Yojson.Safe.to_string |> respond_json
    | Error e ->
      `Assoc [ ("error", `String (Error.show e)) ] |> Yojson.Safe.to_string
      |> fun json -> respond_json ~status:`Not_acceptable json)
  (* POST /set_strategy - Set strategy *)
  | `POST, "/set_strategy" ->
    let body_string = Cohttp_eio.Body.to_string body in
    let strategy_arg =
      try Yojson.Safe.from_string body_string |> Yojson.Safe.Util.to_string with
      | _ -> body_string (* fallback to raw string *)
    in
    let strategies_loaded = Longleaf_strategies.all_strategy_names in
    (match List.mem strategy_arg strategies_loaded with
    | true ->
      let cli = { Settings.settings.cli_vars with strategy_arg } in
      Settings.settings.cli_vars <- cli;
      `Assoc [ ("message", `String "settings.cli_vars.strategy_arg set") ]
      |> Yojson.Safe.to_string |> respond_json
    | false ->
      `Assoc [ ("error", `String "Could not find strategy in data directory") ]
      |> Yojson.Safe.to_string
      |> fun json -> respond_json ~status:`Not_acceptable json)
  (* POST /set_runtype - Set runtype *)
  | `POST, "/set_runtype" ->
    let body_string = Cohttp_eio.Body.to_string body in
    (try
       let r =
         Yojson.Safe.from_string body_string |> Core.Runtype.t_of_yojson
       in
       let cli = { Settings.settings.cli_vars with runtype = r } in
       Settings.settings.cli_vars <- cli;
       `Assoc [ ("message", `String "settings.cli_vars.strategy_arg set") ]
       |> Yojson.Safe.to_string |> respond_json
     with
    | _ ->
      `Assoc [ ("error", `String "Could not find strategy in data directory") ]
      |> Yojson.Safe.to_string
      |> fun json -> respond_json ~status:`Not_acceptable json)
  (* POST /set_cli - Set CLI vars *)
  | `POST, "/set_cli" ->
    let body_string = Cohttp_eio.Body.to_string body in
    (try
       let r =
         Yojson.Safe.from_string body_string |> Core.Options.CLI.t_of_yojson
       in
       Settings.settings.cli_vars <- r;
       `Assoc [ ("message", `String "settings.cli_vars._arg set") ]
       |> Yojson.Safe.to_string |> respond_json
     with
    | _ ->
      `Assoc [ ("error", `String "Could not find strategy in data directory") ]
      |> Yojson.Safe.to_string
      |> fun json -> respond_json ~status:`Not_acceptable json)
  (* Default - 404 *)
  | _ -> respond_404 ()

let () =
  Eio_main.run @@ fun env ->
  let port = 8080 in
  log "Starting server on http://localhost:%d" port;
  Cohttp_eio.Server.run ~port env (handler env);
  Eio.traceln "longleaf_server: exited";
  ()
