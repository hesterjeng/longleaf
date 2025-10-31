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
let respond_json ?(status = `OK) json_string =
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  Cohttp_eio.Server.respond_string ~headers ~status ~body:json_string ()

let respond_html ?(status = `OK) html_string =
  let headers = Cohttp.Header.init_with "Content-Type" "text/html" in
  Cohttp_eio.Server.respond_string ~headers ~status ~body:html_string ()

let respond_text ?(status = `OK) text =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
  Cohttp_eio.Server.respond_string ~headers ~status ~body:text ()

let respond_404 () = respond_text ~status:`Not_found "404 Not Found"
let respond_500 message = respond_text ~status:`Internal_server_error message

(* Main request handler *)
let handler env _conn request body =
  let ( let* ) = Result.( let* ) in
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in

  Eio.traceln "%s %s" (Cohttp.Code.string_of_method meth) path;

  let result = match (meth, path) with
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
     |> Yojson.Safe.to_string |> respond_json)

  (* GET /status - Get current status *)
  | `GET, "/status" ->
    Result.return @@
    (Settings.yojson_of_status Settings.settings.status
     |> Yojson.Safe.to_string |> respond_json)

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
        |> Yojson.Safe.to_string |> respond_json
      | Some j -> Yojson.Safe.to_string j |> respond_json )
    in
    Result.return response

  (* GET /settings - Get current settings *)
  | `GET, "/settings" ->
    Result.return @@
    (Settings.yojson_of_t Settings.settings
     |> Yojson.Safe.to_string |> respond_json)

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
        |> fun json -> respond_json ~status:`Bad_request json
      | Some () ->
        `Assoc [ ("message", `String "shutdown mutex set") ]
        |> Yojson.Safe.to_string |> respond_json )
    in
    Result.return response

  (* GET /data - List available data files *)
  | `GET, "/data" ->
    Result.return @@
    (Bars.files ()
     |> List.filter (fun x -> not @@ String.is_empty x)
     |> List.map (fun x -> `String x)
     |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json)

  (* GET /options - Show configured options *)
  | `GET, "/options" ->
    Result.return @@
    (`Assoc [ ("message", `String "Show configured options") ]
     |> Yojson.Safe.to_string |> respond_json)

  (* GET /strategies - List all strategies *)
  | `GET, "/strategies" ->
    Result.return @@
    (List.map (fun x -> `String x) Longleaf_strategies.all_strategy_names
     |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json)

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
      Yojson.Safe.to_string strategy_info |> respond_json
    | None ->
      `Assoc
        [
          ( "error",
            `String (Printf.sprintf "Strategy '%s' not found" strategy_name) );
        ]
      |> Yojson.Safe.to_string |> respond_json
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
        `List [] |> Yojson.Safe.to_string |> respond_json
      | Some symbols ->
        (String.split ~by:"," symbols |> List.map @@ fun x -> `String x)
        |> fun x -> `List x |> Yojson.Safe.to_string |> respond_json )
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
        |> Yojson.Safe.to_string |> respond_json
      | Some j -> Yojson.Safe.to_string j |> respond_json )
    | _ -> respond_404 ()
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
    Result.return @@ respond_html ~status:`OK json_response

  (* GET / - Root endpoint, return settings *)
  | `GET, "/" ->
    Eio.traceln "=== GET / endpoint called ===";
    Eio.traceln "Current CLI settings:";
    Eio.traceln "  print_tick_arg: %b" Settings.settings.cli_vars.print_tick_arg;
    Eio.traceln "  strategy_arg: %s" Settings.settings.cli_vars.strategy_arg;
    let json = Settings.yojson_of_t Settings.settings |> Yojson.Safe.to_string in
    Eio.traceln "Returning JSON: %s" json;
    Result.return @@ respond_json json

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
     |> Yojson.Safe.to_string |> respond_json)

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
     |> Yojson.Safe.to_string |> respond_json)

  (* POST /set_strategy - Set strategy *)
  | `POST, "/set_strategy" ->
    let* body_string = Tools.read_body body in
    let strategy_arg =
      Yojson.Safe.from_string body_string |> Yojson.Safe.Util.to_string
    in
    let strategies_loaded = Longleaf_strategies.all_strategy_names in
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
     |> Yojson.Safe.to_string |> respond_json)

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
     |> Yojson.Safe.to_string |> respond_json)

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
     |> Yojson.Safe.to_string |> respond_json)

  (* Default - 404 *)
  | _ -> Result.return @@ respond_404 ()
  in
  match result with
  | Ok response -> response
  | Error e -> respond_500 (Error.show e)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let port = 8080 in
  Eio.traceln "Starting server on http://localhost:%d" port;
  Eio.traceln "Frontend available at http://localhost:3000 (by default)";
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true ~reuse_port:true
      (`Tcp (Eio.Net.Ipaddr.V4.any, port))
  in
  let server = Cohttp_eio.Server.make ~callback:(handler env) () in
  Cohttp_eio.Server.run socket server ~on_error:raise
