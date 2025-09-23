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

let get env =
  [
    Dream.get "/static/**" @@ Dream.static "static";
    ( Dream.get "/execute" @@ fun _ ->
      let s = Settings.settings in
      s.status <- Started;
      try
        (Dream.log "running strategy";
         Lwt.async @@ fun () ->
         Lwt_eio.run_eio @@ fun () ->
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
           Settings.settings.status <- Error);
        Dream.respond ~status:`OK "strategy execution started"
      with
      | exn ->
        Dream.log "strategy execution failed with exception";
        s.status <- Error;
        Dream.respond ~status:`Internal_Server_Error
        @@ Printf.sprintf "Strategy execution failed: %s"
             (Printexc.to_string exn) );
    ( Dream.get "/status" @@ fun _ ->
      Settings.yojson_of_status Settings.settings.status
      |> Yojson.Safe.to_string |> Dream.json );
    ( Dream.get "/performance" @@ fun _ ->
      Settings.settings.mutices
      |> Option.map (fun (m : Longleaf_state.Mutex.t) ->
             Longleaf_util.Pmutex.get m.state_mutex)
      |> Option.map Longleaf_server__Plotly.performance_graph
      |> function
      | None ->
        Dream.respond ~status:`Bad_Request
          "Unable to get state performance history"
      | Some j -> Yojson.Safe.to_string j |> Dream.json );
    ( Dream.get "/settings" @@ fun _ ->
      Settings.yojson_of_t Settings.settings
      |> Yojson.Safe.to_string |> Dream.json );
    ( Dream.get "/shutdown" @@ fun _ ->
      Option.Infix.(
        let+ mutices = Settings.settings.mutices in
        Longleaf_util.Pmutex.set mutices.shutdown_mutex true)
      |> function
      | None ->
        Dream.respond ~status:`Bad_Request
          "Unable to get shutdown mutex at shutdown endpoint"
      | Some () -> Dream.respond ~status:`OK "shutdown mutex set" );
    ( Dream.get "/data" @@ fun _ ->
      Bars.files ()
      |> List.filter (fun x -> not @@ String.is_empty x)
      |> List.map (fun x -> `String x)
      |> fun x -> `List x |> Yojson.Safe.to_string |> Dream.json );
    (Dream.get "/options" @@ fun _ -> Dream.html "Show configured options");
    ( Dream.get "/strategies" @@ fun _ ->
      List.map (fun x -> `String x) Longleaf_strategies.all_strategy_names
      |> fun x -> `List x |> Yojson.Safe.to_string |> Dream.json );
    ( Dream.get "/symbols" @@ fun _ ->
      Option.Infix.(
        let* mutices = Settings.settings.mutices in
        let+ symbols = Longleaf_util.Pmutex.get mutices.symbols_mutex in
        Dream.log "returning symbols %s" symbols;
        symbols)
      |> function
      | None ->
        Dream.warning (fun log -> log "mutices not set (symbols)");
        `List [] |> Yojson.Safe.to_string |> Dream.json
      | Some symbols ->
        (String.split ~by:"," symbols |> List.map @@ fun x -> `String x)
        |> fun x -> `List x |> Yojson.Safe.to_string |> Dream.json );
    ( Dream.get "/data/:symbol/json" @@ fun request ->
      let symbol_str = Dream.param request "symbol" in
      let instrument = Instrument.of_string symbol_str in
      Option.Infix.(
        let* mutices = Settings.settings.mutices in
        Dream.log "symbol request %s" symbol_str;
        let state = Longleaf_util.Pmutex.get mutices.state_mutex in
        Longleaf_server__Plotly.of_state state instrument)
      |> function
      | None ->
        Dream.error (fun log ->
            log "failed to get mutices or plotly json for instrument");
        Dream.respond ~status:`Bad_Request "problem at data endpoint"
      | Some j -> Yojson.Safe.to_string j |> Dream.json );
    ( Dream.get "/tearsheet" @@ fun _ ->
      try
        Dream.log "generating tearsheet via QuantStats FastAPI service";

        (* Extract actual returns and dates from state/portfolio history *)
        let state_data =
          Option.Infix.(
            let* mutices = Settings.settings.mutices in
            Longleaf_util.Pmutex.get mutices.state_mutex
            |> Longleaf_state.Conv.to_tearsheet_json |> Option.return)
        in
        match state_data with
        | None ->
          Dream.respond ~status:`Bad_Request
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
          let ( let* ) = Lwt.Syntax.( let* ) in
          let* response =
            Lwt_eio.run_eio @@ fun () ->
            Eio.Switch.run @@ fun sw ->
            let client =
              match
                Piaf.Client.create ~sw env
                  (Uri.of_string "http://localhost:5000")
              with
              | Ok client -> client
              | Error e ->
                invalid_arg
                @@ Format.asprintf "Failed to create client: %a"
                     Piaf.Error.pp_hum e
            in
            let headers =
              Piaf.Headers.of_list [ ("Content-Type", "application/json") ]
            in
            let body = Yojson.Safe.from_string request in
            let resp =
              Longleaf_apis.Tools.post_piaf ~client ~body ~headers
                ~endpoint:"/tearsheet"
            in
            let body = Piaf.Response.body resp in
            match Piaf.Body.to_string body with
            | Ok html_content -> html_content
            | Error e ->
              invalid_arg
              @@ Format.asprintf "Failed to read response body: %a"
                   Piaf.Error.pp_hum e
          in
          Dream.respond ~status:`OK
            ~headers:[ ("Content-Type", "text/html") ]
            response
      with
      | exn ->
        Dream.log "tearsheet generation failed: %s" (Printexc.to_string exn);
        Dream.respond ~status:`Internal_Server_Error
        @@ Printf.sprintf "Tearsheet generation failed: %s"
             (Printexc.to_string exn) );
    ( Dream.get "/" @@ fun _ ->
      Settings.yojson_of_t Settings.settings
      |> Yojson.Safe.to_string |> Dream.json );
  ]

let post =
  let ( let* ) = Lwt.Syntax.( let* ) in
  [
    ( Dream.post "/set_status" @@ fun request ->
      try
        let* body = Dream.body request in
        Dream.log "%s" body;
        let status =
          Yojson.Safe.from_string body |> Settings.status_of_yojson
        in
        Settings.settings.status <- status;
        Dream.html "Set status..."
      with
      | e -> Dream.respond ~status:`Not_Acceptable @@ Printexc.to_string e );
    ( Dream.post "/set_target" @@ fun request ->
      let* target_str = Dream.body request in
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
      match target with
      | Ok target ->
        Settings.settings.target <- target;
        Dream.respond ~status:`OK "settings.cli_vars.strategy_arg set"
      | Error e -> Dream.respond ~status:`Not_Acceptable @@ Error.show e );
    ( Dream.post "/set_strategy" @@ fun request ->
      let* body = Dream.body request in
      let strategy_arg =
        try Yojson.Safe.from_string body |> Yojson.Safe.Util.to_string with
        | _ -> body (* fallback to raw string *)
      in
      let strategies_loaded = Longleaf_strategies.all_strategy_names in
      match List.mem strategy_arg strategies_loaded with
      | true ->
        let cli = { Settings.settings.cli_vars with strategy_arg } in
        Settings.settings.cli_vars <- cli;
        Dream.respond ~status:`OK "settings.cli_vars.strategy_arg set"
      | false ->
        Dream.respond ~status:`Not_Acceptable
          "Could not find strategy in data directory" );
    ( Dream.post "/set_runtype" @@ fun request ->
      let* body = Dream.body request in
      try
        let r = Yojson.Safe.from_string body |> Core.Runtype.t_of_yojson in
        let cli = { Settings.settings.cli_vars with runtype = r } in
        Settings.settings.cli_vars <- cli;
        Dream.respond ~status:`OK "settings.cli_vars.strategy_arg set"
      with
      | _ ->
        Dream.respond ~status:`Not_Acceptable
          "Could not find strategy in data directory" );
    ( Dream.post "/set_cli" @@ fun request ->
      let* body = Dream.body request in
      try
        let r = Yojson.Safe.from_string body |> Core.Options.CLI.t_of_yojson in
        Settings.settings.cli_vars <- r;
        Dream.respond ~status:`OK "settings.cli_vars._arg set"
      with
      | _ ->
        Dream.respond ~status:`Not_Acceptable
          "Could not find strategy in data directory" );
  ]

let handler env : Dream.handler = Dream.router @@ get env @ post

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop ~clock @@ fun () ->
  let _ =
    Lwt_eio.run_lwt @@ fun () -> Dream.serve @@ Dream.logger @@ handler env
  in
  Eio.traceln "longleaf_server: exited";
  ()
