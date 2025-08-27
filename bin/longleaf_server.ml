module Error = Core.Error
module Cmd = Cmdliner.Cmd
module CLI = Core.Options.CLI
               module Instrument = Longleaf_core.Instrument

module Settings = struct
  type status = Ready | Started | Error [@@deriving show, yojson]

  type t = {
    mutable cli_vars : Core.Options.CLI.t;
    mutable target : Core.Target.t;
    mutable status : status;
  }
  [@@deriving show, yojson]

  let settings =
    { cli_vars = Core.Options.CLI.default; target = Download; status = Ready }
end


let get =
  [
    Dream.get "/static/**" @@ Dream.static "static";
    ( Dream.get "/execute" @@ fun _ ->
      let s = Settings.settings in
      s.status <- Started;
      let r = Longleaf_strategies.Run.top s.cli_vars s.target in
      match r with
      |
        Ok c ->
        (
      s.status <- Ready;
        Dream.json @@ Yojson.Safe.to_string @@ `Float c)
      | Error e ->
(
      s.status <- Error;
          Dream.respond ~status:`Not_Found @@ Error.show e)
    );
    ( Dream.get "/status" @@ fun _ ->
      Settings.yojson_of_status Settings.settings.status
      |> Yojson.Safe.to_string |> Dream.json );
    ( Dream.get "/settings" @@ fun _ ->
      Settings.yojson_of_t Settings.settings
      |> Yojson.Safe.to_string |> Dream.json );
    ( Dream.get "/data" @@ fun _ ->
      Bars.files () |> List.map (fun x -> `String x) |> fun x ->
      `List x |> Yojson.Safe.to_string |> Dream.json );
    (Dream.get "/options" @@ fun _ -> Dream.html "Show configured options");
    ( Dream.get "/strategies" @@ fun _ ->
      List.map (fun x -> `String x) Longleaf_strategies.all_strategy_names
      |> fun x -> `List x |> Yojson.Safe.to_string |> Dream.json );
    ( Dream.get "/symbols" @@ fun _ ->
      (* Get symbols from current target data *)
      match Settings.settings.target with
      | Download -> Dream.json @@ Yojson.Safe.to_string @@
        `List []
      | File _ ->
        (try
           invalid_arg "symbols endpoint nyi"
        with
        | e -> Dream.respond ~status:`Not_Found (Printf.sprintf "Error loading symbols: %s" (Printexc.to_string e))) );
    ( Dream.get "/data/:symbol/json" @@ fun request ->
      let symbol_str = Dream.param request "symbol" in
      try
        let _symbol = Instrument.of_string symbol_str in
        match Settings.settings.target with
        | Download ->
          Dream.respond ~status:`Not_Found "No data loaded for charting"
        | File _ ->
          Dream.respond ~status:`Not_Found symbol_str
          (* let bars = Bars.of_file filename in *)
          (* (\* Create a simple mock state for plotting - this is a simplified version *\) *)
          (* let mock_indicators_config = Indicators_config.{ tacaml_indicators = [] } in *)
          (* let mock_state = match State.make 0 bars () mock_indicators_config 10000.0 with *)
          (*   | Ok state -> state *)
          (*   | Error e -> failwith (Error.show e) *)
          (* in *)
          (* (match Longleaf_server__Plotly.of_state mock_state symbol with *)
          (* | Some json -> Dream.json (Yojson.Safe.to_string json) *)
          (* | None -> Dream.respond ~status:`Not_Found "Error generating chart") *)
      with
      | e -> Dream.respond ~status:`Bad_Request (Printf.sprintf "Error: %s" (Printexc.to_string e)) );
    ( Dream.get "/" @@ fun _ ->
      let html =
        Format.asprintf "%a" (Tyxml.Html.pp_elt ()) Longleaf_server__Index.page
      in
      Dream.html html );
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
          | _ -> Error.json @@"Problem converting target string" ^ target_str

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
        try let r = Yojson.Safe.from_string body
          |> Core.Runtype.t_of_yojson in
        let cli = { Settings.settings.cli_vars with runtype = r } in
        Settings.settings.cli_vars <- cli;
        Dream.respond ~status:`OK "settings.cli_vars.strategy_arg set"

        with
        | _ ->
        Dream.respond ~status:`Not_Acceptable
          "Could not find strategy in data directory"
    );
    ( Dream.post "/set_cli" @@ fun request ->
      let* body = Dream.body request in
        try let r = Yojson.Safe.from_string body
          |> Core.Options.CLI.t_of_yojson in
        Settings.settings.cli_vars <- r;
        Dream.respond ~status:`OK "settings.cli_vars._arg set"

        with
        | _ ->
        Dream.respond ~status:`Not_Acceptable
          "Could not find strategy in data directory"
    );
  ]

let handler : Dream.handler = Dream.router @@ get @ post

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop ~clock @@ fun () ->
  let _ = Lwt_eio.run_lwt @@ fun () -> Dream.serve @@ Dream.logger @@ handler in
  Eio.traceln "longleaf_server: exited";
  ()
