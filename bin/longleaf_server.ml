module Error = Core.Error
module Cmd = Cmdliner.Cmd
module CLI = Core.Options.CLI

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
    ( Dream.get "/" @@ fun _ ->
      let html =
        Format.asprintf "%a" (Tyxml.Html.pp_elt ()) Longleaf_server__Index.page
      in
      Dream.html html );
  ]

let post =
  let ( let* ) = Lwt.Syntax.( let* ) in
  [
    (Dream.post "/set_options" @@ fun _ -> Dream.html "Set options...");
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
          | _ -> Error.json "Problem converting target string"
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
        try Yojson.Safe.from_string body |> Yojson.Safe.Util.to_string
        with _ -> body (* fallback to raw string *)
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
  ]

let handler : Dream.handler = Dream.router @@ get @ post

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop ~clock @@ fun () ->
  let _ = Lwt_eio.run_lwt @@ fun () -> Dream.serve @@ Dream.logger @@ handler in
  Eio.traceln "longleaf_server: exited";
  ()
