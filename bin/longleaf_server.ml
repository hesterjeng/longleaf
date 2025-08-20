module Error = Core.Error
module Cmd = Cmdliner.Cmd
module CLI = Core.Options.CLI

[@@@warning "-26-32-69"]

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Settings = struct
  type t = {
    mutable cli_vars : Core.Options.CLI.t;
    mutable target : Core.Target.t option;
  }
  [@@deriving show, yojson]

  let settings = { cli_vars = Core.Options.CLI.default; target = None }
end

let get =
  [
    Dream.get "/static/**" @@ Dream.static "static";
    (Dream.get "/status" @@ fun _ -> Dream.html "Place status here...");
    ( Dream.get "/settings" @@ fun _ ->
      Settings.yojson_of_t Settings.settings
      |> Yojson.Safe.to_string |> Dream.json );
    ( Dream.get "/data" @@ fun _ ->
      Sys.readdir "data" |> Array.to_list
      |> List.filter (fun x -> String.equal (Filename.extension x) ".json")
      |> List.map (fun x -> `String x)
      |> fun x -> `List x |> Yojson.Safe.to_string |> Dream.json );
    (Dream.get "/options" @@ fun _ -> Dream.html "Show configured options");
    ( Dream.get "/strategies" @@ fun _ ->
      List.map (fun x -> `String x) Longleaf_strategies.all_strategy_names
      |> fun x -> `List x |> Yojson.Safe.to_string |> Dream.json );
    Dream.get "/" @@ Dream.from_filesystem "static" "index.html";
  ]

let post =
  [
    (Dream.post "set_target" @@ fun _ -> Dream.html "Set bars...");
    (Dream.post "set_options" @@ fun _ -> Dream.html "Set options...");
    ( Dream.post "set_strategy" @@ fun request ->
      let ( let* ) = Lwt.Syntax.( let* ) in
      let* json_str = Dream.body request in
      let strategy_arg =
        Yojson.Safe.from_string json_str |> Yojson.Safe.Util.to_string
      in
      let strategies_loaded = Longleaf_strategies.all_strategy_names in
      let cli = { Settings.settings.cli_vars with strategy_arg } in
      Settings.settings.cli_vars <- cli;
      Dream.respond ~status:`OK "settings.cli_vars.strategy_arg set" );
  ]

let handler : Dream.handler = Dream.router @@ get @ post

let () =
  let doc = "C&C server for longleaf" in
  let info = Cmd.info ~doc "longleaf_server" in
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop ~clock @@ fun () ->
  let _ = Lwt_eio.run_lwt @@ fun () -> Dream.serve @@ Dream.logger @@ handler in
  Eio.traceln "longleaf_server: exited";
  ()
