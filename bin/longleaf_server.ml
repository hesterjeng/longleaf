module Error = Core.Error
module Cmd = Cmdliner.Cmd
module CLI = Core.Options.CLI

[@@@warning "-26-32-69"]

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Settings = struct
  type t = {
    mutable cli_vars : Core.Options.CLI.t option;
    mutable target : Core.Target.t option;
  }
  [@@deriving show, yojson]

  let settings = { cli_vars = None; target = None }

  let get () =
    (Option.is_some settings.cli_vars && Option.is_some settings.target)
    |> function
    | true -> Result.return settings
    | false -> Error.fatal "CLI vars or target not initialized"
end

let get =
  [
    Dream.get "/static/**" @@ Dream.static "static";
    (Dream.get "/status" @@ fun _ -> Dream.html "Place status here...");
    ( Dream.get "/settings" @@ fun _ ->
      match Settings.get () with
      | Ok settings ->
        Settings.yojson_of_t settings |> Yojson.Safe.to_string |> Dream.json
      | Error e ->
        let s = Error.show e in
        Dream.respond ~status:`Internal_Server_Error s );
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
    (Dream.post "set_bars" @@ fun _ -> Dream.html "Set bars...");
    (Dream.post "set_options" @@ fun _ -> Dream.html "Set options...");
    (Dream.post "set_strategy" @@ fun _ -> Dream.html "Set strategy...");
  ]

let handler : Dream.handler = Dream.router @@ get @ post

let () =
  let doc = "C&C server for longleaf" in
  let info = Cmd.info ~doc "longleaf_server" in
  let res = Cmd.v info CLI.term in
  Format.printf "Running";
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop ~clock @@ fun () ->
  let _ = Lwt_eio.run_lwt @@ fun () -> Dream.serve @@ Dream.logger @@ handler in
  Eio.traceln "longleaf_server: exited";
  ()
