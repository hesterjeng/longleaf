module Error = Core.Error
module Cmd = Cmdliner.Cmd
module CLI = Core.Options.CLI

(* Shared types that will be available on both client and server *)
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

(* Eliom service definitions *)
let status_service = Eliom_service.create
  ~path:(Eliom_service.Path ["status"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let settings_service = Eliom_service.create
  ~path:(Eliom_service.Path ["settings"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let data_service = Eliom_service.create
  ~path:(Eliom_service.Path ["data"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let strategies_service = Eliom_service.create
  ~path:(Eliom_service.Path ["strategies"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let options_service = Eliom_service.create
  ~path:(Eliom_service.Path ["options"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let set_status_service = Eliom_service.create
  ~path:(Eliom_service.Path ["set_status"])
  ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
  ()

let set_target_service = Eliom_service.create
  ~path:(Eliom_service.Path ["set_target"])
  ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
  ()

let set_strategy_service = Eliom_service.create
  ~path:(Eliom_service.Path ["set_strategy"])
  ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
  ()

let home_service = Eliom_service.create
  ~path:(Eliom_service.Path [])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

(* Service handlers *)
let status_handler () () =
  let json_response = 
    Settings.yojson_of_status Settings.settings.status
    |> Yojson.Safe.to_string
  in
  Lwt.return (json_response, "application/json")

let settings_handler () () =
  let json_response = 
    Settings.yojson_of_t Settings.settings
    |> Yojson.Safe.to_string
  in
  Lwt.return (json_response, "application/json")

let data_handler () () =
  let json_response =
    Bars.files () |> List.map (fun x -> `String x) |> fun x ->
    `List x |> Yojson.Safe.to_string
  in
  Lwt.return (json_response, "application/json")

let strategies_handler () () =
  let json_response =
    List.map (fun x -> `String x) Longleaf_strategies.all_strategy_names
    |> fun x -> `List x |> Yojson.Safe.to_string
  in
  Lwt.return (json_response, "application/json")

let options_handler () () =
  Lwt.return ("Show configured options", "text/html")

let set_status_handler () raw_post_data =
  let%lwt body_string = Eliom_request_info.get_post_data raw_post_data in
  try
    let status =
      Yojson.Safe.from_string body_string |> Settings.status_of_yojson
    in
    Settings.settings.status <- status;
    Lwt.return ("Set status...", "text/html")
  with
  | e -> 
    let error_msg = Printexc.to_string e in
    Lwt.return (error_msg, "text/html")

let set_target_handler () raw_post_data =
  let%lwt target_str = Eliom_request_info.get_post_data raw_post_data in
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
    Lwt.return ("settings.cli_vars.strategy_arg set", "text/html")
  | Error e -> 
    Lwt.return (Error.show e, "text/html")

let set_strategy_handler () raw_post_data =
  let%lwt strategy_arg = Eliom_request_info.get_post_data raw_post_data in
  let strategies_loaded = Longleaf_strategies.all_strategy_names in
  match List.mem strategy_arg strategies_loaded with
  | true ->
    let cli = { Settings.settings.cli_vars with strategy_arg } in
    Settings.settings.cli_vars <- cli;
    Lwt.return ("settings.cli_vars.strategy_arg set", "text/html")
  | false ->
    Lwt.return ("Could not find strategy in data directory", "text/html")

let home_handler () () =
  let%lwt content = 
    let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input "static/index.html" in
    let%lwt content = Lwt_io.read ic in
    let%lwt () = Lwt_io.close ic in
    Lwt.return content
  in
  Lwt.return (content, "text/html")

(* Register services with handlers *)
let () =
  Eliom_registration.String.register ~service:status_service status_handler;
  Eliom_registration.String.register ~service:settings_service settings_handler;
  Eliom_registration.String.register ~service:data_service data_handler;
  Eliom_registration.String.register ~service:strategies_service strategies_handler;
  Eliom_registration.String.register ~service:options_service options_handler;
  Eliom_registration.String.register ~service:set_status_service set_status_handler;
  Eliom_registration.String.register ~service:set_target_service set_target_handler;
  Eliom_registration.String.register ~service:set_strategy_service set_strategy_handler;
  Eliom_registration.String.register ~service:home_service home_handler

(* Static file service for assets *)
let static_service = Eliom_service.create_attached
  ~fallback:home_service
  ~get_params:Eliom_parameter.(suffix (all_suffix "path"))
  ()

let static_handler path () =
  let file_path = String.concat "/" ("static" :: path) in
  if Sys.file_exists file_path then
    let%lwt content = 
      let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input file_path in
      let%lwt content = Lwt_io.read ic in
      let%lwt () = Lwt_io.close ic in
      Lwt.return content
    in
    let content_type = 
      if String.ends_with file_path ".js" then "application/javascript"
      else if String.ends_with file_path ".css" then "text/css"
      else if String.ends_with file_path ".html" then "text/html"
      else if String.ends_with file_path ".png" then "image/png"
      else "application/octet-stream"
    in
    Lwt.return (content, content_type)
  else
    Lwt.fail (Failure "File not found")

let () =
  Eliom_registration.String.register ~service:static_service static_handler

(* Main server startup with Eio/Lwt bridge *)
let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Lwt_eio.with_event_loop ~clock @@ fun () ->
  let server_config = Ocsigen_server.{ 
    default_config with 
    default_port = 8080;
    command_pipe = None;
  } in
  let _ = Lwt_eio.run_lwt @@ fun () -> 
    Ocsigen_server.start server_config
  in
  Eio.traceln "longleaf_eliom: exited";
  ()