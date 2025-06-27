open Longleaf_lib

let mk_options switch eio_env flags target : Options.t =
  let longleaf_env = Environment.make () in
  let mutices = Longleaf_mutex.create () in
  {
    symbols = Longleaf_strategies.Collections.sp100;
    eio_env;
    longleaf_env;
    switch;
    flags;
    tick = 600.0;
    indicators_config = Indicator_config.default;
    target;
    mutices;
  }

(* type cli_args = Longleaf_strategies.t Options.CLI.t *)

let top ~eio_env (flags : Options.CLI.t) target =
  let domain_manager = Eio.Stdenv.domain_mgr eio_env in
  let mutices = Longleaf_mutex.create () in
  let run_strategy () =
    Eio.Domain_manager.run domain_manager @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    let options = mk_options switch eio_env flags target in
    let _res = Longleaf_strategies.run options strategy_arg in
    ()
  in
  let run_server () =
    match flags.no_gui with
    | true -> ()
    | false ->
      Eio.Domain_manager.run domain_manager @@ fun () ->
      Server.top ~mutices eio_env
  in
  let _ = Eio.Fiber.both run_strategy run_server in
  ()
