module Options = Longleaf_core.Options
module Cmd = Cmdliner.Cmd

let top () =
  let doc =
    Format.asprintf
      "@[This is the OCaml algorithmic trading platform longleaf.  It relies \
       on having a backend instantiated, with an appropriate.  The user must \
       select a backend and a strategy to run.  For an example of how to \
       create a new strategy, look at template_example.ml.  In \
       longleaf_strategies.ml, you can instantiate the functors to create your \
       strategy.  Valid strategies include:@]@.@.@[%a]@."
      (List.pp String.pp)
      (Longleaf_strategies.all_strategy_names ())
  in
  let info = Cmd.info ~doc "longleaf" in
  let res = Cmd.v info Options.CLI.Full.term in
  let value = Cmd.eval_value' res in
  match value with
  | `Ok s -> Result.return s
  | `Exit e -> Result.fail e

let print_res fmt x =
  match x with
  | Ok _ -> ()
  | Error e -> Format.fprintf fmt "%a" Longleaf_strategies.Error.pp e

let () =
  (* Enable exception backtraces for debugging *)
  Printexc.record_backtrace true;
  match top () with
  | Error e ->
    Eio.traceln "Exiting with code %d" e;
    Stdlib.exit e
  | Ok s ->
    Fmt_tty.setup_std_outputs ();
    Longleaf_util.handle_output s.output;
    Longleaf_indicators.Indicators.initialize ();
    Eio_main.run @@ fun env ->
    let f = Longleaf_strategies.Run.top None env s.cli s.target in
    Eio.traceln "%a" print_res f;
    Stdlib.exit Cmd.Exit.ok
