module Options = Longleaf_core.Options
module Cmd = Cmdliner.Cmd

let top () =
  let doc =
    "This is the OCaml algorithmic trading platform longleaf.  It relies on \
     having a backend instantiated, with an appropriate.  The user must select \
     a backend and a strategy to run.  For an example of how to create a new \
     strategy, look at template_example.ml.  In longleaf_strategies.ml, you \
     can instantiate the functors to create your strategy."
  in
  let info = Cmd.info ~doc "longleaf" in
  let res = Cmd.v info Options.CLI.Full.term in
  let value = Cmd.eval_value' res in
  match value with
  | `Ok s -> Result.return s
  | `Exit e -> Result.fail e

let () =
  match top () with
  | Error e -> Stdlib.exit e
  | Ok s ->
    Fmt_tty.setup_std_outputs ();
    Longleaf_util.handle_output s.output;
    Longleaf_indicators.Indicators.initialize ();
    let _ = Longleaf_strategies.Run.top s.cli s.target in
    Stdlib.exit Cmd.Exit.ok
