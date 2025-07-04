module RunType = struct
  type t =
    | Live
    | Paper
    | Backtest
    | Manual
    | Multitest (* Run the strategy multiple times. *)
    | Montecarlo (* Run the test with randomly generated target data. *)
    | MultiMontecarlo
    | RandomSliceBacktest
    | MultiRandomSliceBacktest
    | RandomTickerBacktest
    | AstarSearch
    | MultiRandomTickerBacktest
      (* Run multiple tests with ranomly generated target data. *)
  [@@deriving show, eq, yojson, variants]

  let all = List.map fst Variants.descriptions

  let of_string_res x =
    let j = `List [ `String x ] in
    try Result.return @@ t_of_yojson j with
    | _ ->
      Result.fail
      @@ `Msg
           (Format.asprintf
              "@[Unknown runtype selected: %s@]@.@[Valid options are: %a@]@." x
              (List.pp String.pp) all)

  let conv = Cmdliner.Arg.conv (of_string_res, pp)

  let is_manual = function
    | Manual -> true
    | _ -> false

  let is_multitest = function
    | Multitest -> true
    | _ -> false
end

module CLI = struct
  type t = {
    runtype : RunType.t;
    stacktrace : bool;
    strategy_arg : string;
    no_gui : bool;
    save_received : bool;
    save_to_file : bool;
    nowait_market_open : bool;
    print_tick_arg : bool;
    precompute_indicators_arg : bool;
    compare_preloaded : bool;
    start : int;
  }
  [@@deriving show]

  type cli = t

  module Args = struct
    (* module Runtype = Longleaf.Options.RunType *)

    (* Define the CLI arguments *)
    let runtype_arg =
      let runtype_conv = RunType.conv in
      let doc =
        Format.asprintf "The type of run.  Valid choices are %a."
          (List.pp String.pp) RunType.all
      in
      Cmdliner.Arg.(
        required & pos 0 (some runtype_conv) None & info [] ~docv:"runtype" ~doc)

    (* let strategy_arg = *)
    (*   let doc = *)
    (*     Format.asprintf "The selected strategy.  Valid choices are %a." *)
    (*       (List.pp String.pp) Longleaf_strategies.all *)
    (*   in *)
    (*   Cmdliner.Arg.( *)
    (*     required *)
    (*     & pos 1 (some Longleaf_strategies.conv) None *)
    (*     & info [] ~docv:"strategy" ~doc) *)

    let strategy_arg =
      let doc = "The selected strategy" in
      Cmdliner.Arg.(
        required & pos 1 (some string) None & info [] ~docv:"strategy" ~doc)

    (* let preload_arg = *)
    (*   let preload_conv = Preload.conv in *)
    (*   let doc = *)
    (*     "The data used to \"warmup\" indicators.  This data should be just \ *)
  (*      before the target data.  Valid choices are \"none\", \"download\", or \ *)
  (*      \"%s\" where %s is the file you want preloaded as bars.  This data will \ *)
  (*      be in the background, as historical information.  If this value is \ *)
  (*      None, the strategy will run on the target data as if there were no \ *)
  (*      preloaded data.  If the argument is Download, an attempt to download \ *)
  (*      some market data will be made.  Otherwise, `--preload $file will` \ *)
  (*      attempt to use $file, which is expects to be a file in Alpaca market \ *)
  (*      data JSON format." *)
    (*   in *)
    (*   Cmdliner.Arg.(value & opt preload_conv None & info [ "p"; "preload" ] ~doc) *)

    let target_arg =
      let preload_conv = Target.conv in
      let doc =
        "The data file to actually backtest on.  This is only for use with \
         backtesting.  The algorithm will process this information as if it is \
         being received over the wire."
      in
      Cmdliner.Arg.(
        required & pos 2 (some preload_conv) None & info [] ~docv:"target" ~doc)
    (* Cmdliner.Arg.(value & opt preload_conv None & info [ "t"; "target" ] ~doc) *)

    let output_file_arg =
      let doc = "Output file for a log." in
      Cmdliner.Arg.(
        value & opt (some string) None & info [ "o"; "output" ] ~doc)

    let stacktrace_arg =
      let doc = "Print a stacktrace if an exception occurs." in
      Cmdliner.Arg.(value & flag & info [ "g" ] ~doc)

    let print_tick_arg =
      let doc = "Print the current tick." in
      Cmdliner.Arg.(value & flag & info [ "pt" ] ~doc)

    let no_gui_arg =
      let doc = "Disable the gui process." in
      Cmdliner.Arg.(value & flag & info [ "nogui" ] ~doc)

    let precompute_indicators_arg =
      let doc = "Precompute indicators." in
      Cmdliner.Arg.(value & flag & info [ "indicators" ] ~doc)

    let compare_preload =
      let doc = "Compare live with preloaded indicators" in
      Cmdliner.Arg.(value & flag & info [ "compare-preload" ] ~doc)

    let save_received_arg =
      let doc = "Save received data." in
      Cmdliner.Arg.(value & flag & info [ "sr"; "save-received" ] ~doc)

    let save_to_file =
      let doc = "Save data to files." in
      Cmdliner.Arg.(value & flag & info [ "s"; "save" ] ~doc)

    let nowait_market_open =
      let doc = "Don't wait for market open to try running the strategy." in
      Cmdliner.Arg.(value & flag & info [ "nowait-market-open" ] ~doc)

    let start_arg =
      let doc = "Starting index for backtest" in
      Cmdliner.Arg.(value & opt int 0 & info [ "i"; "index" ] ~doc)
  end

  let cli_term =
    let open Cmdliner.Term.Syntax in
    let+ runtype = Args.runtype_arg
    and+ stacktrace = Args.stacktrace_arg
    and+ no_gui = Args.no_gui_arg
    and+ save_received = Args.save_received_arg
    and+ strategy_arg = Args.strategy_arg
    and+ save_to_file = Args.save_to_file
    and+ nowait_market_open = Args.nowait_market_open
    and+ print_tick_arg = Args.print_tick_arg
    and+ precompute_indicators_arg = Args.precompute_indicators_arg
    and+ compare_preloaded = Args.compare_preload
    and+ start = Args.start_arg in
    {
      runtype;
      stacktrace;
      no_gui;
      save_received;
      strategy_arg;
      save_to_file;
      nowait_market_open;
      print_tick_arg;
      precompute_indicators_arg;
      compare_preloaded;
      start;
    }

  module Full = struct
    type t = { cli : cli; target : Target.t; output : string option }

    let term =
      let open Cmdliner.Term.Syntax in
      let+ cli = cli_term
      and+ target = Args.target_arg
      and+ output = Args.output_file_arg in
      { cli; target; output }
  end
end

type t = {
  symbols : string list;
  tick : float;
  indicators_config : Indicator_config.t;
  eio_env : Eio_unix.Stdenv.base; [@opaque]
  longleaf_env : Environment.t; [@opaque]
  switch : Eio.Switch.t; [@opaque]
  target : Target.t; [@opaque]
  flags : CLI.t;
  mutices : Server.Longleaf_mutex.t;
}
[@@deriving show]
