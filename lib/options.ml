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
  mutices : Longleaf_mutex.t;
}
[@@deriving show]
