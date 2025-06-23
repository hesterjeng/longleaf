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
  type flags =
    {
      stacktrace : bool;
      no_gui : bool;
      save_received : bool;
      save_to_file : bool;
      nowait_market_open : bool;
      print_tick_arg : bool;
      precompute_indicators_arg : bool;
      compare_preloaded : bool;
      randomized_backtest_length : bool;
      start : int;
    } [@@deriving show]

  type 'a t = {
    runtype : RunType.t;
    target : Target.t;
    strategy_arg : 'a;
    flags : flags;
  }
  [@@deriving make]
end

module Context = struct
  type t = {
    strategy : string;
    runtype : RunType.t;
    (* indicators : Indicators.t; [@opaque] *)
    eio_env : Eio_unix.Stdenv.base; [@opaque]
    longleaf_env : Environment.t; [@opaque]
    switch : Eio.Switch.t; [@opaque]
    (* preload : Preload.t; [@opaque] *)
    target : Target.t; [@opaque]
    flags : CLI.flags;
    (* compare_preloaded : bool; *)
    (* save_received : bool; *)
    (* no_gui : bool; *)
    (* nowait_market_open : bool; *)
    mutices : Longleaf_mutex.t;
    (* save_to_file : bool; *)
    (* print_tick_arg : bool; *)
  }
  [@@deriving show]

  (* let load x = *)
  (*   { *)
  (*     x with *)
  (*     (\* preload = Loaded (Preload.load x.preload); *\) *)
  (*     target = Target.load x.target; *)
  (*   } *)
end

type t = {
  symbols : string list;
  tick : float;
  (* overnight : bool; *)
  (* resume_after_liquidate : bool; *)
  indicators_config : Indicator_config.t;
  (* dropout : bool; *)
  (* randomized_backtest_length : int; *)
  context : Context.t;
}

(* let load (x : t) = { x with context = Context.load x.context } *)
