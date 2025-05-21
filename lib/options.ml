module Runtype = struct
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

module Preload = struct
  (* Start with empty bars, load bars from a file, or download data *)
  type t = None | File of string | Download | Loaded of Bars.t
  [@@deriving show]

  let of_string_res x =
    match x with
    | "None"
    | "none" ->
      Ok None
    | "Download"
    | "download" ->
      Ok Download
    | s when Sys.file_exists s -> Ok (File s)
    | _ ->
      Error (`Msg "Expected a valid preload selection, or file doesn't exist")

  let conv = Cmdliner.Arg.conv (of_string_res, pp)

  let load = function
    | None -> invalid_arg "Cannot load missing preload in Options.Preload.load"
    | File s -> Bars.of_file s
    | Download -> invalid_arg "Cannot load download in Options.Preload.load"
    | Loaded b -> b
end

module Context = struct
  type t = {
    strategy : string;
    runtype : Runtype.t;
    eio_env : Eio_unix.Stdenv.base; [@opaque]
    longleaf_env : Environment.t; [@opaque]
    switch : Eio.Switch.t; [@opaque]
    preload : Preload.t;
    target : Preload.t;
    save_received : bool;
    no_gui : bool;
    nowait_market_open : bool;
    mutices : Longleaf_mutex.t;
    save_to_file : bool;
    print_tick_arg : bool;
  }
  [@@deriving show]

  let load x =
    {
      x with
      preload = Loaded (Preload.load x.preload);
      target = Loaded (Preload.load x.target);
    }
end

type t = {
  symbols : string list;
  tick : float;
  overnight : bool;
  resume_after_liquidate : bool;
  indicators_config : Indicator_config.t;
  dropout : bool;
  randomized_backtest_length : int;
  context : Context.t;
}

let load (x : t) = { x with context = Context.load x.context }
