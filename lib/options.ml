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
    | MultiRandomTickerBacktest
      (* Run multiple tests with ranomly generated target data. *)
  [@@deriving show, eq, yojson, variants]

  let all = List.map fst Variants.descriptions

  let of_string_res x =
    let j = `List [ `String x ] in
    try Result.return @@ t_of_yojson j
    with _ ->
      Result.fail
      @@ `Msg
           (Format.asprintf
              "@[Unknown runtype selected: %s@]@.@[Valid options are: %a@]@." x
              (List.pp String.pp) all)

  let conv = Cmdliner.Arg.conv (of_string_res, pp)
  let is_manual = function Manual -> true | _ -> false
  let is_multitest = function Multitest -> true | _ -> false
end

module Preload = struct
  (* Start with empty bars, load bars from a file, or download data *)
  type t = None | File of string | Download [@@deriving show]

  let of_string_res x =
    match x with
    | "None" | "none" -> Ok None
    | "Download" | "download" -> Ok Download
    | s when Sys.file_exists s -> Ok (File s)
    | _ ->
        Error (`Msg "Expected a valid preload selection, or file doesn't exist")

  let conv = Cmdliner.Arg.conv (of_string_res, pp)
end

module Context = struct
  type t = {
    strategy : string;
    runtype : Runtype.t;
    eio_env : Eio_unix.Stdenv.base; [@opaque]
    longleaf_env : Environment.t; [@opaque]
    switch : Eio.Switch.t; [@opaque]
    preload : Preload.t;
    target : string option;
    save_received : bool;
    nowait_market_open : bool;
    mutices : Longleaf_mutex.t;
    save_to_file : bool;
  }
  [@@deriving show]
end

type t = {
  symbols : string list;
  tick : float;
  overnight : bool;
  resume_after_liquidate : bool;
  indicators_config : Indicators.Config.t;
  dropout : bool;
  randomized_backtest_length : int;
  context : Context.t;
}
