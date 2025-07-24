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
  | Invalid (* Run multiple tests with ranomly generated target data. *)
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

let real = function
  | Live
  | Paper ->
    true
  | _ -> false

let is_manual = function
  | Manual -> true
  | _ -> false

let is_multitest = function
  | Multitest -> true
  | _ -> false
