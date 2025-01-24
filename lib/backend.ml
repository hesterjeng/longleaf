module Run_options = Backend_intf.Run_options
module Run_context = Backend_intf.Run_context

module type S = Backend_intf.S
module type BACKEND_INPUT = Backend_intf.BACKEND_INPUT

module SliceBacktesting = struct
  let select_midpoint length =
    assert (length >= 8000);
    let start_range = 1000 in
    let end_range = length - 1000 in
    Int.random_range start_range end_range Util.random_state

  (* let top target_length (module Input : BACKEND_INPUT) = *)
  let top ~(options : Run_options.t) bars target =
    Eio.traceln "Selecting and creating random slice...";
    let preload = bars in
    let target =
      target |> Option.get_exn_or "Must have target for slice backtesting"
    in
    let combined = Bars.combine [ preload; target ] in
    let combined_length = Bars.length combined in
    let midpoint = select_midpoint combined_length in
    let new_bars, new_target =
      Bars.split ~midpoint ~target_length:options.randomized_backtest_length
        ~combined_length combined
    in
    Eio.traceln "Sorting random slices...";
    Bars.sort Item.compare new_bars;
    Bars.sort (Ord.opp Item.compare) new_target;
    Eio.traceln "Finished creating random slice...";
    (new_bars, new_target)
end

let make_bars ~(options : Run_options.t) ~(context : Run_context.t) =
  let preload = context.preload in
  let target = context.target in
  let bars =
    match preload with
    | None -> Bars.empty ()
    | Download -> invalid_arg "Downloading data for preload NYI"
    | File file ->
        Eio.traceln "Preloading bars from %s" file;
        let res = Yojson.Safe.from_file file |> Bars.t_of_yojson in
        Bars.sort Item.compare res;
        res
  in
  let target =
    let ( let+ ) = Option.( let+ ) in
    let+ res =
      target
      |> Option.map @@ fun f -> Yojson.Safe.from_file f |> Bars.t_of_yojson
    in
    Bars.sort (Ord.opp Item.compare) res;
    match options.runtype with
    | Options.Runtype.Montecarlo | MultiMontecarlo ->
        Monte_carlo.Bars.of_bars ~preload:bars ~target:res
    | _ -> res
  in
  match options.runtype with
  | RandomSliceBacktest | MultiRandomSliceBacktest ->
      let bars, target = SliceBacktesting.top ~options bars target in
      (bars, Some target)
  | Live | Manual | Paper | Backtest | Multitest | Montecarlo | MultiMontecarlo
    ->
      (bars, target)

let make_backend_input (options : Run_options.t) (context : Run_context.t) =
  let bars, target = make_bars ~options ~context in
  (module struct
    let switch = context.switch
    let longleaf_env = context.longleaf_env
    let eio_env = context.eio_env
    let save_received = context.save_received
    let save_to_file = context.save_to_file
    let mutices = context.mutices
    let symbols = options.symbols
    let overnight = options.overnight
    let resume_after_liquidate = options.resume_after_liquidate
    let tick = options.tick
    let runtype = options.runtype
    let indicators_config = options.indicators_config
    let dropout = options.dropout
    let bars = bars
    let target = target
  end : BACKEND_INPUT)

let make (options : Run_options.t) (context : Run_context.t) =
  let module Input = (val make_backend_input options context) in
  match options.runtype with
  | Live -> invalid_arg "Live trading not implemented"
  | Manual -> invalid_arg "Cannot create a strategy with manual runtype"
  | Paper ->
      Eio.traceln "@[create_backend: Creating Alpaca backend@]@.";
      (module Alpaca_backend.Make (Input) : S)
  | Backtest | Multitest | Montecarlo | MultiMontecarlo | RandomSliceBacktest
  | MultiRandomSliceBacktest ->
      Eio.traceln "@[create_backend: Creating Backtesting backend@]@.";
      (module Backtesting_backend.Make (Input))
