module type S = Backend_intf.S
module type BACKEND_INPUT = Backend_intf.BACKEND_INPUT

module SliceBacktesting = struct
  let select_midpoint length =
    assert (length >= 8000);
    let start_range = 1000 in
    let end_range = length - 1000 in
    Int.random_range start_range end_range Util.random_state

  (* let top target_length (module Input : BACKEND_INPUT) = *)
  let top ~(options : Options.t) bars target =
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

let make_bars (options : Options.t) =
  let context = options.context in
  let preload = context.preload in
  let target = context.target in
  let symbols = options.symbols in
  let bars =
    match preload with
    | None -> Bars.empty ()
    | Download ->
        Eio.traceln "Downloading data from tiingo for preload";
        let module Param = struct
          let longleaf_env = context.longleaf_env
          let client = Tiingo_api.tiingo_client context.eio_env context.switch
        end in
        let module Tiingo = Tiingo_api.Make (Param) in
        let request : Market_data_api.Request.t =
          let end_ = Time.get_todays_date () in
          let start = Time.subtract_30_days end_ in
          let tick_mins = Int.of_float (options.tick /. 60.0) in
          {
            timeframe = Trading_types.Timeframe.min tick_mins;
            symbols;
            start;
            end_ = Some end_;
          }
        in
        let res = Tiingo.Data.top request in
        Bars.sort Item.compare res;
        res
    | File file ->
        Eio.traceln "Preloading bars from %s" file;
        let res = Yojson.Safe.from_file file |> Bars.t_of_yojson in
        Bars.sort Item.compare res;
        res
  in
  let target =
    let ( let+ ) = Option.( let+ ) in
    let+ res =
      let+ target = target in
      Yojson.Safe.from_file target |> Bars.t_of_yojson
    in
    Bars.sort (Ord.opp Item.compare) res;
    match context.runtype with
    | Options.Runtype.Montecarlo | MultiMontecarlo ->
        Monte_carlo.Bars.of_bars ~preload:bars ~target:res
    | _ -> res
  in
  match context.runtype with
  | RandomSliceBacktest | MultiRandomSliceBacktest ->
      let bars, target = SliceBacktesting.top ~options bars target in

      let indicators =
        Indicators.initialize { fft = false } bars
      in
      (bars, Some target)
  | Live | Manual | Paper | Backtest | Multitest | Montecarlo | MultiMontecarlo
  | RandomTickerBacktest | MultiRandomTickerBacktest ->
      (bars, target)

let make_backend_input (options : Options.t) bars target =
  let bars, target =
    match (bars, target) with
    | Some b, Some t -> (Bars.Hashtbl.copy b, (Option.map Bars.Hashtbl.copy) t)
    | _ -> make_bars options

(* let make_indicators (options : Options.t) bars target = *)
(*   let x = Indicators.initialize Indicators.Config.{ fft = false } () in *)
  (* let bars, target = *)

  (*   match (bars, target) with *)
  (*   | Some b, Some t -> (Bars.Hashtbl.copy b, (Option.map Bars.Hashtbl.copy) t) *)
  (*   | _ -> make_bars options *)
  in
  (module struct
    let options = options
    let bars = bars
    let target = target
  end : BACKEND_INPUT)

let make (options : Options.t) bars target =
  let module Input = (val make_backend_input options bars target) in
  let res =
    match options.context.runtype with
    | Manual -> invalid_arg "Cannot create a strategy with manual runtype"
    | Paper | Live ->
        Eio.traceln "@[create_backend: Creating Alpaca backend@]@.";
        (module Alpaca_backend.Make (Input) : S)
    | Backtest | Multitest | Montecarlo | MultiMontecarlo | RandomSliceBacktest
    | RandomTickerBacktest | MultiRandomTickerBacktest
    | MultiRandomSliceBacktest ->
        Eio.traceln "@[create_backend: Creating Backtesting backend@]@.";
        (module Backtesting_backend.Make (Input))
  in
  Eio.traceln "Finished creating backend";
  res
