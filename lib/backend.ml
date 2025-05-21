module type S = Backend_intf.S
module type BACKEND_INPUT = Backend_intf.BACKEND_INPUT

let make_bars (options : Options.t) =
  let ( let* ) = Result.( let* ) in
  let context = options.context in
  let preload = context.preload in
  let target = context.target in
  let symbols = options.symbols in
  let* bars =
    match preload with
    | None -> Result.return @@ Bars.empty ()
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
      Result.return res
    | File file ->
      Eio.traceln "Preloading bars from %s" file;
      let* res = Yojson.Safe.from_file file |> Bars.t_of_yojson in
      Bars.sort Item.compare res;
      Result.return res
    | Loaded b ->
      let res = Bars.copy b in
      Bars.sort Item.compare res;
      Result.return res
  in
  let* target =
    (fun f ->
      match target with
      | None -> Ok None
      | File t -> f t
      | Download -> Error.missing_data "NYI Download target"
      | Loaded bars -> Result.return @@ Option.return @@ Bars.copy bars)
    @@ fun t ->
    let* res =
      let conv = Yojson.Safe.from_file t |> Bars.t_of_yojson in
      conv
    in
    Bars.sort (Ord.opp Item.compare) res;
    match context.runtype with
    | Options.Runtype.Montecarlo
    | MultiMontecarlo ->
      Result.return @@ Option.some
      @@ Monte_carlo.Bars.of_bars ~preload:bars ~target:res
    | _ -> Result.return @@ Some res
  in
  match context.runtype with
  | RandomSliceBacktest
  | MultiRandomSliceBacktest ->
    let bars, target = Slice_backtesting.top ~options bars target in
    Result.return @@ (bars, Some target)
  | Live
  | Manual
  | Paper
  | Backtest
  | Multitest
  | Montecarlo
  | MultiMontecarlo
  | RandomTickerBacktest
  | MultiRandomTickerBacktest
  | AstarSearch ->
    Result.return @@ (bars, target)

let make_backend_input (options : Options.t) bars target =
  let ( let* ) = Result.( let* ) in
  let* bars, target =
    match (bars, target) with
    | Some b, Some t -> Result.return @@ (Bars.copy b, Option.map Bars.copy t)
    | _ -> make_bars options
  in
  Result.return
  @@ (module struct
       let options = options
       let bars = bars
       let target = target
     end : BACKEND_INPUT)

let make (options : Options.t) bars target =
  let ( let* ) = Result.( let* ) in
  let* mod_ = make_backend_input options bars target in
  let module Input = (val mod_) in
  let* res =
    match options.context.runtype with
    | Manual -> Error.fatal "Cannot create a strategy with manual runtype"
    | Paper
    | Live ->
      Eio.traceln "@[create_backend: Creating Alpaca backend@]@.";
      Result.return @@ (module Alpaca_backend.Make (Input) : S)
    | Backtest
    | Multitest
    | Montecarlo
    | MultiMontecarlo
    | RandomSliceBacktest
    | RandomTickerBacktest
    | MultiRandomTickerBacktest
    | AstarSearch
    | MultiRandomSliceBacktest ->
      Eio.traceln "@[create_backend: Creating Backtesting backend@]@.";
      Result.return @@ (module Backtesting_backend.Make (Input) : S)
  in
  Eio.traceln "Finished creating backend";
  Result.return @@ res
