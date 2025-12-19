module type S = Backend_intf.S
module type BACKEND_INPUT = Backend_intf.BACKEND_INPUT

module Tiingo_api = Longleaf_apis.Tiingo_api
module Market_data_api = Longleaf_apis.Market_data_api
module Bars = Longleaf_bars
module Indicators = Longleaf_indicators

let make_backend_input mutices (target : Bars.t option) (options : Options.t) =
  (module struct
    let options = options
    let mutices = mutices
    let target = target
  end : BACKEND_INPUT)

let make mutices (bars : Bars.t) (options : Options.t) =
  let ( let* ) = Result.( let* ) in

  (* Add custom indicators to the indicator config *)
  let backend_input = make_backend_input mutices (Some bars) options in
  let module Input = (val backend_input) in
  let* res =
    match options.flags.runtype with
    | Invalid -> Error.fatal "Cannot create a strategy with invalid backend"
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
    | MultiRandomSliceBacktest
    | Battery ->
      Eio.traceln "@[create_backend: Creating Backtesting backend@]@.";
      Result.return @@ (module Backtesting_backend.Make (Input) : S)
  in
  Eio.traceln "Finished creating backend";
  Result.return @@ res
