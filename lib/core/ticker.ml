let tick ~(runtype : Runtype.t) env time =
  match runtype with
  | Invalid
  | Backtest
  | Multitest
  | Manual
  | Montecarlo
  | MultiMontecarlo
  | RandomSliceBacktest
  | MultiRandomSliceBacktest
  | RandomTickerBacktest
  | MultiRandomTickerBacktest
  | Battery ->
    ()
  | Live
  | Paper ->
    Eio.Time.sleep env#clock time
