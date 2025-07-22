let tick ~(runtype : Options.RunType.t) env time =
  match runtype with
  | Backtest
  | Multitest
  | Manual
  | Montecarlo
  | MultiMontecarlo
  | RandomSliceBacktest
  | MultiRandomSliceBacktest
  | RandomTickerBacktest
  | MultiRandomTickerBacktest
  | AstarSearch ->
    ()
  | Live
  | Paper ->
    Eio.Time.sleep env#clock time
