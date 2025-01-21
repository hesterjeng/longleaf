let tick ~(runtype : Options.Runtype.t) env time =
  match runtype with
  | Backtest | Multitest | Manual | Montecarlo | MultiMontecarlo
  | RandomSliceBacktest | MultiRandomSliceBacktest ->
      ()
  | Live | Paper -> Eio.Time.sleep env#clock time
