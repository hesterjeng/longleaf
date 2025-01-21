let tick ~(runtype : Options.Runtype.t) env time =
  match runtype with
  | Backtest | Multitest | Manual | Montecarlo | MultiMontecarlo
  | RandomBacktest ->
      ()
  | Live | Paper -> Eio.Time.sleep env#clock time
