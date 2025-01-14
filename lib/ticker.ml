let tick ~(runtype : Options.Runtype.t) env time =
  match runtype with
<<<<<<< HEAD
  | Backtest | Multitest | Manual | Montecarlo | MultiMontecarlo
  | RandomSliceBacktest | MultiRandomSliceBacktest | RandomTickerBacktest
  | MultiRandomTickerBacktest ->
      ()
=======
  | Backtest | Manual -> ()
>>>>>>> a13b8a6 (chore: src -> lib)
  | Live | Paper -> Eio.Time.sleep env#clock time
