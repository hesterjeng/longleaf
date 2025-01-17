let tick ~(runtype : Options.Runtype.t) env time =
  match runtype with
  | Backtest | Multitest | Manual | Montecarlo -> ()
  | Live | Paper -> Eio.Time.sleep env#clock time
