let tick ~(runtype : Options.Runtype.t) env time =
  match runtype with
  | Backtest | Multitest | Manual -> ()
  | Live | Paper -> Eio.Time.sleep env#clock time
