type runtype =
  | Live
  | Paper
  | Backtest

type t =
  {
    runtype : runtype;
    output_file : string option;
  }
