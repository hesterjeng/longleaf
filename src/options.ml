type runtype = Live | Paper | Backtest [@@deriving show]
type t = { runtype : runtype; output_file : string option } [@@deriving show]

let runtype_of_string_res x =
  match x with
  | "Live" -> Ok Live
  | "Paper" -> Ok Paper
  | "Backtest" -> Ok Backtest
  | _ -> Error (`Msg "Expected a valid runtype")

let runtype_conv = Cmdliner.Arg.conv (runtype_of_string_res,pp_runtype)
