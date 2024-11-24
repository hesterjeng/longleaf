module Runtype = struct
  type t = Live | Paper | Backtest | Manual [@@deriving show]

  let of_string_res x =
    match x with
    | "Live" | "live" -> Ok Live
    | "Paper" | "paper" -> Ok Paper
    | "Backtest" | "backtest" -> Ok Backtest
    | "Manual" | "manual" -> Ok Backtest
    | _ -> Error (`Msg "Expected a valid runtype")
  [@@deriving eq]

  let conv = Cmdliner.Arg.conv (of_string_res, pp)
  let is_manual = function Manual -> true | _ -> false
end

type t = { runtype : Runtype.t; output_file : string option } [@@deriving show]
