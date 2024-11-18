module Runtype = struct
  type t = Live | Paper | Backtest [@@deriving show]

  let of_string_res x =
    match x with
    | "Live" | "live" -> Ok Live
    | "Paper" | "paper" -> Ok Paper
    | "Backtest" | "backtest" -> Ok Backtest
    | _ -> Error (`Msg "Expected a valid runtype")

  let conv = Cmdliner.Arg.conv (of_string_res, pp)
end

type t = { runtype : Runtype.t; output_file : string option } [@@deriving show]
