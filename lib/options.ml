module Runtype = struct
  type t = Live | Paper | Backtest | Manual | Multitest | Montecarlo
  [@@deriving show, eq]

  let of_string_res x =
    match x with
    | "Live" | "live" -> Ok Live
    | "Paper" | "paper" -> Ok Paper
    | "Backtest" | "backtest" -> Ok Backtest
    | "Manual" | "manual" -> Ok Manual
    | "Multitest" | "multitest" -> Ok Multitest
    | "Montecarlo" | "monte" | "montecarlo" -> Ok Multitest
    (* | "Listener" | "listener" -> Ok Listener *)
    (* | "Buyandhold" | "buyandhold" -> Ok BuyAndHold *)
    | _ -> Error (`Msg "Expected a valid runtype")

  let conv = Cmdliner.Arg.conv (of_string_res, pp)
  let is_manual = function Manual -> true | _ -> false
  let is_multitest = function Multitest -> true | _ -> false
end

module Preload = struct
  (* Start with empty bars, load bars from a file, or download data *)
  type t = None | File of string | Download [@@deriving show]

  let of_string_res x =
    match x with
    | "None" | "none" -> Ok None
    | "Download" | "download" -> Ok Download
    | s when Sys.file_exists s -> Ok (File s)
    | _ ->
        Error (`Msg "Expected a valid preload selection, or file doesn't exist")

  let conv = Cmdliner.Arg.conv (of_string_res, pp)
end

type t = {
  runtype : Runtype.t;
  preload : Preload.t;
  output_file : string option;
}
[@@deriving show]
