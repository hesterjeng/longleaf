(* Start with empty bars, load bars from a file, download data, or run a battery *)
type t =
  | Download
  | File of string
  | BatteryName of string
[@@deriving show, variants, yojson]

let of_string_res x =
  match x with
  | "Download" | "download" -> Ok Download
  | s when Sys.file_exists s -> Result.return @@ File s
  | s -> Result.return @@ BatteryName s

let conv : t Cmdliner.Arg.conv = Cmdliner.Arg.conv (of_string_res, pp)
