(* Start with empty bars, load bars from a file, or download data *)
type t = Download | File of string
(* Store file path for deferred loading *)
(* | Loaded of 'a (\* Placeholder - will be updated when bars are available *\) *)
[@@deriving show, variants]

let of_string_res =
 fun x ->
  match x with
  | "Download"
  | "download" ->
    Ok Download
  | s when Sys.file_exists s ->
    (* Store file path for deferred loading *)
    Result.return @@ File s
  | _ ->
    Error (`Msg "Expected a valid preload selection, or file doesn't exist")

let conv : t Cmdliner.Arg.conv = Cmdliner.Arg.conv (of_string_res, pp)
