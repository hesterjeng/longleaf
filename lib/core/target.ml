(* Start with empty bars, load bars from a file, or download data *)
type t =
  | Download
  | File of string (* Store file path for deferred loading *)
  | Loaded of string (* Placeholder - will be updated when bars are available *)
[@@deriving show, variants]

let of_string_res x =
  match x with
  | "Download"
  | "download" ->
    Ok Download
  | s when Sys.file_exists s ->
    (* Store file path for deferred loading *)
    Result.return @@ File s
  | _ ->
    Error (`Msg "Expected a valid preload selection, or file doesn't exist")

(* let pp_opt : t option Format.printer = *)
(*   fun fmt x -> *)
(*   Format.fprintf fmt "%a" (Option.pp pp) x *)

(* let of_string_opt_res x = *)
(*   match x with *)
(*   | Some s -> of_string_res s *)
(*   | None -> Error (`Msg "The target argument is required") *)

let conv = Cmdliner.Arg.conv (of_string_res, pp)

(* let load = function *)
(*   | None -> invalid_arg "Cannot load missing preload in Options.Preload.load" *)
(*   | File s -> *)
(*     let res = Bars.of_file s in *)
(*     Loaded res *)
(*   | Download -> invalid_arg "Cannot load download in Options.Preload.load" *)
(*   | Loaded b -> Loaded b *)

(* Functions to be implemented when bars library is available *)
let load_bars ~eio_env = function
  | Download -> invalid_arg "Target.load_bars: Download not yet implemented"
  | File file_path ->
    invalid_arg "Target.load_bars: Bars loading not yet implemented - needs bars library"
  | Loaded _ -> invalid_arg "Target.load_bars: Loaded case needs bars type"

let bars = function
  | Loaded _ -> invalid_arg "Target.bars: needs bars type"
  | File _ -> invalid_arg "Target.bars: bars not loaded"
  | Download -> invalid_arg "Target.bars: Download not supported"
