(* Start with empty bars, load bars from a file, or download data *)
type t =
  | Download
  | Loaded of Bars.t (* None | File of string | Download | Loaded of Bars.t *)
[@@deriving show, variants]

let of_string_res x =
  match x with
  (* | "None" *)
  (* | "none" -> *)
  (*   Ok None *)
  | "Download"
  | "download" ->
    Ok Download
  | s when Sys.file_exists s ->
    Eio.traceln "Loading bars from %s" s;
    let res = Bars.of_file s in
    Eio.traceln "Done loading bars";
    Result.return @@ Loaded res
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

let bars = function
  | Loaded b -> b
  | _ -> invalid_arg "Preload.bars: bars not loaded"
