(** Tacaml.t conversion from string expressions *)

(** Parse parameters from a string like "SMA 20" or "MACD 12 26 9" *)
let parse_params (s : string) : string list =
  String.split_on_char ' ' s |> List.map String.trim
  |> List.filter (fun x -> not (String.equal x ""))

(** Convert Tacaml.t to a human-readable string representation *)
let to_string_representation (tacaml : Tacaml.t) : string =
  (* This is challenging since Tacaml.t are functions, 
     but we can try to extract some info if available *)
  try Format.asprintf "%a" Tacaml.pp tacaml with
  | _ -> "Custom Indicator"

(** Get indicator name from string expression *)
let get_indicator_name (expr : string) : string =
  try
    let cleaned =
      expr |> String.trim |> fun s ->
      String.sub s 1 (String.length s - 2) |> String.trim
    in
    let parts = parse_params cleaned in
    match parts with
    | [] -> "Unknown"
    | name :: _ -> name
  with
  | _ -> "Unknown"
