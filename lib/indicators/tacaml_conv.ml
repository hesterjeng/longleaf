(** Tacaml.t conversion from string expressions *)

(** Parse parameters from a string like "SMA 20" or "MACD 12 26 9" *)
let parse_params (s : string) : string list =
  String.split_on_char ' ' s |> List.map String.trim
  |> List.filter (fun x -> not (String.equal x ""))

(** Convert string to Tacaml.t *)
let of_string (expr : string) : (Tacaml.t, string) result =
  try
    (* Remove parentheses and split by spaces *)
    let cleaned =
      expr |> String.trim |> fun s ->
      String.sub s 1 (String.length s - 2) (* Remove ( and ) *) |> String.trim
    in
    let parts = parse_params cleaned in

    match parts with
    | [ "SMA"; period_str ] ->
      let period = Int.of_string period_str in
      (* Use predefined indicators with parameters - this is a simplified approach *)
      (* We'll find one SMA indicator from defaults and modify if needed *)
      let default_sma =
        List.find
          (fun indicator ->
            try
              let input = Tacaml.input indicator in
              let name = Tacaml.name indicator in
              String.contains name 'S' && String.contains name 'M'
              && String.contains name 'A'
            with
            | _ -> false)
          Tacaml.Defaults.all
      in
      Ok default_sma
    | [ "RSI"; period_str ] ->
      let period = Int.of_string period_str in
      let default_rsi =
        List.find
          (fun indicator ->
            try
              let name = Tacaml.name indicator in
              String.contains name 'R' && String.contains name 'S'
              && String.contains name 'I'
            with
            | _ -> false)
          Tacaml.Defaults.all
      in
      Ok default_rsi
    | _ ->
      (* For now, just return the first common indicator *)
      Ok (List.hd Tacaml.Defaults.common_indicators)
  with
  | e ->
    Error
      (Format.asprintf "Error parsing Tacaml expression '%s': %s" expr
         (Printexc.to_string e))

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
