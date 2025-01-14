type t = {
  apca_api_key_id : string;
  apca_api_secret_key : string;
<<<<<<< HEAD
  (* openai_key : string option; *)
  (* polygon_key : string option; *)
  tiingo_key : string option;
      (* apca_api_base_url : Uri.t; *)
      (* apca_api_data_url : Uri.t; *)
}
[@@deriving show]

let getenv_opt s =
  try Option.some @@ Unix.getenv s
  with Not_found ->
    Eio.traceln "error: Could not find environment variable %s" s;
    None
=======
  openai_key : string option;
  polygon_key : string option;
  tiingo_key : string option;
  apca_api_base_url : Uri.t;
  apca_api_data_url : Uri.t;
}
[@@deriving show]

let getenv_opt s = try Option.some @@ Unix.getenv s with Not_found -> None
>>>>>>> a13b8a6 (chore: src -> lib)

let make () =
  try
    let apca_api_key_id = Unix.getenv "APCA_API_KEY_ID" in
    let apca_api_secret_key = Unix.getenv "APCA_API_SECRET_KEY" in
<<<<<<< HEAD
    let tiingo_key = getenv_opt "TIINGO_KEY" in
    (* let apca_api_base_url = Unix.getenv "APCA_API_BASE_URL" |> Uri.of_string in *)
    (* let apca_api_data_url = Unix.getenv "APCA_API_DATA_URL" |> Uri.of_string in *)
    (* let openai_key = getenv_opt "OPENAI_KEY" in *)
    (* let polygon_key = getenv_opt "POLYGON_KEY" in *)
    {
      apca_api_key_id;
      apca_api_secret_key;
      (* apca_api_base_url; *)
      (* apca_api_data_url; *)
      (* openai_key; *)
      (* polygon_key; *)
=======
    let openai_key = getenv_opt "OPENAI_KEY" in
    let polygon_key = getenv_opt "POLYGON_KEY" in
    let tiingo_key = getenv_opt "TIINGO_KEY" in
    let apca_api_base_url = Unix.getenv "APCA_API_BASE_URL" |> Uri.of_string in
    let apca_api_data_url = Unix.getenv "APCA_API_DATA_URL" |> Uri.of_string in
    {
      apca_api_key_id;
      apca_api_secret_key;
      apca_api_base_url;
      apca_api_data_url;
      openai_key;
      polygon_key;
>>>>>>> a13b8a6 (chore: src -> lib)
      tiingo_key;
    }
  with Not_found ->
    Eio.traceln
      "@[Could not find an environment variable.  Is the .envrc configured?@]@.";
    exit 1
