type t = {
  apca_api_key_id : string;
  apca_api_secret_key : string;
  apca_api_base_url : Uri.t;
}
[@@deriving show]

let make () =
  let apca_api_key_id = Unix.getenv "APCA_API_KEY_ID" in
  let apca_api_secret_key = Unix.getenv "APCA_API_SECRET_KEY" in
  let apca_api_base_url = Unix.getenv "APCA_API_BASE_URL" |> Uri.of_string in
  { apca_api_key_id; apca_api_secret_key; apca_api_base_url }
