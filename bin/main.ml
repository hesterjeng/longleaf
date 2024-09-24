module Vars = struct
  type env_variables = {
    apca_api_key_id : string;
    apca_api_secret_key : string;
    apca_api_base_url : string;
  }

  let make () =
    let apca_api_key_id = Unix.getenv "APCA_API_KEY_ID" in
    let apca_api_secret_key = Unix.getenv "APCA_API_SECRET_KEY" in
    let apca_api_base_url = Unix.getenv "APCA_API_BASE_URL" in
    { apca_api_key_id; apca_api_secret_key; apca_api_base_url }
end

let () =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Py.initialize ();
  let file = "data/da.json" in
  let json = Yojson.Safe.from_file file in
  let _res = Longleaf.process_json json in
  exit (if Logs.err_count () > 0 then 1 else 0)
