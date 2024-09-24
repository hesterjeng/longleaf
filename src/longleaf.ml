module Log = (val Logs.src_log Logs.(Src.create "longleaf"))

module Vars = struct
  type t = {
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

module Tickers = struct
  let get_tickers () =
    let open Pyops in
    let sp600_url =
      {|https://en.wikipedia.org/wiki/List_of_S%26P_600_companies|}
      |> Py.String.of_string
    in
    let pandas = Py.import "pandas" in
    let read_html = pandas.&("read_html") in
    let tables = read_html [| sp600_url |] in
    let col = tables.![Py.Int.of_int 0] in
    let ticker_symbols = col.!$["Symbol"] in
    Py.List.to_array_map Py.Object.to_string ticker_symbols
end

let process_json (x : Yojson.Safe.t) =
  let vars = Vars.make () in
  Log.app (fun k -> k "Attempting to create dataframe from json");
  Dataframe.of_json x
