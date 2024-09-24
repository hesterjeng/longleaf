module Log = (val Logs.src_log Logs.(Src.create "longleaf"))

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
  let vars = Environment.make () in
  Log.app (fun k -> k "%a" Environment.pp vars);
  let account = Api.get_account vars in
  Lwt_main.run account;
  (* Log.app (fun k -> k "Attempting to create dataframe from json"); *)
  Dataframe.of_json x
