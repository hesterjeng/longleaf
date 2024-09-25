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
  let env = Environment.make () in
  Log.app (fun k -> k "%a" Environment.pp env);
  let _ = Api.get_account env in
  let status, resp_body =
    Lwt_main.run
    @@ Api.create_market_order env "AAPL" Api.Side.Buy Api.TIF.Opening
         Api.OT.Market 1
  in
  Log.app (fun k -> k "status: %s" status);
  Log.app (fun k -> k "resp_body: %s" resp_body);
  Dataframe.of_json x
