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

(* let process_json (x : Yojson.Safe.t) = *)
(*   let env = Environment.make () in *)
(*   Log.app (fun k -> k "%a" Environment.pp env); *)
(*   let _ = Trading_api.Accounts.get_account env in *)
(*   let resp_body = *)
(*     Trading_types.( *)
(*       Lwt_main.run *)
(*       @@ Market_data_api.Stock.historical_bars env (Timeframe.hour 1) *)
(*            ~start:(Time.of_ymd "2024-08-28") [ "AAPL" ]) *)
(*   in *)
(*   Log.app (fun k -> k "resp_body longleaf: %a" Trading_types.Bars.pp resp_body); *)
(*   Dataframe.of_json x *)

let top () =
  (* Using state machine *)
  let open Lwt.Syntax in
  let env = Environment.make () in
  let* bars =
    (* Log.app (fun k -> k "Data display"); *)
    (* let* latest_bars = *)
    (*   Market_data_api.Stock.latest_bars env [ "AAPL"; "NVDA" ] *)
    (* in *)
    let history_request : Market_data_api.Stock.Historical_bars_request.t =
      {
        timeframe = Trading_types.Timeframe.day;
        start = Time.of_ymd "2012-06-06";
        symbols = [ "MSFT"; "GOOG"; "NVDA"; "AAPL" ];
      }
    in
    let* historical_bars =
      Market_data_api.Stock.historical_bars env history_request
    in
    match historical_bars with
    | Ok x -> Lwt.return x
    | Error e -> Lwt.fail_with e
    (* Log.app (fun k -> k "latest_bars:"); *)
    (* Log.app (fun k -> k "%a" Trading_types.Bars.pp latest_bars); *)
    (* Log.app (fun k -> k "historical_bars:"); *)
    (* Log.app (fun k -> k "%a" Trading_types.Bars.pp historical_bars); *)
    (* Log.app (fun k -> k "Data display finished"); *)
  in
  let module Backend = State_machine.Backtesting_backend (struct
    let bars = bars
  end) in
  let module Strategy = State_machine.SimpleStateMachine (Backend) in
  (* let module Strategy = *)
  (*   State_machine.SimpleStateMachine (State_machine.Alpaca_backend) in *)
  let* res = Strategy.run env in
  Log.app (fun k -> k "State machine shutdown:");
  Log.app (fun k -> k "%s" res);
  Lwt.return_unit
