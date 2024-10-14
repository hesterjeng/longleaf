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

let download_test env =
  let open Lwt.Syntax in
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
  match historical_bars with Ok x -> Lwt.return x | Error e -> Lwt.fail_with e

let position_test env =
  let open Lwt.Syntax in
  let* position = Trading_api.Positions.get_all_open_positions env in
  match position with
  | Ok p ->
      Lwt.return
      @@ Log.app (fun k -> k "%a" Position.pp_alpaca_position_response p)
  | Error e -> invalid_arg e

let top () =
  try
    let open Lwt.Syntax in
    CalendarLib.Time_Zone.change (UTC_Plus (-5));
    let env = Environment.make () in
    (* let module Backend = State_machine.Alpaca_backend in *)
    let* bars = download_test env in
    let* () = position_test env in
    let module Backend = Backend.Backtesting (struct
      let bars = bars
    end) in
    let module Strategy = Strategies.SimpleStateMachine (Backend) in
    let* res = Strategy.run env in
    Log.app (fun k -> k "State machine shutdown:");
    Log.app (fun k -> k "%s" res);
    Lwt.return_unit
  with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, _) ->
    let err = Printexc.to_string e in
    invalid_arg @@ Format.asprintf "%s" err
