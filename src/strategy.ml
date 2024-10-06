module type INPUT = sig
  val account : unit -> Trading_api.Accounts.t Lwt.t
  val environment : Environment.t
end

module type STRATEGY = sig
  include INPUT

  val top : unit -> Cohttp.Code.status_code Lwt.t
end

module Get_account_strategy (Input : INPUT) : STRATEGY = struct
  module Log = (val Logs.src_log Logs.(Src.create "get-account-strategy"))
  open Lwt.Syntax
  open Trading_api.Orders
  open Trading_types
  include Input

  let () = Random.self_init ()

  let do_something () =
    let rand = Random.bool () in
    let* response =
      match rand with
      | true ->
          Log.app (fun k -> k "Going long on apple!");
          let order : Order.t =
            {
              symbol = "NVDA";
              side = Side.Buy;
              tif = TimeInForce.Day;
              order_type = OrderType.Market;
              qty = 1;
            }
          in
          let* response_json = create_market_order environment order in
          Lwt.return response_json
      | false ->
          Log.app (fun k -> k "Shorting Apple!");
          let order : Order.t =
            {
              symbol = "NVDA";
              side = Side.Sell;
              tif = TimeInForce.Day;
              order_type = OrderType.Market;
              qty = 1;
            }
          in
          let* response_json = create_market_order environment order in
          Lwt.return response_json
    in
    Log.app (fun k -> k "%a" Yojson.Safe.pp response);
    Lwt.return_unit

  let rec top () =
    let* account = Input.account () in
    let* clock = Trading_api.Clock.get environment in
    let* latest_quotes =
      Market_data_api.Stock.latest_quotes environment [ "NVDA"; "MSFT" ]
    in
    Log.app (fun k -> k "latest quotes:");
    Log.app (fun k -> k "%a" Yojson.Safe.pp latest_quotes);
    let* latest_bars =
      Market_data_api.Stock.latest_bars environment [ "NVDA"; "MSFT" ]
    in
    Log.app (fun k -> k "latest bars:");
    Log.app (fun k -> k "%a" Bars.pp latest_bars);
    let* hist_data =
      Market_data_api.Stock.historical_bars environment (Timeframe.min 10)
        ~start:(Time.of_ymd "2024-10-04") [ "NVDA"; "MSFT" ]
    in
    Log.app (fun k -> k "hist data bars:");
    Log.app (fun k -> k "%a" Bars.pp hist_data);
    Log.app (fun k -> k "%a" Trading_api.Clock.pp clock);
    Log.app (fun k -> k "%a" Trading_api.Accounts.pp account);
    let* () = if clock.is_open then do_something () else Lwt.return_unit in
    let* () = Lwt_unix.sleep 300.0 in
    top ()
end
