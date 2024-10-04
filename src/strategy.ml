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
          let* response_json =
            create_market_order environment "AAPL" Side.Buy TimeInForce.Day
              OrderType.Market 1
          in
          Lwt.return response_json
      | false ->
          Log.app (fun k -> k "Shorting Apple!");
          let* response_json =
            create_market_order environment "AAPL" Side.Sell TimeInForce.Day
              OrderType.Market 1
          in
          Lwt.return response_json
    in
    Log.app (fun k -> k "%a" Yojson.Safe.pp response);
    Lwt.return_unit

  let rec top () =
    let* account = Input.account () in
    let* clock = Trading_api.Clock.get environment in
    Log.app (fun k -> k "%a" Trading_api.Clock.pp clock);
    Log.app (fun k -> k "%a" Trading_api.Accounts.pp account);
    let* () = if clock.is_open then do_something () else Lwt.return_unit in
    let* () = Lwt_unix.sleep 300.0 in
    top ()
end
