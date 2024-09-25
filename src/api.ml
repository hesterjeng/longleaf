open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
module Log = (val Logs.src_log Logs.(Src.create "api"))

module TIF = struct
  type t =
    | Day
    | GoodTillCanceled
    | Opening
    | Close
    | ImmediateOrCancel
    | FillOrKill

  let to_string = function
    | Day -> "day"
    | GoodTillCanceled -> "gtc"
    | Opening -> "opg"
    | Close -> "cls"
    | ImmediateOrCancel -> "ioc"
    | FillOrKill -> "fok"
end

module Side = struct
  type t = Buy | Sell

  let to_string = function Buy -> "buy" | Sell -> "sell"
end

module OT = struct
  type t = Market | Limit | Stop | StopLimit | TrailingStop

  let to_string = function
    | Market -> "market"
    | Limit -> "limit"
    | Stop -> "stop"
    | StopLimit -> "stop_limit"
    | TrailingStop -> "trailing_stop"
end

let get_account (env : Environment.t) =
  let uri = Uri.with_path env.apca_api_base_url "/v2/account" in
  let headers =
    Header.init () |> fun h ->
    Header.add h "APCA-API-KEY-ID" env.apca_api_key_id |> fun h ->
    Header.add h "APCA-API-SECRET-KEY" env.apca_api_secret_key
  in
  let* resp, body = Client.get ~headers uri in
  let* body = Cohttp_lwt.Body.to_string body in
  let body_json = Yojson.Safe.from_string body in
  let status = Response.status resp |> Code.string_of_status in
  Log.app (fun k -> k "@[%s@]@.@[%a@]@." status Yojson.Safe.pp body_json);
  Lwt.return (status, body_json)

let create_market_order (env : Environment.t) (symbol : string) (side : Side.t)
    (tif : TIF.t) (order_type : OT.t) (qty : int) =
  let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
  let headers =
    Header.init () |> fun h ->
    Header.add h "APCA-API-KEY-ID" env.apca_api_key_id |> fun h ->
    Header.add h "APCA-API-SECRET-KEY" env.apca_api_secret_key
  in
  let body =
    Yojson.Safe.(
      `Assoc
        [
          ("symbol", `String symbol);
          ("type", `String (OT.to_string order_type));
          ("time_in_force", `String (TIF.to_string tif));
          ("side", `String (Side.to_string side));
          ("qty", `String (Int.to_string qty));
        ])
    |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
  in
  let* response, body_stream = Client.post ~headers ~body uri in
  let* resp_body = Cohttp_lwt.Body.to_string body_stream in
  let status = Response.status response |> Code.string_of_status in
  Lwt.return (status, resp_body)
