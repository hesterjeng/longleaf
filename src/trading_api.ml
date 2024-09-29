open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
module Log = (val Logs.src_log Logs.(Src.create "api"))
open Trading_types

(* Create the headers based on the current environment *)
let h (env : Environment.t) =
  Header.init () |> fun h ->
  Header.add h "APCA-API-KEY-ID" env.apca_api_key_id |> fun h ->
  Header.add h "APCA-API-SECRET-KEY" env.apca_api_secret_key

module Accounts = struct
  let get_account (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/account" in
    let headers = h env in
    let* resp, body = Client.get ~headers uri in
    let* body = Cohttp_lwt.Body.to_string body in
    let body_json = Yojson.Safe.from_string body in
    let status = Response.status resp |> Code.string_of_status in
    Log.app (fun k -> k "@[%s@]@.@[%a@]@." status Yojson.Safe.pp body_json);
    Lwt.return (status, body_json)
end

module Orders = struct
  let create_market_order (env : Environment.t) (symbol : string)
      (side : Side.t) (tif : TimeInForce.t) (order_type : OrderType.t)
      (qty : int) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    let body =
      Yojson.Safe.(
        `Assoc
          [
            ("symbol", `String symbol);
            ("type", `String (OrderType.to_string order_type));
            ("time_in_force", `String (TimeInForce.to_string tif));
            ("side", `String (Side.to_string side));
            ("qty", `String (Int.to_string qty));
          ])
      |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
    in
    let* response, body_stream = Client.post ~headers ~body uri in
    let* resp_body = Cohttp_lwt.Body.to_string body_stream in
    let status = Response.status response |> Code.string_of_status in
    Lwt.return (status, resp_body)

  let get_all_orders (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    let* resp, body = Client.get ~headers uri in
    let* body = Cohttp_lwt.Body.to_string body in
    let body_json = Yojson.Safe.from_string body in
    let status = Response.status resp |> Code.string_of_status in
    Log.app (fun k -> k "@[%s@]@.@[%a@]@." status Yojson.Safe.pp body_json);
    Lwt.return (status, body_json)

  let delete_all_orders (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    let* resp, body = Client.delete ~headers uri in
    let* body = Cohttp_lwt.Body.to_string body in
    let body_json = Yojson.Safe.from_string body in
    let status = Response.status resp |> Code.string_of_status in
    Log.app (fun k -> k "@[%s@]@.@[%a@]@." status Yojson.Safe.pp body_json);
    Lwt.return (status, body_json)
end
