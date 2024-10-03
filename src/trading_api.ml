open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
module Log = (val Logs.src_log Logs.(Src.create "trading-api"))
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
    Util.get ~headers ~uri
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
    Util.post ~headers ~body ~uri

  let get_all_orders (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    Util.get ~headers ~uri

  let delete_all_orders (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    Util.delete ~headers ~uri

  let get_order_by_id (env : Environment.t) (id : OrderId.t) =
    let uri =
      Uri.with_path env.apca_api_base_url @@ "/v2/orders" |> fun u ->
      Uri.with_path u @@ OrderId.to_string id
    in
    let headers = h env in
    Util.get ~headers ~uri

  let delete_order_by_id (env : Environment.t) (id : OrderId.t) =
    let uri =
      Uri.with_path env.apca_api_base_url @@ "/v2/orders" |> fun u ->
      Uri.with_path u @@ OrderId.to_string id
    in
    let headers = h env in
    Util.delete ~headers ~uri
end
