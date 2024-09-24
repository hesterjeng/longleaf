open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
module Log = (val Logs.src_log Logs.(Src.create "api"))

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
  Lwt.return_unit

let create_market_order (env : Environment.t) =
  let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
  let headers =
    Header.init () |> fun h ->
    Header.add h "APCA-API-KEY-ID" env.apca_api_key_id |> fun h ->
    Header.add h "APCA-API-SECRET-KEY" env.apca_api_secret_key
  in
  Client.post
