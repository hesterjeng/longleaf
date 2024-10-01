open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
module Log = (val Logs.src_log Logs.(Src.create "data-api"))
open Trading_types

let h = Trading_api.h

module Stock = struct
  let historical_auctions (env : Environment.t) (symbols : string list) =
    let headers = h env in
    let symbols = String.concat "," symbols in
    let uri = Uri.with_path env.apca_api_data_url "/v2/stocks/auctions" in
    let body =
      Yojson.Safe.(`Assoc [ ("symbols", `String symbols) ])
      |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
    in
    let* response, body_stream = Client.post ~headers ~body uri in
    let* resp_body = Cohttp_lwt.Body.to_string body_stream in
    let status = Response.status response |> Code.string_of_status in
    Lwt.return (status, resp_body)

  let historical_bars (env : Environment.t) (timeframe : Timeframe.t)
      ~(start : Time.t) (* ~(end_ : Time.t) *)
                          (symbols : string list) =
    let headers = h env in
    let symbols = String.concat "," symbols in
    let uri =
      Uri.with_path env.apca_api_data_url "/v2/stocks/bars" |> fun u ->
      Uri.add_query_param' u ("symbols", symbols) |> fun u ->
      Uri.add_query_param' u ("timeframe", Timeframe.to_string timeframe)
      |> fun u -> Uri.add_query_param' u ("start", Ptime.to_rfc3339 start)
    in
    let* response, body_stream = Client.get ~headers uri in
    let* resp_body = Cohttp_lwt.Body.to_string body_stream in
    let resp_body =
      Yojson.Safe.from_string resp_body |> Trading_types.Bars.t_of_yojson
    in
    let status = Response.status response |> Code.string_of_status in
    Lwt.return (status, resp_body)
end
