open Lwt_result.Syntax
open Trading_types

let h = Trading_api.h

module Stock = struct
  module Historical_bars_request = struct
    type t = { symbols : string list; timeframe : Timeframe.t; start : Time.t }
    [@@deriving show, yojson]
  end

  let historical_auctions (env : Environment.t) (symbols : string list) =
    let headers = h env in
    let symbols = String.concat "," symbols in
    let uri = Uri.with_path env.apca_api_data_url "/v2/stocks/auctions" in
    let body =
      `Assoc [ ("symbols", `String symbols) ]
      |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
    in
    let* resp_body = Util.post ~headers ~body ~uri in
    Lwt_result.return resp_body

  let historical_bars (env : Environment.t)
      (request : Historical_bars_request.t) =
    let headers = h env in
    let symbols = String.concat "," request.symbols in
    let uri =
      Uri.with_path env.apca_api_data_url "/v2/stocks/bars" |> fun u ->
      Uri.add_query_param' u ("symbols", symbols) |> fun u ->
      Uri.add_query_param' u ("timeframe", Timeframe.to_string request.timeframe)
      |> fun u ->
      Uri.add_query_param' u ("start", Ptime.to_rfc3339 request.start)
    in
    let rec collect_data uri acc =
      let* resp_body_json = Util.get ~headers ~uri in
      let acc = Bars.t_of_yojson resp_body_json :: acc in
      match Util.get_next_page_token resp_body_json with
      | Some npt ->
          let uri =
            let removed = Uri.remove_query_param uri "page_token" in
            Uri.add_query_param' removed ("page_token", npt)
          in
          collect_data uri acc
      | None -> Lwt_result.return acc
    in
    let* paginated = collect_data uri [] in
    Lwt_result.return @@ Bars.combine paginated

  let latest_bars (env : Environment.t) (symbols : string list) =
    let headers = h env in
    let symbols = String.concat "," symbols in
    let uri =
      Uri.with_path env.apca_api_data_url "/v2/stocks/bars/latest" |> fun u ->
      Uri.add_query_param' u ("symbols", symbols)
    in
    let* resp_body_json = Util.get ~headers ~uri in
    (* Util.Util_log.app (fun k -> k "%a" Yojson.Safe.pp resp_body_json); *)
    let bar = Bars.t_of_yojson resp_body_json in
    Lwt_result.return bar

  let latest_quotes (env : Environment.t) (symbols : string list) =
    let headers = h env in
    let symbols = String.concat "," symbols in
    let uri =
      Uri.with_path env.apca_api_data_url "/v2/stocks/quotes/latest" |> fun u ->
      Uri.add_query_param' u ("symbols", symbols)
    in
    let* resp_body_json = Util.get ~headers ~uri in
    Lwt_result.return resp_body_json
end
