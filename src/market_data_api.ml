open Trading_types
module Headers = Piaf.Headers

module Make (Alpaca : Util.ALPACA_SERVER) = struct
  let client = Alpaca.client
  let longleaf_env = Alpaca.longleaf_env
  let get = Util.get_piaf ~client
  let delete = Util.delete_piaf ~client
  let post = Util.post_piaf ~client

  let headers () =
    Headers.of_list
      [
        ("APCA-API-KEY-ID", longleaf_env.apca_api_key_id);
        ("APCA-API-SECRET-KEY", longleaf_env.apca_api_secret_key);
      ]

  module Stock = struct
    module Historical_bars_request = struct
      type t = {
        symbols : string list;
        timeframe : Timeframe.t;
        start : Time.t;
      }
      [@@deriving show, yojson]
    end

    let historical_bars () (request : Historical_bars_request.t) =
      let symbols = String.concat "," request.symbols in
      let endpoint = "/v2/stocks/bars" in
      let headers =
        headers () |> fun h ->
        Headers.add_list h
          [
            ("symbols", symbols);
            ("timeframe", Timeframe.to_string request.timeframe);
            ("start", Ptime.to_rfc3339 request.start);
          ]
      in
      let rec collect_data ~endpoint ~headers acc =
        let resp_body_json = get ~headers ~endpoint in
        let acc = Bars.t_of_yojson resp_body_json :: acc in
        match Util.get_next_page_token resp_body_json with
        | Some npt ->
            let headers = Headers.replace headers "page_token" npt in
            collect_data ~endpoint ~headers acc
        | None -> acc
      in
      let paginated = collect_data ~endpoint ~headers [] in
      Bars.combine paginated

    let latest_bars () (symbols : string list) =
      let symbols = String.concat "," symbols in
      let endpoint = "/v2/stocks/bars/latest" in
      let headers = headers () |> fun h -> Headers.add h "symbols" symbols in
      let resp_body_json = get ~headers ~endpoint in
      Bars.t_of_yojson resp_body_json

    let latest_quotes () (symbols : string list) =
      let symbols = String.concat "," symbols in
      let endpoint = "/v2/stocks/quotes/latest" in
      let headers = headers () |> fun h -> Headers.add h "symbols" symbols in
      let resp_body_json = get ~headers ~endpoint in
      resp_body_json
  end
end
