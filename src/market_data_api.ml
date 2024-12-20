open Trading_types
module Headers = Piaf.Headers

module Historical_bars_request = struct
  type t = {
    symbols : string list;
    timeframe : Timeframe.t;
    start : Time.t;
    end_ : Time.t option; [@key "end"]
  }
  [@@deriving show, yojson]

  (* For use with the data_downloader binary *)
  let of_data_downloader symbols begin_arg end_arg timeframe_arg interval_arg =
    let ( let* ) = Option.( let* ) in
    let* begin_arg = begin_arg in
    let* end_arg = end_arg in
    let* timeframe_arg = timeframe_arg in
    let* interval_arg = interval_arg in
    Option.return
    @@ {
         symbols;
         timeframe = timeframe_arg interval_arg;
         start = Time.of_ymd begin_arg;
         end_ = Option.return @@ Time.of_ymd end_arg;
       }
end

module Make (Alpaca : Util.CLIENT) = struct
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
    let historical_bars (request : Historical_bars_request.t) =
      let symbols = String.concat "," request.symbols in
      let headers = headers () in
      let endpoint =
        Uri.of_string "/v2/stocks/bars" |> fun e ->
        Uri.add_query_params' e
        @@ [
             ("symbols", symbols);
             ("timeframe", Timeframe.to_string request.timeframe);
             ("start", Ptime.to_rfc3339 request.start);
           ]
        |> (fun uri ->
        match request.end_ with
        | Some end_t -> Uri.add_query_param' uri ("end", Ptime.to_rfc3339 end_t)
        | None -> uri)
        |> Uri.to_string
      in
      let rec collect_data ~endpoint ~headers acc =
        Eio.traceln "Sending a get request";
        let resp_body_json = get ~headers ~endpoint in
        let acc = Bars.t_of_yojson resp_body_json :: acc in
        match Util.get_next_page_token resp_body_json with
        | Some npt ->
            let endpoint =
              Uri.of_string endpoint |> fun u ->
              Uri.remove_query_param u "page_token" |> fun u ->
              Uri.add_query_param' u ("page_token", npt) |> Uri.to_string
            in
            collect_data ~endpoint ~headers acc
        | None -> acc
      in
      let paginated = collect_data ~endpoint ~headers [] in
      Bars.combine paginated

    let latest_bars (symbols : string list) =
      let symbols = String.concat "," symbols in
      let endpoint =
        Uri.of_string "/v2/stocks/bars/latest" |> fun u ->
        Uri.add_query_param' u ("symbols", symbols) |> Uri.to_string
      in
      let headers = headers () in
      let resp_body_json = get ~headers ~endpoint in
      Bars.t_of_yojson resp_body_json

    let latest_quotes (symbols : string list) =
      let symbols = String.concat "," symbols in
      let endpoint =
        Uri.of_string "/v2/stocks/quotes/latest" |> fun u ->
        Uri.add_query_param' u ("symbols", symbols) |> Uri.to_string
      in
      let headers = headers () in
      let resp_body_json = get ~headers ~endpoint in
      resp_body_json
  end
end
