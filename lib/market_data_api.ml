open Trading_types
module Headers = Piaf.Headers

module Request = struct
  (* Type for requesting historical bars *)
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
    Option.return
    @@ {
         symbols;
         timeframe = timeframe_arg interval_arg;
         start = Time.of_ymd begin_arg;
         end_ = Option.return @@ Time.of_ymd end_arg;
       }

  let split (x : t) =
    match x.end_ with
    | None -> [ x ]
    | Some request_end ->
        let timeframe = Timeframe.to_float x.timeframe in
        let request_start = x.start in
        Eio.traceln "@[start: %a@]@.@[end: %a@]@." (Ptime.pp_human ())
          request_start (Ptime.pp_human ()) request_end;
        let start_f, end_f =
          Pair.map_same Ptime.to_float_s (request_start, request_end)
        in
        assert (end_f >=. start_f);
        let diff = end_f -. start_f in
        let diff_divided =
          (diff /. timeframe /. 10000.0) +. 2.0 |> Float.round |> Float.to_int
        in
        Eio.traceln "Breaking request into %d requests" diff_divided;
        let increment = diff /. Float.of_int diff_divided in
        let start_end_times =
          ( List.init diff_divided @@ fun i ->
            start_f +. (Float.of_int i *. increment) |> Ptime.of_float_s )
          |> fun l ->
          List.mapi
            (fun i x ->
              Pair.map_same
                (fun t_opt ->
                  let t =
                    Option.get_exn_or "Time error in Request.split" t_opt
                  in
                  let date = Ptime.to_date t in
                  Ptime.of_date date
                  |> Option.get_exn_or "Bad date in Request.split")
                ( x,
                  match List.get_at_idx (i + 1) l with
                  | Some y ->
                      let one_day =
                        Ptime.Span.of_float_s 86400.0
                        |> Option.get_exn_or "A day is a span"
                      in
                      let y =
                        Option.get_exn_or "Must have a time in Request.split" y
                      in
                      Ptime.sub_span y one_day
                  | None -> Option.return request_end ))
            l
        in
        let rs =
          List.map
            (fun (start, end_) -> { x with start; end_ = Some end_ })
            start_end_times
        in
        Eio.traceln "%a" (List.pp pp) rs;
        rs
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
    let historical_bars (request : Request.t) =
      let ( let* ) = Result.( let* ) in
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
        let resp_body_json =
          get ~headers ~endpoint |> function
          | Ok x -> x
          | Error e ->
              Eio.traceln
                "market_data_api.ml: error while getting historical data with \
                 alpaca: %a"
                Error.pp e;
              invalid_arg "Bad JSON while getting historical bars"
        in
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
      let* paginated = collect_data ~endpoint ~headers [] |> Result.flatten_l in
      Result.return @@ Bars.combine paginated

    let latest_bars (symbols : string list) =
      let symbols = String.concat "," symbols in
      let endpoint =
        Uri.of_string "/v2/stocks/bars/latest" |> fun u ->
        Uri.add_query_param' u ("symbols", symbols) |> Uri.to_string
      in
      let headers = headers () in
      let resp_body_json = get ~headers ~endpoint in
      Result.map Bars.t_of_yojson resp_body_json

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
