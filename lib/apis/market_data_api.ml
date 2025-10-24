open Trading_types
module Headers = Cohttp.Header
module Util = Longleaf_util
module Bars = Longleaf_bars
module Data = Bars.Data

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

  (* Split a single large request into multiple *)
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
                let t = Option.get_exn_or "Time error in Request.split" t_opt in
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
      Eio.traceln "split: %a" (List.pp pp) rs;
      rs
end

(* Base URL for Alpaca market data *)
let base_url = "https://data.alpaca.markets"
let make_url path = base_url ^ path

module type CONFIG = sig
  val client : Cohttp_eio.Client.t
  val longleaf_env : Environment.t
end

module Make (Config : CONFIG) = struct
  let headers =
    Headers.of_list
      [
        ("APCA-API-KEY-ID", Config.longleaf_env.apca_api_key_id);
        ("APCA-API-SECRET-KEY", Config.longleaf_env.apca_api_secret_key);
      ]

  let get path =
    Tools.get_cohttp ~client:Config.client ~headers ~endpoint:(make_url path)

  module Stock = struct
    let historical_bars (request : Request.t) =
      let ( let* ) = Result.( let* ) in
      let symbols = String.concat "," request.symbols in
      let endpoint =
        Uri.of_string (make_url "/v2/stocks/bars") |> fun e ->
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
      let rec collect_data ~endpoint acc =
        Eio.traceln "Sending a get request";
        let* acc = acc in
        let* resp_body_json = Tools.get_cohttp ~client:Config.client ~headers ~endpoint in
        let* new_bars = Bars.t_of_yojson resp_body_json in
        let acc = new_bars :: acc in
        match Util.get_next_page_token resp_body_json with
        | Some npt ->
          let endpoint =
            Uri.of_string endpoint |> fun u ->
            Uri.remove_query_param u "page_token" |> fun u ->
            Uri.add_query_param' u ("page_token", npt) |> Uri.to_string
          in
          let res = collect_data ~endpoint (Ok acc) in
          res
        | None -> Ok acc
      in
      let* paginated = collect_data ~endpoint (Ok []) in
      Result.return @@ Bars.combine paginated

    let latest_bars (symbols : string list) =
      let ( let* ) = Result.( let* ) in
      let symbols = String.concat "," symbols in
      let endpoint =
        Uri.of_string (make_url "/v2/stocks/bars/latest") |> fun u ->
        Uri.add_query_param' u ("symbols", symbols) |> Uri.to_string
      in
      (* Eio.traceln "@[endpoint: %s@]@." endpoint; *)
      let* resp_body_json = Tools.get_cohttp ~client:Config.client ~headers ~endpoint in
      (* Eio.traceln "@[%a@]@." Yojson.Safe.pp resp_body_json; *)
      Bars.t_of_yojson resp_body_json

    let latest_quotes (symbols : string list) =
      let symbols = String.concat "," symbols in
      let endpoint =
        Uri.of_string (make_url "/v2/stocks/quotes/latest") |> fun u ->
        Uri.add_query_param' u ("symbols", symbols) |> Uri.to_string
      in
      Tools.get_cohttp ~client:Config.client ~headers ~endpoint

    (* Alpaca version of Tiingo.latest - mutates existing bars with latest data *)
    let latest bars tickers tick =
      let ( let* ) = Result.( let* ) in
      let symbols = List.map Instrument.symbol tickers in
      let* latest_bars_result = latest_bars symbols in
      (* Iterate through the returned bars and update existing bars at current tick *)
      Bars.fold latest_bars_result (Ok ()) @@ fun symbol latest_data acc ->
        let* () = acc in
        let* existing_data = Bars.get bars symbol in
        (* Alpaca returns single latest bar at index 0 *)
        Data.set existing_data Data.Type.Time tick
          (Data.get latest_data Data.Type.Time 0);
        Data.set existing_data Data.Type.Open tick
          (Data.get latest_data Data.Type.Open 0);
        Data.set existing_data Data.Type.High tick
          (Data.get latest_data Data.Type.High 0);
        Data.set existing_data Data.Type.Low tick
          (Data.get latest_data Data.Type.Low 0);
        Data.set existing_data Data.Type.Close tick
          (Data.get latest_data Data.Type.Close 0);
        Data.set existing_data Data.Type.Last tick
          (Data.get latest_data Data.Type.Last 0);
        Data.set existing_data Data.Type.Volume tick
          (Data.get latest_data Data.Type.Volume 0);
        Result.return ()
  end

  module Contract = struct
    open Longleaf_core.Contract

    (* Get all of the contracts available corresponding to the request. *)
    (*  The important thing is the symbol of the contract you want. *)
    (*   You can then buy/sell this option normally, like other securities.  *)
    (* i/e using a function of type Backend_intf.place_order *)
    let rec get_all (request : Request.t) =
      let ( let* ) = Result.( let* ) in
      let endpoint =
        Uri.of_string (make_url "/v2/positions") |> fun u ->
        Uri.add_query_params' u (Request.to_query_params request)
        |> Uri.to_string
      in
      let* res = Tools.get_cohttp ~client:Config.client ~headers ~endpoint in
      let* response = response_of_yojson_res res in
      let* next =
        match response.next_page_token with
        | None -> Result.return []
        | Some page_token ->
          let next_request = { request with page_token = Some page_token } in
          let* res = get_all next_request in
          Result.return res
      in
      Result.return @@ response.option_contracts @ next
  end
end
