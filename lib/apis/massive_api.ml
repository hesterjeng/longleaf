module Bars = Longleaf_bars
module Data = Bars.Data
module Headers = Cohttp.Header

(* Massive.io aggregate (bar) response item *)
type item = {
  o : float;              (* Open price *)
  h : float;              (* High price *)
  l : float;              (* Low price *)
  c : float;              (* Close price *)
  v : float;              (* Volume - API returns as number/float *)
  vw : float option;      (* Volume-weighted average price (optional) *)
  t : int;                (* Unix millisecond timestamp *)
  n : int option;         (* Number of transactions (optional) *)
}
[@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

let compare_item x y = Int.compare x.t y.t

(* Massive response structure *)
type response = {
  ticker : string;
  adjusted : bool option; [@yojson.option]
  queryCount : int option; [@yojson.option]
  resultsCount : int option; [@yojson.option]
  status : string;
  results : item list;
  next_url : string option; [@yojson.option]
}
[@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

(* Wrapper for safe yojson parsing with better error messages *)
let response_of_yojson_safe x =
  match x with
  | `Null ->
    Result.fail @@ `JsonError "massive_api: received null in response_of_yojson"
  | _ ->
    (try Result.return @@ response_of_yojson x with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, json) ->
      Eio.traceln "[massive_api] JSON conversion error:";
      Eio.traceln "  Exception: %s" (Printexc.to_string exn);
      Eio.traceln "  JSON: %a" Yojson.Safe.pp json;
      Result.fail @@ `JsonError ("Error decoding Massive_api.response: " ^ Printexc.to_string exn)
    | s ->
      Eio.traceln "[massive_api] %a" Yojson.Safe.pp x;
      let e = Printexc.to_string s in
      Eio.traceln "[massive_api] %s" e;
      Result.fail @@ `JsonError "Error while decoding json of Massive_api.response")

let massive_client eio_env _sw =
  Cohttp_eio.Client.make
    ~https:None (Eio.Stdenv.net eio_env)

(* Base URL for Massive API *)
let base_url = "https://api.massive.com"
let make_url path = base_url ^ path

module type CONFIG = sig
  val client : Cohttp_eio.Client.t
  val longleaf_env : Environment.t
end

module Make (Config : CONFIG) = struct
  let massive_key =
    match Config.longleaf_env.massive_key with
    | Some s -> s
    | None -> invalid_arg "No massive key when trying to make massive connection"

  let headers =
    Headers.of_list
      [
        ("Content-Type", "application/json");
      ]

  (* Build endpoint with apiKey query parameter *)
  let add_api_key uri =
    Uri.add_query_param' uri ("apiKey", massive_key)

  let get path =
    let endpoint = make_url path |> Uri.of_string |> add_api_key |> Uri.to_string in
    Tools.get_cohttp ~client:Config.client ~headers ~endpoint

  (* Get latest bars for given tickers *)
  let latest bars tickers tick =
    let ( let* ) = Result.( let* ) in
    let symbols = List.map Instrument.symbol tickers in

    (* Massive doesn't have a bulk "latest bars" endpoint like Tiingo/Alpaca *)
    (* We need to make individual requests or use the previous close endpoint *)
    (* For now, we'll implement using previous day's close endpoint *)
    (* In production, you'd want to use WebSocket for real-time data *)

    let* () =
      List.fold_left
        (fun acc ticker ->
          let* () = acc in
          let symbol = Instrument.symbol ticker in
          let endpoint =
            Uri.of_string (make_url ("/v2/aggs/ticker/" ^ symbol ^ "/prev"))
            |> add_api_key
            |> Uri.to_string
          in
          let* resp = Tools.get_cohttp ~client:Config.client ~headers ~endpoint in
          let* massive_resp = response_of_yojson_safe resp in

          match massive_resp.results with
          | [] ->
            Eio.traceln "Warning: No results for %s in Massive latest" symbol;
            Result.return ()
          | item :: _ ->
            let* data = Bars.get bars ticker in
            (* Convert Unix milliseconds to Ptime *)
            let timestamp_s = Float.of_int item.t /. 1000.0 in
            let* timestamp =
              match Ptime.of_float_s timestamp_s with
              | Some t -> Result.return t
              | None -> Error.fatal "Invalid timestamp from Massive"
            in

            (* Update data fields *)
            Data.set data Data.Type.Time tick (Ptime.to_float_s timestamp);
            Data.set data Data.Type.Open tick item.o;
            Data.set data Data.Type.High tick item.h;
            Data.set data Data.Type.Low tick item.l;
            Data.set data Data.Type.Close tick item.c;
            Data.set data Data.Type.Last tick item.c;
            Data.set data Data.Type.Volume tick item.v;
            Result.return ())
        (Ok ()) tickers
    in
    Result.return ()

  module Download = struct
    module Request = Market_data_api.Request
    module Timeframe = Trading_types.Timeframe

    (* Filter bars to only regular trading hours (9:30 AM - 4:00 PM ET, weekdays only) *)
    (* Massive includes pre-market, after-hours, and weekend data by default *)
    let is_regular_hours (timestamp : Ptime.t) : bool =
      let unix_time = Ptime.to_float_s timestamp in

      (* Use Time module's functions for proper ET timezone handling *)
      let minutes_since_open = Time.minutes_since_open unix_time in
      let minutes_until_close = Time.minutes_until_close unix_time in

      (* Check if within market hours (9:30 AM - 4:00 PM ET) *)
      (* minutes_since_open >= 0 means at or after 9:30 AM ET *)
      (* minutes_until_close > 0 means before 4:00 PM ET *)
      let is_market_hours =
        Float.compare minutes_since_open 0.0 >= 0 &&
        Float.compare minutes_until_close 0.0 > 0 in

      (* Check if weekday using Ptime's built-in weekday function *)
      let weekday = Ptime.weekday timestamp in
      let is_weekday = match weekday with
        | `Mon | `Tue | `Wed | `Thu | `Fri -> true
        | `Sat | `Sun -> false
      in

      is_weekday && is_market_hours

    let filter_regular_hours (items : item list) : item list =
      List.filter (fun item ->
        match Ptime.of_float_s (Float.of_int item.t /. 1000.0) with
        | Some timestamp -> is_regular_hours timestamp
        | None -> false
      ) items

    (* Convert Massive Unix milliseconds to Ptime *)
    let timestamp_of_millis ms =
      let seconds = Float.of_int ms /. 1000.0 in
      match Ptime.of_float_s seconds with
      | Some t -> Ok t
      | None -> Error.fatal "Invalid timestamp from Massive"

    (* Set data fields for a bar *)
    let set_data_fields data i ~timestamp ~open_ ~high ~low ~close ~volume =
      Data.set data Data.Type.Index i (Float.of_int i);
      Data.set data Data.Type.Time i (Ptime.to_float_s timestamp);
      Data.set data Data.Type.Open i open_;
      Data.set data Data.Type.High i high;
      Data.set data Data.Type.Low i low;
      Data.set data Data.Type.Close i close;
      Data.set data Data.Type.Last i close;
      Data.set data Data.Type.Volume i volume

    (* Parse a single bar item *)
    let parse_item data i item =
      let ( let* ) = Result.( let* ) in
      let* timestamp = timestamp_of_millis item.t in
      set_data_fields data i ~timestamp ~open_:item.o ~high:item.h
        ~low:item.l ~close:item.c ~volume:item.v;
      Result.return ()

    (* Convert results list to Data.t, filtering to regular trading hours *)
    let results_to_data ~minimal_allocation results =
      let ( let* ) = Result.( let* ) in
      let original_count = List.length results in
      (* Filter to only regular trading hours *)
      let filtered = filter_regular_hours results in
      let filtered_count = List.length filtered in
      if filtered_count < original_count then
        Eio.traceln "      Filtered %d bars to %d regular hours bars (removed %d extended hours)"
          original_count filtered_count (original_count - filtered_count);
      let data =
        if minimal_allocation then Data.make_for_download filtered_count
        else Data.make filtered_count
      in
      let* () =
        List.foldi
          (fun acc i item ->
            let* () = acc in
            parse_item data i item)
          (Ok ()) filtered
      in
      Result.return data

    (* Build Massive aggregates endpoint *)
    let build_endpoint (request : Request.t) symbol =
      let timespan =
        match request.timeframe with
        | Min _ -> "minute"
        | Hour _ -> "hour"
        | Day -> "day"
        | Week -> "week"
        | Month _ -> "month"
      in
      let multiplier =
        match request.timeframe with
        | Min n -> string_of_int n
        | Hour n -> string_of_int n
        | Day -> "1"
        | Week -> "1"
        | Month n -> string_of_int n
      in
      let from_date = Time.to_ymd request.start in
      let to_date =
        match request.end_ with
        | Some end_t -> Time.to_ymd end_t
        | None -> Time.to_ymd (Ptime_clock.now ())
      in

      let path = Format.sprintf "/v2/aggs/ticker/%s/range/%s/%s/%s/%s"
        symbol multiplier timespan from_date to_date
      in

      Uri.of_string (make_url path)
      |> add_api_key
      |> fun u -> Uri.add_query_param' u ("adjusted", "true")
      |> fun u -> Uri.add_query_param' u ("sort", "asc")
      |> fun u -> Uri.add_query_param' u ("limit", "50000")
      |> Uri.to_string

    (* Fetch data for a single instrument with pagination *)
    let rec fetch_instrument_data_paginated ~minimal_allocation (request : Request.t) instrument acc_results endpoint =
      let ( let* ) = Result.( let* ) in

      Eio.traceln "    Fetching %a: %s" Instrument.pp instrument endpoint;
      let* json = Tools.get_cohttp ~client:Config.client ~headers ~endpoint in
      let* massive_resp = response_of_yojson_safe json in

      match massive_resp.status with
      | "OK" | "DELAYED" ->
        (* Log if we're receiving delayed data *)
        if String.equal massive_resp.status "DELAYED" then
          Eio.traceln "    Note: Receiving delayed data for %a" Instrument.pp instrument;

        let all_results = acc_results @ massive_resp.results in

        (* Check if there's a next page *)
        (match massive_resp.next_url with
        | Some next_url ->
          (* Add API key to next_url *)
          let next_endpoint =
            Uri.of_string next_url
            |> add_api_key
            |> Uri.to_string
          in
          Eio.traceln "Pagination: fetching next page (%d results so far)"
            (List.length all_results);
          fetch_instrument_data_paginated ~minimal_allocation request instrument all_results next_endpoint
        | None ->
          (* No more pages, convert to Data.t *)
          Eio.traceln "      Converting %d raw bars for %a"
            (List.length all_results) Instrument.pp instrument;
          let* data = results_to_data ~minimal_allocation all_results in
          Eio.traceln "      Result: %a has %d bars after filtering"
            Instrument.pp instrument (Data.size data);
          Result.return (instrument, data))
      | status ->
        Error.fatal (Format.sprintf "Massive API error: %s" status)

    let fetch_instrument_data ~minimal_allocation (request : Request.t) instrument =
      let symbol = Instrument.symbol instrument in
      let endpoint = build_endpoint request symbol in
      fetch_instrument_data_paginated ~minimal_allocation request instrument [] endpoint

    (* Align all symbols to a common regular timeline *)
    (* Generate fixed market schedule: 9:30 AM - 4:00 PM ET, 1-minute intervals *)
    (* This ensures consistent EOD bars and eliminates dependency on data quality *)
    let generate_market_timeline (start_time : float) (end_time : float) (interval_seconds : float)
        : float list =
      (* Market hours in minutes since midnight ET *)
      let market_open_minutes = 9.0 *. 60.0 +. 30.0 in  (* 9:30 AM = 570 minutes *)
      let market_close_minutes = 16.0 *. 60.0 in        (* 4:00 PM = 960 minutes *)
      let interval_minutes = interval_seconds /. 60.0 in

      (* Convert start/end times to Ptime for date extraction *)
      let start_ptime = Ptime.of_float_s start_time
                        |> Option.get_exn_or "massive_api: invalid start_time" in
      let end_ptime = Ptime.of_float_s end_time
                      |> Option.get_exn_or "massive_api: invalid end_time" in
      let ((start_year, start_month, start_day), _) = Ptime.to_date_time start_ptime in

      (* Generate timestamps for a single day's market hours *)
      let generate_day_bars (year, month, day) =
        let weekday = Time.weekday_of_date year month day in
        if weekday >= 1 && weekday <= 5 then begin
          (* Determine DST status for this date using noon UTC as reference *)
          let ref_ptime = Ptime.of_date_time ((year, month, day), ((12, 0, 0), 0))
                          |> Option.get_exn_or "massive_api: failed to create reference time" in
          let ref_unix = Ptime.to_float_s ref_ptime in
          let et_offset_hours = Time.et_utc_offset_hours ref_unix in

          (* Calculate number of bars in market day *)
          let num_bars = int_of_float ((market_close_minutes -. market_open_minutes) /. interval_minutes) + 1 in

          List.init num_bars (fun i ->
            let minutes_et = market_open_minutes +. (float_of_int i *. interval_minutes) in
            let hours_et = int_of_float (minutes_et /. 60.0) in
            let mins_et = int_of_float (minutes_et -. (float_of_int hours_et *. 60.0)) in

            (* Create Ptime treating ET time as if it were UTC *)
            let ptime = Ptime.of_date_time ((year, month, day), ((hours_et, mins_et, 0), 0))
                        |> Option.get_exn_or "massive_api: failed to create market bar time" in
            let unix_time = Ptime.to_float_s ptime in

            (* Adjust to convert from "ET treated as UTC" to actual UTC *)
            unix_time -. (et_offset_hours *. 3600.0)
          )
        end else
          []  (* Weekend - no bars *)
      in

      (* Recursively generate bars for all days in range *)
      let rec generate_days current_date =
        let current_ptime = Ptime.of_date current_date
                            |> Option.get_exn_or "massive_api: invalid current_date" in
        if Ptime.compare current_ptime end_ptime > 0 then
          []
        else
          let day_bars = generate_day_bars current_date in
          let next_date = Time.add_days current_date 1 in
          day_bars @ generate_days next_date
      in

      generate_days (start_year, start_month, start_day)

    let align_to_timeline ~minimal_allocation (data_list : (Instrument.t * Data.t) list) (interval_seconds : float)
        : ((Instrument.t * Data.t) list, Error.t) result =
      let ( let* ) = Result.( let* ) in

      if List.is_empty data_list then
        Result.return data_list
      else begin
        Eio.traceln "Aligning %d symbols to fixed market schedule (9:30-4:00 PM ET)..."
          (List.length data_list);

        (* Determine date range from the data - extract earliest and latest timestamps *)
        let earliest_times = List.map (fun (_, data) ->
          Data.get data Data.Type.Time 0  (* First timestamp in this symbol's data *)
        ) data_list in
        let latest_times = List.map (fun (_, data) ->
          let size = Data.size data in
          Data.get data Data.Type.Time (size - 1)  (* Last timestamp in this symbol's data *)
        ) data_list in

        (* Find the overall earliest and latest timestamps across all symbols *)
        let start_time = List.fold_left Float.min (List.hd earliest_times) earliest_times in
        let end_time = List.fold_left Float.max (List.hd latest_times) latest_times in

        (* Generate fixed market timeline *)
        let timeline = generate_market_timeline start_time end_time interval_seconds in
        let num_bars = List.length timeline in

        (* Create skeleton Data.t from fixed timeline *)
        let longest_data =
          if minimal_allocation then Data.make_for_download num_bars
          else Data.make num_bars
        in
        List.iteri (fun i timestamp ->
          Data.set longest_data Data.Type.Time i timestamp;
          Data.set longest_data Data.Type.Index i (float_of_int i)
        ) timeline;

        Eio.traceln "  Generated fixed schedule: %d bars (every %.0f seconds)"
          num_bars interval_seconds;
        Eio.traceln "  Time range: %.0f to %.0f" start_time end_time;

        (* Align each symbol to the skeleton's exact timestamps *)
        (* This preserves the regular-hours-only nature of the filtered data *)
        Result.map_l (fun (instrument, sparse_data) ->
          if Data.size sparse_data = 0 then begin
            Eio.traceln "  WARNING: %a has no data" Instrument.pp instrument;
            Error.fatal (Format.asprintf "Symbol %a has no data in regular hours" Instrument.pp instrument)
          end else begin
            let aligned_data =
              if minimal_allocation then Data.make_for_download num_bars
              else Data.make num_bars
            in
            let sparse_size = Data.size sparse_data in

            (* Find first valid bar to use for back-filling *)
            let first_bar_idx = 0 in

            (* Track last valid source for forward-filling *)
            let last_valid_idx = ref None in
            let sparse_idx = ref 0 in

            (* For each tick in the skeleton timeline *)
            for aligned_tick = 0 to num_bars - 1 do
              (* Use skeleton's actual timestamp (already filtered to regular hours) *)
              let target_time = Data.get longest_data Data.Type.Time aligned_tick in

              (* Advance sparse_idx past any bars before target_time *)
              let threshold = target_time -. (interval_seconds /. 2.0) in
              while !sparse_idx < sparse_size &&
                    Stdlib.( < ) (Data.get sparse_data Data.Type.Time !sparse_idx) threshold do
                last_valid_idx := Some !sparse_idx;
                incr sparse_idx
              done;

              (* Determine which sparse bar to use for this timeline tick *)
              let source_idx =
                if !sparse_idx < sparse_size then
                  let sparse_time = Data.get sparse_data Data.Type.Time !sparse_idx in
                  (* If sparse bar is close enough to target time, use it *)
                  let tolerance = interval_seconds /. 2.0 in
                  if Stdlib.( < ) (abs_float (sparse_time -. target_time)) tolerance then begin
                    last_valid_idx := Some !sparse_idx;
                    Some !sparse_idx
                  end else
                    !last_valid_idx  (* Forward-fill from last valid *)
                else
                  !last_valid_idx  (* Past end of data, forward-fill *)
              in

              (* Copy data from source to aligned timeline *)
              let src_idx = match source_idx with
                | Some idx -> idx
                | None -> first_bar_idx  (* Back-fill: use first bar if no data yet *)
              in
              Data.set aligned_data Data.Type.Index aligned_tick (float_of_int aligned_tick);
              Data.set aligned_data Data.Type.Time aligned_tick target_time;
              Data.set aligned_data Data.Type.Open aligned_tick (Data.get sparse_data Data.Type.Open src_idx);
              Data.set aligned_data Data.Type.High aligned_tick (Data.get sparse_data Data.Type.High src_idx);
              Data.set aligned_data Data.Type.Low aligned_tick (Data.get sparse_data Data.Type.Low src_idx);
              Data.set aligned_data Data.Type.Close aligned_tick (Data.get sparse_data Data.Type.Close src_idx);
              Data.set aligned_data Data.Type.Last aligned_tick (Data.get sparse_data Data.Type.Close src_idx);
              Data.set aligned_data Data.Type.Volume aligned_tick (Data.get sparse_data Data.Type.Volume src_idx)
            done;

            Eio.traceln "  %a: aligned %d sparse bars to %d skeleton bars"
              Instrument.pp instrument sparse_size num_bars;
            Result.return (instrument, aligned_data)
          end
        ) data_list
      end

    (* Download historical data for all symbols in request *)
    let top ?(minimal_allocation=false) (starting_request : Request.t) =
      let ( let* ) = Result.( let* ) in
      let split_requests = Request.split starting_request in
      Eio.traceln "Massive_api.top (minimal_allocation=%b)" minimal_allocation;

      let* r =
        let request_symbols =
          List.map Instrument.of_string starting_request.symbols
        in
        Eio.traceln "Massive_api: About to fetch data for %d symbols"
          (List.length request_symbols);
        Result.map_l
          (fun request ->
            let* res =
              Result.map_l
                (fetch_instrument_data ~minimal_allocation request)
                request_symbols
            in
            (* Calculate interval in seconds from timeframe *)
            let interval_seconds =
              match request.timeframe with
              | Timeframe.Min n -> float_of_int n *. 60.0
              | Timeframe.Hour n -> float_of_int n *. 3600.0
              | Timeframe.Day -> 86400.0
              | Timeframe.Week -> 604800.0
              | Timeframe.Month _ -> 2592000.0
            in
            (* Align all symbols to regular timeline before creating Bars *)
            let* aligned_res = align_to_timeline ~minimal_allocation res interval_seconds in
            Result.return @@ Bars.of_list aligned_res)
          split_requests
      in

      Eio.traceln "About to combine %d bar sets" (List.length r);
      let* final = Bars.combine r in
      Eio.traceln "Massive_api.top done";
      Result.return final
  end
end
