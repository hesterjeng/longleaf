module Bars = Longleaf_bars
module Data = Bars.Data
module Headers = Cohttp.Header

(* Massive.io aggregate (bar) response item *)
type item = {
  o : float;              (* Open price *)
  h : float;              (* High price *)
  l : float;              (* Low price *)
  c : float;              (* Close price *)
  v : int;                (* Volume *)
  vw : float option;      (* Volume-weighted average price (optional) *)
  t : int;                (* Unix millisecond timestamp *)
  n : int option;         (* Number of transactions (optional) *)
}
[@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

let compare_item x y = Int.compare x.t y.t

(* Massive response structure *)
type response = {
  ticker : string;
  adjusted : bool option;
  queryCount : int option;
  resultsCount : int option;
  status : string;
  results : item list;
  next_url : string option;
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
let base_url = "https://api.massive.io"
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
            Data.set data Data.Type.Volume tick (Float.of_int item.v);
            Result.return ())
        (Ok ()) tickers
    in
    Result.return ()

  module Download = struct
    module Request = Market_data_api.Request
    module Timeframe = Trading_types.Timeframe

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
      Data.set data Data.Type.Volume i (Float.of_int volume)

    (* Parse a single bar item *)
    let parse_item data i item =
      let ( let* ) = Result.( let* ) in
      let* timestamp = timestamp_of_millis item.t in
      set_data_fields data i ~timestamp ~open_:item.o ~high:item.h
        ~low:item.l ~close:item.c ~volume:item.v;
      Result.return ()

    (* Convert results list to Data.t *)
    let results_to_data results =
      let ( let* ) = Result.( let* ) in
      let data = Data.make @@ List.length results in
      let* () =
        List.foldi
          (fun acc i item ->
            let* () = acc in
            parse_item data i item)
          (Ok ()) results
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
    let rec fetch_instrument_data_paginated (request : Request.t) instrument acc_results endpoint =
      let ( let* ) = Result.( let* ) in

      Eio.traceln "Fetching: %s" endpoint;
      let* json = Tools.get_cohttp ~client:Config.client ~headers ~endpoint in
      let* massive_resp = response_of_yojson_safe json in

      match massive_resp.status with
      | "OK" ->
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
          fetch_instrument_data_paginated request instrument all_results next_endpoint
        | None ->
          (* No more pages, convert to Data.t *)
          Eio.traceln "Massive_api: Converting %d bars for %a"
            (List.length all_results) Instrument.pp instrument;
          let* data = results_to_data all_results in
          Result.return (instrument, data))
      | status ->
        Error.fatal (Format.sprintf "Massive API error: %s" status)

    let fetch_instrument_data (request : Request.t) instrument =
      let symbol = Instrument.symbol instrument in
      let endpoint = build_endpoint request symbol in
      fetch_instrument_data_paginated request instrument [] endpoint

    (* Download historical data for all symbols in request *)
    let top (starting_request : Request.t) =
      let ( let* ) = Result.( let* ) in
      let split_requests = Request.split starting_request in
      Eio.traceln "Massive_api.top";

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
                (fetch_instrument_data request)
                request_symbols
            in
            Result.return @@ Bars.of_list res)
          split_requests
      in

      Eio.traceln "About to combine %d bar sets" (List.length r);
      let* final = Bars.combine r in
      Eio.traceln "Massive_api.top done";
      Result.return final
  end
end
