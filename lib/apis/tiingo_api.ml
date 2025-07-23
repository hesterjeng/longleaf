module Bars = Longleaf_bars
module Data = Bars.Data
module Headers = Piaf.Headers
(* module Hashtbl = Bars.Hashtbl *)

type item = {
  ticker : Instrument.t;
  timestamp : Time.t;
  last : float; [@key "tngoLast"]
  open_ : float; [@key "open"]
  prevClose : float;
  high : float;
  (* ask_price : float option; [@key "askPrice"] *)
  (* bid_price : float option; [@key "bidPrice"] *)
  low : float;
  volume : int;
}
[@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

let compare_item x y = Ptime.compare x.timestamp y.timestamp

let item_of_yojson x =
  match x with
  | `Null ->
    Result.fail @@ `JsonError "tiingo_api: received null in item_of_yojson"
  | _ -> (
    try Result.return @@ item_of_yojson x with
    | s ->
      Eio.traceln "[tiingo_api] %a" Yojson.Safe.pp x;
      let e = Printexc.to_string s in
      Eio.traceln "[tiingo_api] %s" e;
      Result.fail @@ `JsonError "Error while decoding json of Tiingo_api.item")

type t = item list [@@deriving show { with_path = false }]

let t_of_yojson (l : Yojson.Safe.t) =
  match l with
  | `List l -> Result.map_l item_of_yojson l
  | `Null -> Result.fail @@ `JsonError "Got `Null from Tiingo_api"
  | _ -> Result.fail @@ `JsonError "Expected a list in Tiingo_api.t_of_yojson"

let tiingo_client eio_env sw =
  let res =
    Piaf.Client.create ~sw eio_env @@ Uri.of_string "https://api.tiingo.com"
  in
  match res with
  | Ok x -> x
  | Error s ->
    Eio.traceln "%a" Piaf.Error.pp_hum s;
    invalid_arg "Unable to create Tiingo client"

module Make (Tiingo : Client.CLIENT) = struct
  let client = Tiingo.client

  let tiingo_key =
    match Tiingo.longleaf_env.tiingo_key with
    | Some s -> s
    | None -> invalid_arg "No tiingo key when trying to make tiingo connection"

  let headers =
    Headers.of_list
      [
        ("Content-Type", "application/json");
        ("Authorization", Format.asprintf "Token %s" tiingo_key);
      ]

  let get = Tools.get_piaf ~client:Tiingo.client
  let iex_endpoint = Uri.of_string "/iex/"

  let test () =
    let endpoint = Uri.of_string "/api/test" |> Uri.to_string in
    get ~headers ~endpoint

  let latest bars tickers =
    let ( let* ) = Result.( let* ) in
    let symbols = List.map Instrument.symbol tickers |> String.concat "," in
    let endpoint =
      Uri.add_query_params' iex_endpoint [ ("tickers", symbols) ]
      |> Uri.to_string
    in
    (* Eio.traceln "@[endpoint: %s@]@." endpoint; *)
    let* resp = get ~headers ~endpoint in
    (* Eio.traceln "@[%a@]@." Yojson.Safe.pp resp; *)
    let* tiingo = t_of_yojson resp in
    assert (List.is_sorted ~cmp:compare_item tiingo);
    let* () =
      List.foldi
        (fun acc i tiingo_item ->
          let* () = acc in
          let* data = Bars.get bars tiingo_item.ticker in
          (* Directly set data fields from tiingo item *)
          Data.set data Data.Type.Time i
            (Ptime.to_float_s tiingo_item.timestamp);
          Data.set data Data.Type.Open i tiingo_item.open_;
          Data.set data Data.Type.High i tiingo_item.high;
          Data.set data Data.Type.Low i tiingo_item.low;
          Data.set data Data.Type.Close i tiingo_item.prevClose;
          Data.set data Data.Type.Last i tiingo_item.last;
          Data.set data Data.Type.Volume i (Float.of_int tiingo_item.volume);
          Result.return ())
        (Ok ()) tiingo
    in
    Result.return ()

  module Download = struct
    module Request = Market_data_api.Request
    module Timeframe = Trading_types.Timeframe
    (* module Hashbtl = Bars.Hashtbl *)

    let parse_float_field (fields : (string * Yojson.Safe.t) list)
        (key : string) =
      match List.Assoc.get ~eq:String.equal key fields with
      | Some (`Float f) -> Ok f
      | Some (`Int i) -> Ok (Float.of_int i)
      | Some (`String s) -> (
        match Float.of_string_opt s with
        | Some f -> Ok f
        | None -> Error.fatal "Invalid float")
      | _ -> Error.fatal "Missing or invalid field"

    let parse_time_field (fields : (string * Yojson.Safe.t) list) (key : string)
        =
      match List.Assoc.get ~eq:String.equal key fields with
      | Some (`String s) -> (
        try Ok (Time.of_string s) with
        | _ -> Error.fatal "Invalid timestamp")
      | _ -> Error.fatal "Missing or invalid timestamp field"

    let set_data_fields data i ~date ~open_ ~high ~low ~close ~volume =
      Data.set data Data.Type.Index i (Float.of_int i);
      Data.set data Data.Type.Time i (Ptime.to_float_s date);
      Data.set data Data.Type.Open i open_;
      Data.set data Data.Type.High i high;
      Data.set data Data.Type.Low i low;
      Data.set data Data.Type.Close i close;
      Data.set data Data.Type.Last i close;
      Data.set data Data.Type.Volume i volume

    let parse_json_item data i item =
      let ( let* ) = Result.( let* ) in
      match item with
      | `Assoc fields ->
        let* date = parse_time_field fields "date" in
        let* open_ = parse_float_field fields "open" in
        let* high = parse_float_field fields "high" in
        let* low = parse_float_field fields "low" in
        let* close = parse_float_field fields "close" in
        let* volume = parse_float_field fields "volume" in
        set_data_fields data i ~date ~open_ ~high ~low ~close ~volume;
        Result.return ()
      | _ -> Error (`JsonError "Expected JSON object in array")

    let json_to_data_direct (json : Yojson.Safe.t) : (Data.t, Error.t) result =
      let ( let* ) = Result.( let* ) in
      match json with
      | `List json_list ->
        let size = 2 * List.length json_list in
        let data = Data.make size in
        let* () =
          List.foldi
            (fun acc i item ->
              let* () = acc in
              parse_json_item data i item)
            (Ok ()) json_list
        in
        Result.return data
      | _ -> Error (`JsonError "Expected JSON array")

    let build_base_endpoint (request : Request.t) symbol =
      match request.timeframe with
      | Day ->
        Uri.of_string
          ("/tiingo/daily/" ^ String.lowercase_ascii symbol ^ "/prices")
      | _ -> Uri.of_string ("/iex/" ^ String.lowercase_ascii symbol ^ "/prices")

    let build_query_params (request : Request.t) symbol =
      [
        ("ticker", Option.return @@ symbol);
        ( "resampleFreq",
          match request.timeframe with
          | Day -> None
          | x -> Option.return @@ Timeframe.to_string_tiingo x );
        ("startDate", Option.return @@ Time.to_ymd request.start);
        ( "forceFill",
          match request.timeframe with
          | Day -> None
          | _ -> Some "true" );
        ( "columns",
          match request.timeframe with
          | Day -> None
          | _ -> Option.return @@ "open,high,low,close,volume" );
      ]
      |> List.filter_map (fun (x, y) ->
             match y with
             | None -> None
             | Some y -> Some (x, y))

    let build_endpoint ?(afterhours = false) (request : Request.t) symbol =
      build_base_endpoint request symbol |> fun e ->
      Uri.add_query_params' e (build_query_params request symbol)
      |> (fun uri ->
      match request.end_ with
      | Some end_t -> Uri.add_query_param' uri ("endDate", Time.to_ymd end_t)
      | None -> uri)
      |> (fun uri ->
      if afterhours then Uri.add_query_param' uri ("afterHours", "true")
      else uri)
      |> Uri.to_string

    let fetch_instrument_data ?(afterhours = false) (request : Request.t)
        instrument =
      let ( let* ) = Result.( let* ) in
      let symbol = Instrument.symbol instrument in
      let endpoint = build_endpoint ~afterhours request symbol in
      Eio.traceln "%s" endpoint;
      let* json = get ~headers ~endpoint in
      Eio.traceln "Tiingo_api.ml: Converting data directly from JSON";
      let* data = json_to_data_direct json in
      Result.return @@ (instrument, data)

    let top ?(afterhours = false) (starting_request : Request.t) =
      let ( let* ) = Result.( let* ) in
      let split_requests = Request.split starting_request in
      Eio.traceln "Tiingo_api.top";
      let* r =
        let request_symbols =
          List.map Instrument.of_string starting_request.symbols
        in
        Eio.traceln "Tiingo_api.ml: About to map get_data";
        Result.map_l
          (fun request ->
            let* res =
              Result.map_l
                (fetch_instrument_data ~afterhours request)
                request_symbols
            in
            Result.return @@ Bars.of_list res)
          split_requests
      in
      Eio.traceln "About to combine";
      let final = Bars.combine r in
      Eio.traceln "Tiingo_api.top done";
      final
  end
end
