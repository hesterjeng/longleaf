module Headers = Piaf.Headers
module Hashtbl = Bars.Hashtbl

type item = {
  ticker : string;
  timestamp : Time.t;
  last : float;
  open_ : float; [@key "open"]
  prevClose : float;
  high : float;
  ask_price : float option; [@key "askPrice"]
  bid_price : float option; [@key "bidPrice"]
  low : float;
  volume : int;
}
[@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

type t = item list [@@deriving show { with_path = false }, yojson]

let item_to_bar_item (x : item) : Item.t =
  let open_ = x.open_ in
  let timestamp = x.timestamp in
  let high = x.high in
  let low = x.low in
  let close = x.prevClose in
  let last = x.last in
  let volume = x.volume in
  let order = None in
  Item.make ~open_ ~timestamp ~high ~low ~close ~last ~volume ~order ()

let to_latest (l : t) : Bars.Latest.t =
  List.map (fun (x : item) -> (x.ticker, item_to_bar_item x)) l
  |> Seq.of_list |> Hashtbl.of_seq

let tiingo_client eio_env sw =
  let res =
    Piaf.Client.create ~sw eio_env @@ Uri.of_string "https://api.tiingo.com"
  in
  match res with
  | Ok x -> x
  | Error _ -> invalid_arg "Unable to create Tiingo client"

module Make (Tiingo : Util.CLIENT) = struct
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

  let get = Util.get_piaf ~client:Tiingo.client
  let endpoint = Uri.of_string "/iex/"

  let test () =
    let endpoint = Uri.of_string "/api/test" |> Uri.to_string in
    get ~headers ~endpoint

  let latest tickers : Bars.Latest.t =
    let endpoint =
      Uri.add_query_params' endpoint [ ("tickers", String.concat "," tickers) ]
      |> Uri.to_string
    in
    (* Eio.traceln "@[endpoint: %s@]@." endpoint; *)
    let resp = get ~headers ~endpoint in
    (* Eio.traceln "@[%a@]@." Yojson.Safe.pp resp; *)
    let tiingo = t_of_yojson resp in
    to_latest tiingo

  module Data = struct
    module Historical_bars_request = Market_data_api.Historical_bars_request
    module Timeframe = Trading_types.Timeframe
    module Hashbtl = Bars.Hashtbl

    type t = {
      date : Time.t;
      open_ : float; [@key "open"]
      high : float;
      low : float;
      close : float;
      volume : float;
    }
    [@@deriving show, yojson]

    type resp = t list [@@deriving yojson]

    let item_of (x : t) =
      let { date; open_; high; low; close; volume } = x in
      Item.make ~timestamp:date ~open_ ~high ~low ~close
        ~volume:(Int.of_float volume) ~last:close ~order:None ()

    let historical_bars (request : Historical_bars_request.t) =
      let get_data symbol =
        let endpoint =
          Uri.of_string ("/iex/" ^ String.lowercase_ascii symbol ^ "/prices")
          |> fun e ->
          Uri.add_query_params' e
          @@ [
               ("ticker", symbol);
               ("resampleFreq", Timeframe.to_string_tiingo request.timeframe);
               ("startDate", Time.to_ymd request.start);
               ("forceFill", "true");
               ("columns", "open,high,low,close,volume");
             ]
          |> (fun uri ->
          match request.end_ with
          | Some end_t -> Uri.add_query_param' uri ("endDate", Time.to_ymd end_t)
          | None -> uri)
          |> Uri.to_string
        in
        Eio.traceln "%s" endpoint;
        let resp = get ~headers ~endpoint in
        Eio.traceln "%a" Yojson.Safe.pp resp;
        resp |> resp_of_yojson |> List.map item_of |> fun l ->
        (symbol, Vector.of_list l)
      in
      let items_assoc = List.map get_data request.symbols |> Seq.of_list in
      let hashtbl : Bars.t = Hashtbl.of_seq items_assoc in
      hashtbl
  end
end
