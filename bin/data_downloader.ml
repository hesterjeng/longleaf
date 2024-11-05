open Longleaf

let some_symbols =
  [
    "NVDA";
    "TSLA";
    "AAPL";
    "MSFT";
    "NFLX";
    "META";
    "AMZN";
    "AMD";
    "AVGO";
    "ELV";
    "UNH";
    "MU";
    "V";
    "GOOG";
    "SMCI";
    "MSTR";
    "UBER";
    "LLY";
  ]

module Downloader = struct
  module Bars = Longleaf.Trading_types.Bars

  let data_client switch eio_env (longleaf_env : Longleaf.Environment.t) =
    let res =
      Piaf.Client.create ~sw:switch eio_env longleaf_env.apca_api_data_url
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create data client"

  let top eio_env request =
    Eio.Switch.run @@ fun switch ->
    Util.yojson_safe @@ fun () ->
    let longleaf_env = Environment.make () in
    let data_client = data_client switch eio_env longleaf_env in
    let module Conn : Longleaf.Util.ALPACA_SERVER = struct
      let client = data_client
      let longleaf_env = longleaf_env
    end in
    let module MDA = Market_data_api.Make (Conn) in
    Eio.traceln "Making request %a..."
      Market_data_api.Historical_bars_request.pp request;
    let bars = MDA.Stock.historical_bars request |> Bars.yojson_of_t in
    let str = Yojson.Safe.to_string bars in
    let tail = Lots_of_words.select () ^ "_" ^ Lots_of_words.select () in
    let filename = Format.sprintf "data/%s_%s" "download" tail in
    let oc = open_out filename in
    output_string oc str;
    close_out oc
end

let _ =
  Fmt_tty.setup_std_outputs ();
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  let request : Longleaf.Market_data_api.Historical_bars_request.t =
    {
      timeframe = Trading_types.Timeframe.min 30;
      start = Time.of_ymd "2024-10-01";
      symbols = some_symbols;
    }
  in
  let _ = Eio_main.run @@ fun eio_env -> Downloader.top eio_env request in
  ()
