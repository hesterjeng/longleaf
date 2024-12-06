open Longleaf

module Args = struct
  (* Define the CLI arguments *)
  let today_arg =
    let doc = "Download data only for today." in
    Cmdliner.Arg.(value & flag & info [ "t" ] ~doc)
end

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

let get_todays_date () =
  let time = Unix.time () in
  let local_time = Unix.localtime time in
  Printf.sprintf "%04d-%02d-%02d"
    (1900 + local_time.Unix.tm_year) (* Year *)
    (local_time.Unix.tm_mon + 1) (* Month *)
    local_time.Unix.tm_mday (* Day *)

module Downloader = struct
  let data_client switch eio_env (longleaf_env : Longleaf.Environment.t) =
    let res =
      Piaf.Client.create ~sw:switch eio_env longleaf_env.apca_api_data_url
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create data client"

  let top eio_env request =
    Eio.Switch.run @@ fun switch ->
    Util.yojson_safe true @@ fun () ->
    let longleaf_env = Environment.make () in
    let data_client = data_client switch eio_env longleaf_env in
    let module Conn : Longleaf.Util.CLIENT = struct
      let client = data_client
      let longleaf_env = longleaf_env
    end in
    let module MDA = Market_data_api.Make (Conn) in
    Eio.traceln "Making request %a..."
      Market_data_api.Historical_bars_request.pp request;
    let bars = MDA.Stock.historical_bars request in
    Eio.traceln "Trying infill";
    Bars.Infill.top bars;
    Eio.traceln "%a" Bars.pp_stats bars;
    Bars.print_to_file bars "download";
    Piaf.Client.shutdown data_client
end

module Cmd = struct
  let run today =
    Fmt_tty.setup_std_outputs ();
    let start =
      if today then Time.of_ymd @@ get_todays_date ()
      else Time.of_ymd "2024-11-01"
    in
    let request : Longleaf.Market_data_api.Historical_bars_request.t =
      {
        timeframe = Trading_types.Timeframe.min 10;
        start;
        symbols = some_symbols;
      }
    in
    let _ = Eio_main.run @@ fun eio_env -> Downloader.top eio_env request in
    ()

  let top =
    let term = Cmdliner.Term.(const run $ Args.today_arg) in
    let doc = "Simple data downloader." in
    let info = Cmdliner.Cmd.info ~doc "./main.exe" in
    Cmdliner.Cmd.v info term
end

let () = Stdlib.exit @@ Cmdliner.Cmd.eval Cmd.top
