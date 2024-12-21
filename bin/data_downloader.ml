open Longleaf

module Downloader_ty = struct
  type t = Alpaca | Tiingo [@@deriving show]

  let of_string = function
    | "alpaca" -> Ok Alpaca
    | "tiingo" -> Ok Tiingo
    | _ -> invalid_arg "Unknown downloader service"

  let conv = Cmdliner.Arg.conv (of_string, pp)
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

module Args = struct
  (* Define the CLI arguments *)
  let today_arg =
    let doc = "Download data only for today." in
    Cmdliner.Arg.(value & flag & info [ "t" ] ~doc)

  let afterhours_arg =
    let doc = "Download afterhours data." in
    Cmdliner.Arg.(value & flag & info [ "afterhours" ] ~doc)

  let downloader_arg =
    let doc = "Choose downloader client.  Currently Tiingo or Alpaca." in
    Cmdliner.Arg.(value & pos 0 (some Downloader_ty.conv) None & info [] ~doc)

  let begin_arg =
    let doc = "Begin date as YYYY-MM-DD." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "begin" ] ~doc)

  let end_arg =
    let doc = "End date as YYYY-MM-DD." in
    Cmdliner.Arg.(value & opt (some string) None & info [ "end" ] ~doc)

  let timeframe_arg =
    let doc =
      "Choose a timeframe.  Valid values are minute, hours, day, week, or \
       month."
    in
    Cmdliner.Arg.(
      value
      & opt (some Trading_types.Timeframe.conv) None
      & info [ "timeframe" ] ~doc)

  let interval_arg =
    let doc = "Interval for the timeframe. 10 means every ten minutes." in
    Cmdliner.Arg.(value & opt (some int) None & info [ "interval" ] ~doc)

  let output_file_arg =
    let doc = "Output file for the downloaded data." in
    Cmdliner.Arg.(
      value & pos 1 (some string) None & info [] ~docv:"output file" ~doc)
end

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

  (* let tiingo_client switch eio_env longleaf_env = *)
  (*   let module Tiingo_client *)

  let top eio_env request prefix output_file
      (downloader_arg : Downloader_ty.t option) afterhours =
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
    let bars =
      match downloader_arg with
      | Some Alpaca -> MDA.Stock.historical_bars request
      | Some Tiingo ->
          let module Param = struct
            let longleaf_env = longleaf_env
            let client = Tiingo_api.tiingo_client eio_env switch
          end in
          let module Tiingo = Longleaf.Tiingo_api.Make (Param) in
          let res = Tiingo.Data.historical_bars ~afterhours request in
          Piaf.Client.shutdown Param.client;
          res
      | None ->
          invalid_arg "Need to specify downloader type for data_downloader."
    in
    Eio.traceln "Trying infill";
    Bars.Infill.top bars;
    Eio.traceln "%a" Bars.pp_stats bars;
    Bars.sort Longleaf__.Item.compare bars;
    (match output_file with
    | Some filename -> Bars.print_to_file_direct bars filename
    | None -> Bars.print_to_file bars prefix);
    Piaf.Client.shutdown data_client
end

module Cmd = struct
  let run today begin_arg end_arg timeframe_arg interval_arg output_file_arg
      downloader_arg afterhours_arg =
    Fmt_tty.setup_std_outputs ();
    let prefix = if today then "download_today" else "download" in
    let request =
      match
        Longleaf.Market_data_api.Historical_bars_request.of_data_downloader
          some_symbols begin_arg end_arg timeframe_arg interval_arg
      with
      | Some r -> r
      | None ->
          let start =
            if today then Time.of_ymd @@ get_todays_date ()
            else Time.of_ymd "2024-11-01"
          in
          {
            timeframe = Trading_types.Timeframe.min 10;
            start;
            symbols = some_symbols;
            end_ = None;
          }
    in
    let _ =
      Eio_main.run @@ fun eio_env ->
      Downloader.top eio_env request prefix output_file_arg downloader_arg
        afterhours_arg
    in
    ()

  let top =
    let term =
      Cmdliner.Term.(
        const run $ Args.today_arg $ Args.begin_arg $ Args.end_arg
        $ Args.timeframe_arg $ Args.interval_arg $ Args.output_file_arg
        $ Args.downloader_arg $ Args.afterhours_arg)
    in
    let doc = "Simple data downloader." in
    let info = Cmdliner.Cmd.info ~doc "data_downloader.exe" in
    Cmdliner.Cmd.v info term
end

let () = Stdlib.exit @@ Cmdliner.Cmd.eval Cmd.top
