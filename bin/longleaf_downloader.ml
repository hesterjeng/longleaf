open Longleaf_core
module Collections = Longleaf_strategies.Collections
module Market_data_api = Longleaf_apis.Market_data_api
module Downloader = Longleaf_apis.Downloader
module Ty = Downloader.Ty

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
    Cmdliner.Arg.(value & pos 0 (some Ty.conv) None & info [] ~doc)

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

module Cmd = struct
  let run today begin_arg end_arg timeframe_arg interval_arg output_file_arg
      downloader_arg afterhours_arg =
    Fmt_tty.setup_std_outputs ();
    let prefix = if today then "download_today" else "download" in
    let collection = Collections.sp100 in
    let _ =
      Eio_main.run @@ fun eio_env ->
      let request =
        Downloader.request today (Trading_types.Timeframe.Min 10) collection
          begin_arg end_arg timeframe_arg interval_arg
      in
      Downloader.download eio_env request prefix output_file_arg downloader_arg
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
    let info = Cmdliner.Cmd.info ~doc "longleaf_downloader" in
    Cmdliner.Cmd.v info term
end

let () = Stdlib.exit @@ Cmdliner.Cmd.eval Cmd.top
