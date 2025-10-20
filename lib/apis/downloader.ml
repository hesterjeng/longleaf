module Bars = Longleaf_bars

module Ty = struct
  type t = Alpaca | Tiingo [@@deriving show]

  let of_string = function
    | "alpaca" -> Ok Alpaca
    | "tiingo" -> Ok Tiingo
    | _ -> invalid_arg "Unknown downloader service"

  let conv = Cmdliner.Arg.conv (of_string, pp)
end

let data_client eio_env =
  (* let res = *)
    Cohttp_eio.Client.make ~https:None eio_env#net
    (* Piaf.Client.create ~sw:switch eio_env Longleaf_util.apca_api_data_url *)
  (* match res with *)
  (* | Ok x -> x *)
  (* | Error _ -> invalid_arg "Unable to create data client" *)

let download eio_env request (downloader_arg : Ty.t option) afterhours =
  Eio.Switch.run @@ fun switch ->
  let ( let* ) = Result.( let* ) in
  (* Util.yojson_safe true @@ fun () -> *)
  let longleaf_env = Longleaf_core.Environment.make () in
  (* let data_client = data_client switch eio_env in *)
  let module Conn : Market_data_api.CONFIG = struct
    let client = data_client eio_env
    let longleaf_env = longleaf_env
  end in
  let module MDA = Market_data_api.Make (Conn) in
  Eio.traceln "Making request %a..." Market_data_api.Request.pp request;
  let* bars =
    match downloader_arg with
    | Some Alpaca ->
      MDA.Stock.historical_bars request |> ( function
      | Ok x -> x
      | Error e ->
        Eio.traceln "error in MDA.Stock.historical_bars: %a" Error.pp e;
        invalid_arg "Error downloading historical bars" )
    | Some Tiingo ->
      let module Param : Tiingo_api.CONFIG = struct
        let longleaf_env = longleaf_env
        let client = Tiingo_api.tiingo_client eio_env switch
      end in
      let module Tiingo = Tiingo_api.Make (Param) in
      let res = Tiingo.Download.top ~afterhours request in
      res
    | None -> invalid_arg "Need to specify downloader type for data_downloader."
  in
  Result.return bars

let request today timeframe symbols begin_arg end_arg timeframe_arg interval_arg
    =
  match
    Market_data_api.Request.of_data_downloader symbols begin_arg end_arg
      timeframe_arg interval_arg
  with
  | Some r -> r
  | None ->
    Eio.traceln "Creating default download request because of missing arguments";
    let start =
      if today then Time.get_todays_date () else Time.of_ymd "2024-11-01"
    in
    { timeframe; start; symbols; end_ = None }

let today timeframe symbols : Market_data_api.Request.t =
  let start = Time.get_todays_date () in
  { timeframe; start; symbols; end_ = None }

let previous_30_days timeframe symbols : Market_data_api.Request.t =
  let today = Time.get_todays_date () in
  let start = Time.subtract_30_days today in
  { timeframe; start; symbols; end_ = None }
