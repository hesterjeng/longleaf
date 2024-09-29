open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
module Log = (val Logs.src_log Logs.(Src.create "data-api"))
open Trading_types

let h = Trading_api.h

module Stock = struct

  let historical_autions
      (env : Environment.t)
      (symbols : string list)
      (start : Ptime.t)
      (end_ : Ptime.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/stocks/auctions" in

end
