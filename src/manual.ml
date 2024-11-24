(* Nonsense file for some one-off testing *)

let top eio_env longleaf_env =
  Eio.Switch.run @@ fun switch ->
  Util.yojson_safe true @@ fun () ->
  let client = Tiingo_api.tiingo_client eio_env switch in
  let module Client : Util.CLIENT = struct
    let client = client
    let longleaf_env = longleaf_env
  end in
  let module Tiingo = Tiingo_api.Make (Client) in
  let tickers = [ "AAPL"; "MSFT" ] in
  let test_resp = Tiingo.test () in
  Eio.traceln "@[%a@]@." Yojson.Safe.pp test_resp;
  let resp = Tiingo.latest tickers in
  Eio.traceln "@[%a@]@." Tiingo_api.pp resp;
  ()
