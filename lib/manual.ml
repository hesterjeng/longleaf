(* Nonsense file for some one-off testing *)

<<<<<<< HEAD
(* let tiingo_test eio_env longleaf_env = *)
(*   Eio.Switch.run @@ fun switch -> *)
(*   let client = Tiingo_api.tiingo_client eio_env switch in *)
(*   let module Client : Util.CLIENT = struct *)
(*     let client = client *)
(*     let longleaf_env = longleaf_env *)
(*   end in *)
(*   let module Tiingo = Tiingo_api.Make (Client) in *)
(*   let tickers = [ "AAPL"; "MSFT" ] in *)
(*   let test_resp = Tiingo.test () |> Result.get_exn in *)
(*   Eio.traceln "@[%a@]@." Yojson.Safe.pp test_resp; *)
(*   let _ = Tiingo.latest tickers in *)
(*   (\* Eio.traceln "@[%a@]@." Bars.pp resp; *\) *)
(*   Piaf.Client.shutdown client; *)
(*   () *)

(* let place_order_test eio_env longleaf_env = *)
(*   Eio.Switch.run @@ fun switch -> *)
(*   let module Input : Backend_intf.BACKEND_INPUT = struct *)
(*     let switch = switch *)
(*     let longleaf_env = longleaf_env *)
(*     let eio_env = eio_env *)
(*     let bars = Bars.empty () *)
(*     let symbols = [] *)
(*     let tick = 0.0 *)
(*     let overnight = true *)
(*     let target = None *)
(*     let save_received = false *)
(*     let resume_after_liquidate = false *)
(*     let mutices = Longleaf_mutex.create () *)
(*     let runtype = Options.Runtype.Manual *)
(*     let indicators_config : Indicators.Config.t = { fft = false } *)
(*     let dropout = false *)
(*     let save_to_file = false *)
(*   end in *)
(*   let module Alpaca = Alpaca_backend.Make (Input) in *)
(*   let state = Alpaca.init_state () in *)
(*   let order : Order.t = *)
(*     Order.make ~symbol:"NVDA" ~tick:0 ~side:Buy *)
(*       ~tif:Trading_types.TimeInForce.Day *)
(*       ~order_type:Trading_types.OrderType.Market ~qty:1 ~price:(-1.0) *)
(*       ~timestamp:(Time.of_int 0) ~reason:[ "Order test" ] ~profit:None *)
(*   in *)
(*   match Alpaca.place_order state order with *)
(*   | Ok () -> Alpaca.shutdown () *)
(*   | Error e -> *)
(*       Eio.traceln "%s" e; *)
(*       () *)

(* let top = place_order_test *)
=======
let tiingo_test eio_env longleaf_env =
  Eio.Switch.run @@ fun switch ->
  Util.yojson_safe false @@ fun () ->
  let client = Tiingo_api.tiingo_client eio_env switch in
  let module Client : Util.CLIENT = struct
    let client = client
    let longleaf_env = longleaf_env
  end in
  let module Tiingo = Tiingo_api.Make (Client) in
  let tickers = [ "AAPL"; "MSFT" ] in
  let test_resp = Tiingo.test () in
  Eio.traceln "@[%a@]@." Yojson.Safe.pp test_resp;
  let _ = Tiingo.latest tickers in
  (* Eio.traceln "@[%a@]@." Bars.pp resp; *)
  Piaf.Client.shutdown client;
  ()

let place_order_test eio_env longleaf_env =
  Eio.Switch.run @@ fun switch ->
  let module Input : Backend.BACKEND_INPUT = struct
    let switch = switch
    let longleaf_env = longleaf_env
    let eio_env = eio_env
    let bars = Bars.empty ()
    let symbols = []
    let tick = 0.0
    let overnight = true
    let target = None
    let save_received = false
    let resume_after_liquidate = false
    let mutices = Longleaf_mutex.create ()
    let runtype = Options.Runtype.Manual
    let indicators_config : Indicators.Config.t = { fft = false }
    let dropout = false
    let save_to_file = false
  end in
  let module Alpaca = Backend.Alpaca (Input) in
  let state = Alpaca.init_state () in
  let order : Trading_types.Order.t =
    Trading_types.Order.make ~symbol:"NVDA" ~side:Buy
      ~tif:Trading_types.TimeInForce.Day
      ~order_type:Trading_types.OrderType.Market ~qty:1 ~price:(-1.0)
      ~timestamp:(Time.of_int 0) ~reason:[ "Order test" ] ~profit:None
  in
  match Alpaca.place_order state order with
  | Ok () -> Alpaca.shutdown ()
  | Error e ->
      Eio.traceln "%s" e;
      ()

let top = place_order_test
>>>>>>> a13b8a6 (chore: src -> lib)
