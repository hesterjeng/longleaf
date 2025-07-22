module Bars = Longleaf_bars
module Data = Bars.Data
open Backend_intf
open Trading_types

module Make (Input : BACKEND_INPUT) : S = struct
  (* module Ticker = Ticker.Instant *)
  (* module Portfolio = Portfolio.Generative () *)
  module Input = Input

  let get_trading_client _ = Result.fail @@ `MissingClient "Trading"
  let get_data_client _ = Result.fail @@ `MissingClient "Data"

  let init_state content =
    let ( let* ) = Result.( let* ) in
    let* bars =
      match Input.target with
      | None -> Error.fatal "No target for backtest"
      | Some b ->
        Bars.set_current b Input.options.flags.start;
        Result.return b
    in
    let config = Indicators_config.make Input.options.tacaml_indicators in
    State.make Input.options.flags.start bars content config

  let opts = Input.options

  (* let mutices = Longleaf_state.Mutex.create () *)

  (* let context = Input.options.context *)
  let next_market_open _ = Ok None
  let next_market_close _ = Ok Ptime.max
  let env = opts.eio_env
  let symbols = List.map Instrument.security opts.symbols
  let is_backtest = true
  let shutdown () = ()

  (* let overnight = Input.options.overnight *)
  let save_received = opts.flags.save_received
  let place_order = State.place_order
  let received_data = Bars.empty ()
  let target = Input.target

  let last_data_bar =
    let ( let* ) = Result.( let* ) in
    let last_data_bar = Bars.Latest.empty () in
    match target with
    | None -> Error.missing_data "No target to create last data bar"
    | Some target ->
      let* length = Bars.length target in
      let* () =
        Bars.fold target (Ok ()) @@ fun instrument data ok ->
        let* _ok = ok in
        let* col = Data.Column.of_data data (length - 1) in
        (* Eio.traceln "%a" Instrument.pp instrument; *)
        (* Eio.traceln "%a" Data.pp data; *)
        (* let* start = Data.Column.of_data data 0 in *)
        (* Eio.traceln "%a" Data.Column.pp col; *)
        (* Eio.traceln "%a" Data.Column.pp start; *)
        Bars.Latest.set last_data_bar instrument col;
        assert (not @@ Float.is_nan @@ Data.Column.last_exn col);
        Result.return ()
      in
      Result.return last_data_bar

  let update_bars _ _ _i = Result.return ()
  (* let ( let* ) = Result.( let* ) in *)
  (* let latest_data_bar = Bars.Latest.empty () in *)
  (* match target with *)
  (* | None -> Error.missing_data "No target to create last data bar" *)
  (* | Some target -> *)
  (*   let* () = *)
  (*     Bars.fold target (Ok ()) @@ fun instrument data ok -> *)
  (*     let* _ok = ok in *)
  (*     let* col = Data.Column.of_data data i in *)
  (*     assert (not @@ Float.is_nan @@ Data.Column.last_exn col); *)
  (*     Bars.Latest.set latest_data_bar instrument col; *)
  (*     Result.return () *)
  (*   in *)
  (*   Result.return () *)

  (* let latest_bars _ = *)
  (*   match Queue.take_opt data_remaining with *)
  (*   | Some next -> Result.return next *)
  (*   | None -> Error.missing_data "backtesting_backend.ml: latest_bars" *)

  (* let last_data_bar = *)
  (*   Eio.traceln "@[Creating last data bar.@]"; *)
  (*   let ( let* ) = Result.( let* ) in *)
  (*   let* target = *)
  (*     match Input.target with *)
  (*     | Some x -> Ok x *)
  (*     | None -> Error.missing_data "No target to create last data bar" *)
  (*   in *)
  (*   let res = Queue.fold (fun _ latest -> Some latest) None target in *)
  (*   match res with *)
  (*   | Some res -> Result.return res *)
  (*   | None -> *)
  (*     Error.fatal "No last data bar in Backtesting_backend.last_data_bar?" *)

  let liquidate (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    let symbols = State.held_symbols state in
    Eio.traceln "@[Liquidating %d positions@]@." (List.length symbols);

    (* Create sell orders for all positions *)
    let* liquidated_state =
      List.fold_left
        (fun prev symbol ->
          let* prev = prev in
          let qty = State.qty prev symbol in
          if qty = 0 then
            (* Eio.traceln "@[Skipping %a: no position to liquidate@]@." *)
            (*   Instrument.pp symbol; *)
            Result.return prev
          else
            let* data = State.data prev symbol in
            let last_price = Bars.Data.get data Last @@ State.tick state in
            let* timestamp =
              let time_float = Bars.Data.get data Time @@ State.tick state in
              Time.of_float_res time_float
            in
            let side = if qty > 0 then Side.Sell else Side.Buy in
            let abs_qty = Int.abs qty in
            let tick = State.tick prev in
            let order : Order.t =
              Order.make ~symbol ~tick ~side ~tif:TimeInForce.GoodTillCanceled
                ~order_type:OrderType.Market ~qty:abs_qty ~price:last_price
                ~timestamp ~profit:None ~reason:[ "Liquidate position" ]
            in
            (* Eio.traceln "@[Liquidating %d shares of %a at %f@]@." abs_qty *)
            (*   Instrument.pp symbol last_price; *)
            State.place_order prev order)
        (Ok state) symbols
    in

    (* Verify all positions are closed *)
    let remaining_symbols = State.held_symbols liquidated_state in
    if List.length remaining_symbols > 0 then (
      Eio.traceln "@[Warning: %d positions still remain after liquidation@]@."
        (List.length remaining_symbols);
      List.iter
        (fun sym ->
          let qty = State.qty liquidated_state sym in
          Eio.traceln "@[  %a: %d shares@]@." Instrument.pp sym qty)
        remaining_symbols)
    else Eio.traceln "@[All positions successfully liquidated@]@.";

    Result.return liquidated_state
end
