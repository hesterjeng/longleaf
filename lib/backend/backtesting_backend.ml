module Bars = Longleaf_bars
module Data = Bars.Data
open Backend_intf
open Trading_types

module Make (Input : BACKEND_INPUT) : S = struct
  (* module Ticker = Ticker.Instant *)
  (* module Portfolio = Portfolio.Generative () *)
  module Input = Input

  (* Backend-specific random state initialized when functor is instantiated *)
  let random_state = Random.State.make_self_init ()
  let get_trading_client _ = Result.fail @@ `MissingClient "Trading"
  let get_data_client _ = Result.fail @@ `MissingClient "Data"

  let init_state () =
    let ( let* ) = Result.( let* ) in
    let* bars =
      match Input.target with
      | None -> Error.fatal "No target for backtest"
      | Some b -> Result.return b
    in
    let config =
      Indicators_config.make_with_print_tick Input.options.flags.runtype
        Input.options.tacaml_indicators Input.options.flags.print_tick_arg
    in
    State.make Input.options.flags.start bars config 100000.0
      Input.options.flags.print_tick_arg

  let opts = Input.options

  (* let mutices = Longleaf_state.Mutex.create () *)

  (* let context = Input.options.context *)
  let next_market_open _ = Ok None
  let next_market_close _ = Ok Ptime.max
  let symbols = List.map Instrument.security opts.symbols
  let is_backtest = true
  let shutdown () = ()
  let prepare_live_trading _state = Ok () (* No-op for backtesting *)

  (* let overnight = Input.options.overnight *)
  let save_received = opts.flags.save_received

  let place_order state order =
    let random_drop order =
      match Input.options.flags.random_drop_chance with
      | 0 -> Some order
      | n ->
        assert (n >= 0);
        assert (n <= 100);
        let ratio = Float.(of_int n / 100.0) in
        let flip = Random.float_range 0.0 1.0 @@ random_state in
        if flip >=. ratio then Some order
        else
          let tick = State.tick state in
          Eio.traceln "[%d] Not placing an order due to random chance" tick;
          None
    in
    let price_modifier (order : Order.t) =
      match Input.options.flags.slippage_pct with
      | 0.0 -> order
      | pct ->
        assert (pct >=. 0.0);
        assert (pct <=. 1.0);
        let minmod, maxmod = (1.0 -. pct, 1.0 +. pct) in
        let pricemin, pricemax =
          (minmod *. order.price, maxmod *. order.price)
        in
        let slipped_price =
          Random.float_range pricemin pricemax @@ random_state
        in
        { order with price = slipped_price }
    in
    price_modifier order |> random_drop |> function
    | None -> Result.return state
    | Some order -> State.place_order state order

  let received_data = Bars.empty ()
  let target = Input.target

  (* WORKAROUND: Define as function to defer evaluation until runtime.
     Calling Saturn.Htbl.to_seq during functor body evaluation causes
     Eio Hmap.find to raise Not_found. Deferring avoids this issue. *)
  let last_data_bar () =
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
        Bars.Latest.set last_data_bar instrument col;
        assert (not @@ Float.is_nan @@ Data.Column.last_exn col);
        Result.return ()
      in
      Result.return last_data_bar

  let update_bars state = Result.return state
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

  let liquidate (state : State.t) =
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
            let cost_basis = State.cost_basis prev symbol in
            let profit =
              if qty > 0 then
                (* Selling to close long position *)
                Some ((Float.of_int abs_qty *. last_price) +. cost_basis)
              else
                (* Buying to close short position *)
                Some ((Float.of_int abs_qty *. last_price *. -1.0) +. cost_basis)
            in
            let order : Order.t =
              Order.make ~symbol ~tick ~side ~tif:TimeInForce.GoodTillCanceled
                ~order_type:OrderType.Market ~qty:abs_qty ~price:last_price
                ~timestamp ~profit ~reason:[ "Liquidate position" ]
            in
            (* Eio.traceln "@[Liquidating %d shares of %a at %f@]@." abs_qty *)
            (*   Instrument.pp symbol last_price; *)
            place_order prev order)
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

  let reset_websocket () = () (* No-op for backtesting *)
end
