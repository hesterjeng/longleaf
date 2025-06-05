open Backend_intf

module Make (Input : BACKEND_INPUT) : S = struct
  (* module Ticker = Ticker.Instant *)
  (* module Backend_position = Backend_position.Generative () *)
  module Input = Input

  let get_trading_client _ = Result.fail @@ `MissingClient "Trading"
  let get_data_client _ = Result.fail @@ `MissingClient "Data"

  let init_state content =
    Result.return
    @@ {
         State.current = Initialize;
         bars = Input.bars;
         latest = Bars.Latest.empty ();
         content;
         tick = 0;
         tick_length = Input.options.tick;
         stats = Stats.empty ();
         order_history = Order.History.empty;
         (* indicators = Input.options.context.indicators; *)
         (* (match Input.options.context.indicators.ty with *)
         (* | Options.IndicatorType.Live -> Indicators.empty Live *)
         (* | Precomputed -> Indicators.empty Precomputed); *)
         positions = Backend_position.make () (* active_orders = []; *);
         time = Ptime.min;
       }

  let context = Input.options.context
  let next_market_open _ = Ok None
  let next_market_close _ = Ok Ptime.max
  let env = context.eio_env
  let symbols = List.map Instrument.security Input.options.symbols
  let is_backtest = true
  let shutdown () = ()
  let overnight = Input.options.overnight
  let save_received = context.save_received

  (* Ordered in reverse time order when INPUT is created *)
  let data_remaining : Bars.Latest.t Queue.t =
    match Input.target with
    | Some b -> b
    | None ->
      Eio.traceln "Creating empty data_remaining for backtesting backend?";
      Queue.create ()

  let place_order = State.place_order
  let received_data = Bars.empty ()

  let latest_bars _ =
    let ( let* ) = Result.( let* ) in
    let latest : Bars.Latest.t = Bars.Latest.empty () in
    let* () =
      Bars.fold data_remaining (Ok ()) @@ fun symbol vector prev ->
      let* _ = prev in
      Vector.pop vector |> function
      | None -> Error.missing_data "backtesting_backend.ml:latest_bars"
      | Some value ->
        Bars.Latest.set latest symbol value;
        Result.return @@ ()
    in
    Result.return latest

  let last_data_bar =
    Eio.traceln "@[Creating last data bar.@]";
    (* let module Hashtbl = Bars.Hashtbl in *)
    let ( let* ) = Result.( let* ) in
    (* let tbl : Bars.Latest.t = Hashtbl.create 20 in *)
    let tbl = Bars.Latest.empty () in
    let* target =
      match Input.target with
      | Some x -> Ok x
      | None -> Error.missing_data "No target to create last data bar"
    in
    let res =
      Bars.fold target (Ok tbl)
      @@
      (* Hashtbl.to_seq target *)
      (* |> *)
      (* let fold f = Seq.fold f (Ok tbl) in *)
      (* fold @@ *)
      fun symbol vector ok ->
      let* _ = ok in
      let l = Vector.length vector in
      match l with
      | 0 -> Error.missing_data @@ Instrument.symbol symbol
      | _ ->
        Result.return
        @@
        let item = Vector.get vector 0 in
        Bars.Latest.set tbl symbol item;
        tbl
    in
    res

  let liquidate (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    let* last = last_data_bar in
    let* new_positions = Backend_position.liquidate state.positions last in
    Result.return @@ { state with positions = new_positions }
end
