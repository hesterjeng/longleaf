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
    match Queue.take_opt data_remaining with
    | Some next -> Result.return next
    | None -> Error.missing_data "backtesting_backend.ml: latest_bars"

  let last_data_bar =
    Eio.traceln "@[Creating last data bar.@]";
    let ( let* ) = Result.( let* ) in
    let* target =
      match Input.target with
      | Some x -> Ok x
      | None -> Error.missing_data "No target to create last data bar"
    in
    let res = Queue.fold (fun _ latest -> Some latest) None target in
    match res with
    | Some res -> Result.return res
    | None ->
      Error.fatal "No last data bar in Backtesting_backend.last_data_bar?"

  let liquidate (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    let* last = last_data_bar in
    let* new_positions = Backend_position.liquidate state.positions last in
    Result.return @@ { state with positions = new_positions }
end
