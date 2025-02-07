open Backend_intf

module Make (Input : BACKEND_INPUT) : S = struct
  (* module Ticker = Ticker.Instant *)
  module Backend_position = Backend_position.Generative ()
  module Input = Input

  let get_trading_client _ = Result.fail @@ `MissingClient "Trading"
  let get_data_client _ = Result.fail @@ `MissingClient "Data"

  let init_state content =
    Result.return
    @@ {
         State.current = `Initialize;
         bars = Input.bars;
         latest = Bars.Latest.empty ();
         content;
         tick = 0;
         stats = Stats.empty;
         order_history = Vector.create ();
         indicators = Indicators.empty ();
         (* active_orders = []; *)
       }

  let context = Input.options.context
  let next_market_open _ = Ok None
  let next_market_close _ = Ok Ptime.max
  let env = context.eio_env
  let symbols = Input.options.symbols
  let is_backtest = true
  let shutdown () = ()
  let get_cash = Backend_position.get_cash
  let overnight = Input.options.overnight
  let save_received = context.save_received

  let place_order state (order : Order.t) =
    let ( let* ) = Result.( let* ) in
    Eio.traceln "@[%a@]@." Order.pp order;
    let* () = Backend_position.execute_order state order in
    Result.return state

  (* Ordered in reverse time order when INPUT is created *)
  let data_remaining =
    match Input.target with
    | Some b -> b
    | None ->
        Eio.traceln "Creating empty data_remaining for backtesting backend.";
        Bars.empty ()

  let received_data = Bars.empty ()

  let latest_bars _ =
    let module Hashtbl = Bars.Hashtbl in
    let latest : Bars.Latest.t =
      Hashtbl.create (Hashtbl.length data_remaining)
    in
    let found =
      Hashtbl.to_seq data_remaining
      |> Seq.find_map @@ fun (symbol, vector) ->
         Vector.pop vector |> function
         | None ->
             Option.return @@ `MissingData "backtesting_backend.ml:latest_bars"
         | Some value ->
             Hashtbl.replace latest symbol value;
             None
    in
    match found with Some err -> Error err | None -> Ok latest

  let last_data_bar =
    Eio.traceln "@[Creating last data bar.@]";
    let module Hashtbl = Bars.Hashtbl in
    let ( let* ) = Result.( let* ) in
    let tbl : Bars.Latest.t = Hashtbl.create 20 in
    let* target =
      match Input.target with
      | Some x -> Ok x
      | None -> Result.fail @@ `MissingData "No target to create last data bar"
    in
    let res =
      Hashtbl.to_seq target
      |>
      let fold f = Seq.fold f (Ok tbl) in
      fold @@ fun ok (symbol, vector) ->
      let* _ = ok in
      let l = Vector.length vector in
      match l with
      | 0 ->
          Error (`MissingData symbol)
          (* "No data for symbol in last_data_bar?" *)
      | _ ->
          (* Eio.traceln "@[%a@]@." (Vector.pp Item.pp) vector; *)
          Result.return
          @@
          let item = Vector.get vector 0 in
          Hashtbl.replace tbl symbol item;
          tbl
    in
    (* Eio.traceln "@[Result last data bar.@]"; *)
    (* Eio.traceln "@[last: %a@]@." (Result.pp Bars.Latest.pp) res; *)
    (* invalid_arg "debug" *)
    res

  let liquidate state =
    let ( let* ) = Result.( let* ) in
    let* last = last_data_bar in
    let* () = Backend_position.liquidate state last in
    Ok ()
end
