open Backend_intf

module Make (Input : BACKEND_INPUT) : S = struct
  (* module Ticker = Ticker.Instant *)
  module Backend_position = Backend_position.Generative ()
  module Input = Input

  let get_trading_client _ = Error "Backtesting does not have a trading client"
  let get_data_client _ = Error "Backtesting does not have a data client"

  let init_state content =
    {
      State.current = `Initialize;
      bars = Input.bars;
      latest = Bars.Latest.empty ();
      content;
      tick = 0;
      stats = Stats.empty;
      order_history = Vector.create ();
      indicators = Indicators.empty ();
      active_orders = [];
    }

  let next_market_open _ = None
  let next_market_close _ = Ptime.max
  let env = Input.context.eio_env
  let symbols = Input.config.symbols
  let is_backtest = true
  let shutdown () = ()
  let get_cash = Backend_position.get_cash
  let overnight = Input.config.overnight
  let save_received = Input.context.save_received

  let place_order state (order : Order.t) =
    Eio.traceln "@[%a@]@." Order.pp order;
    Backend_position.execute_order state order

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
         | None -> Some "Empty vector when trying to collect data"
         | Some value ->
             (* Eio.traceln "There are %d members remaining in bar %s." *)
             (*   (Vector.length vector) symbol; *)
             Hashtbl.replace latest symbol value;
             None
    in
    match found with Some err -> Error err | None -> Ok latest

  let last_data_bar =
    Eio.traceln "@[Creating last data bar.@]";
    let module Hashtbl = Bars.Hashtbl in
    let ( let* ) = Result.( let* ) in
    let tbl : Bars.Latest.t = Hashtbl.create 20 in
    let* target = Option.to_result "No target for last data bar" Input.target in
    let res =
      Hashtbl.to_seq target
      |>
      let fold f = Seq.fold f (Ok tbl) in
      fold @@ fun ok (symbol, vector) ->
      let* _ = ok in
      let l = Vector.length vector in
      match l with
      | 0 -> Error "No data for symbol in last_data_bar?"
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
