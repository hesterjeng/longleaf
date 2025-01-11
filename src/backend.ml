module Order = Trading_types.Order

module Run_options = struct
  type t = {
    symbols : string list;
    tick : float;
    overnight : bool;
    resume_after_liquidate : bool;
    runtype : Options.Runtype.t;
    indicators_config : Indicators.Config.t;
  }
end

module Run_context = struct
  type t = {
    eio_env : Eio_unix.Stdenv.base;
    longleaf_env : Environment.t;
    switch : Eio.Switch.t;
    preload : Options.Preload.t;
    target : string option;
    save_received : bool;
    mutices : Longleaf_mutex.t;
  }
end

module type BACKEND_INPUT = sig
  val switch : Eio.Switch.t
  val longleaf_env : Environment.t
  val eio_env : Eio_unix.Stdenv.base

  (* Historical information, ordered with in time order *)
  val bars : Bars.t

  (* The symbols that will be traded on *)
  val symbols : string list

  (* The interval of time that the strategy operates on. *)
  (* i/e we will wait ten minutes then do something, etc. *)
  val tick : float

  (* Allow holding positions overnight *)
  val overnight : bool

  (* Save the received data *)
  val save_received : bool

  (* Allow the strategy to resume after liquidating the position *)
  val resume_after_liquidate : bool

  (* The target is the bars that will be iterated over in a backtest *)
  (* Ordered in reverse time order, so that we can pop off next values easily *)
  val target : Bars.t option

  (* Mutices for delivering information to GUI *)
  val mutices : Longleaf_mutex.t

  (* Record of options *)
  val runtype : Options.Runtype.t

  (* Indicators options *)
  val indicators_config : Indicators.Config.t
end

module type S = sig
  module Backend_position : Backend_position.S
  module Input : BACKEND_INPUT

  (* Is this backend a backtest? *)
  val is_backtest : bool

  (* TODO: Do something with this? *)
  val overnight : bool

  (* Save data that is received in a live/paper run *)
  (* val save_received : bool *)
  val received_data : Bars.t
  val get_trading_client : unit -> (Piaf.Client.t, string) result
  val get_data_client : unit -> (Piaf.Client.t, string) result
  val env : Eio_unix.Stdenv.base
  val init_state : 'a -> 'a State.t
  val get_cash : unit -> float
  val symbols : string list
  val shutdown : unit -> unit

  (* Return the next open time if the market is closed *)
  val next_market_open : unit -> Time.t option
  val next_market_close : unit -> Time.t
  val place_order : _ State.t -> Order.t -> (unit, string) result
  val latest_bars : string list -> (Bars.Latest.t, string) result
  val last_data_bar : (Bars.Latest.t, string) result
  val liquidate : _ State.t -> (unit, string) Result.t
end

(* Backtesting *)
module Backtesting (Input : BACKEND_INPUT) : S = struct
  open Trading_types

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
      stats = Stats.empty;
      order_history = Vector.create ();
      indicators = Indicators.empty ();
    }

  let next_market_open _ = None
  let next_market_close _ = Ptime.max
  let env = Input.eio_env
  let symbols = Input.symbols
  let is_backtest = true
  let shutdown () = ()
  let get_cash = Backend_position.get_cash
  let overnight = Input.overnight
  let save_received = Input.overnight

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

(* Live trading *)
module Alpaca (Input : BACKEND_INPUT) : S = struct
  open Trading_types
  module Backtesting = Backtesting (Input)
  module Backend_position = Backtesting.Backend_position
  module Input = Input

  let get_cash = Backend_position.get_cash
  let env = Input.eio_env
  let overnight = Input.overnight
  let save_received = Input.save_received
  let received_data = Bars.empty ()

  let trading_client =
    let res =
      Piaf.Client.create ~sw:Input.switch Input.eio_env
        Input.longleaf_env.apca_api_base_url
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create trading client"

  let tiingo_client = Tiingo_api.tiingo_client Input.eio_env Input.switch

  module Tiingo_client : Util.CLIENT = struct
    let longleaf_env = Input.longleaf_env
    let client = tiingo_client
  end

  module Tiingo = Tiingo_api.Make (Tiingo_client)

  let data_client =
    let res =
      Piaf.Client.create ~sw:Input.switch Input.eio_env
        Input.longleaf_env.apca_api_data_url
    in
    match res with
    | Ok x -> x
    | Error _ -> invalid_arg "Unable to create data client"

  let get_trading_client _ = Ok trading_client
  let get_data_client _ = Ok data_client

  module Trading_api = Trading_api.Make (struct
    let client = trading_client
    let longleaf_env = Input.longleaf_env
  end)

  module Market_data_api = Market_data_api.Make (struct
    let client = data_client
    let longleaf_env = Input.longleaf_env
  end)

  let init_state content =
    let account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    let account_cash = account_status.cash in
    Backend_position.set_cash account_cash;
    {
      State.current = `Initialize;
      bars = Input.bars;
      latest = Bars.Latest.empty ();
      content;
      stats = Stats.empty;
      order_history = Vector.create ();
      indicators = Indicators.empty ();
    }

  let next_market_open () =
    let clock = Trading_api.Clock.get () in
    if clock.is_open then None else Some clock.next_open

  let next_market_close () =
    let clock = Trading_api.Clock.get () in
    clock.next_close

  let shutdown () =
    Eio.traceln "Alpaca backend shutdown";
    Piaf.Client.shutdown trading_client;
    Piaf.Client.shutdown data_client;
    Piaf.Client.shutdown tiingo_client;
    ()

  let symbols = Input.symbols
  let is_backtest = false
  let get_account = Trading_api.Accounts.get_account
  let last_data_bar = Error "No last data bar in Alpaca backend"

  let latest_bars symbols =
    match symbols with
    | [] ->
        Eio.traceln "No symbols in latest bars request.";
        Result.return @@ Bars.Latest.empty ()
    | _ ->
        let _ = Backtesting.latest_bars symbols in
        (* let res = Market_data_api.Stock.latest_bars symbols in *)
        let res = Tiingo.latest symbols in
        if save_received then Bars.append res received_data;
        Ok res

  let get_clock = Trading_api.Clock.get

  let place_order state order =
    let ( let* ) = Result.( let* ) in
    let* () = Backtesting.place_order state order in
    Trading_api.Orders.create_market_order order

  let liquidate state =
    let ( let* ) = Result.( let* ) in
    let symbols = Backend_position.symbols () in
    let* last_data_bar = latest_bars symbols in
    let _ =
      List.iter
        (fun symbol ->
          let qty = Backend_position.qty symbol in
          if qty = 0 then ()
          else
            let latest_info = Bars.Latest.get last_data_bar symbol in
            let order : Order.t =
              let side = if qty >= 0 then Side.Sell else Side.Buy in
              let tif = TimeInForce.GoodTillCanceled in
              let order_type = OrderType.Market in
              let qty = Int.abs qty in
              let price = Item.last latest_info in
              let timestamp = Item.timestamp latest_info in
              Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp
                ~profit:None ~reason:[ "Liquidate" ]
            in
            (* Eio.traceln "%a" Order.pp order; *)
            let _json_resp = place_order state order in
            ())
        symbols
    in
    let account_status = Trading_api.Accounts.get_account () in
    Eio.traceln "@[Account status:@]@.@[%a@]@." Trading_api.Accounts.pp
      account_status;
    Ok ()
end

let make_backend_input (options : Run_options.t) (context : Run_context.t) =
  (module struct
    let switch = context.switch
    let longleaf_env = context.longleaf_env
    let eio_env = context.eio_env
    let save_received = context.save_received
    let mutices = context.mutices
    let symbols = options.symbols
    let overnight = options.overnight
    let resume_after_liquidate = options.resume_after_liquidate
    let tick = options.tick
    let runtype = options.runtype
    let indicators_config = options.indicators_config

    (* Target *)
    let target =
      let ( let+ ) = Option.( let+ ) in
      let+ res =
        context.target
        |> Option.map @@ fun f -> Yojson.Safe.from_file f |> Bars.t_of_yojson
      in
      Bars.sort (Ord.opp Item.compare) res;
      res

    (* Preload *)
    let bars =
      match context.preload with
      | None -> Bars.empty ()
      | Download -> invalid_arg "Downloading data for preload NYI"
      | File file ->
          Eio.traceln "Preloading bars from %s" file;
          let res = Yojson.Safe.from_file file |> Bars.t_of_yojson in
          Bars.sort Item.compare res;
          res
  end : BACKEND_INPUT)

let create_backend (options : Run_options.t) (context : Run_context.t) =
  let module Input = (val make_backend_input options context) in
  match options.runtype with
  | Live -> invalid_arg "Live trading not implemented"
  | Manual -> invalid_arg "Cannot create a strategy with manual runtype"
  | Paper ->
      Eio.traceln "@[create_backend: Creating Alpaca backend@]@.";
      (module Alpaca (Input) : S)
  | Backtest ->
      Eio.traceln "@[create_backend: Creating Backtesting backend@]@.";
      (module Backtesting (Input))
