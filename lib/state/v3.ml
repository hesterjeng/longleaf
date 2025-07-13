type state =
  | Initialize
  | Listening
  | Ordering
  | Liquidate
  | LiquidateContinue
  | Continue
  | BeginShutdown
  | Finished of string
[@@deriving show { with_path = false }]

module State_config = struct
  type t = { placeholder : bool }
end

module type S = sig
  type 'a t
  type 'a res = ('a, Error.t) result

  (* The current "state of the state" *)
  val current : 'a t -> state

  (* Create a new state with index, data, and content *)
  val make : int -> Bars.t -> 'a -> 'a t res

  (* Print basic info about the state *)
  val pp : 'a t Format.printer

  (* Export the state to a simple string *)
  val show : 'a t -> string

  (* Get the amount of cash available.  This value is updated only when orders are executed.*)
  val cash : 'a t -> float res

  (* Get the time of the state machine.  Uses Bars.timestamp *)
  val time : 'a t -> Time.t res

  (* Get list of active orders *)
  val orders : 'a t -> Order.t list Vector.ro_vector

  (* Get the data of the state for this symbol using Bars.get. *)
  val data : 'a t -> Instrument.t -> (Bars.Data.t, Error.t) result

  (* Get the value of the state's portfolio, plus the cash on hand *)
  val value : 'a t -> float res

  (* The same state but with with x.state set to Listening *)
  val listen : 'a t -> 'a t

  (* Place an order.  Assume that orders are filled completely and instantly at the current price. *)
  val place_order : 'a t -> Order.t -> 'a t res

  (* Current tick of the state machine (corresponds to the Bars.t index) *)
  val tick : 'a t -> int

  (* Return the options record *)
  val options : 'a t -> State_config.t
end

module V3_impl : S = struct
  type 'a t = {
    current_state : state;
    bars : Bars.t;
    current_tick : int;
    content : 'a;
    config : State_config.t;
    cash : float;
    orders : Order.t list;
  }
  [@@warning "-69"]

  type 'a res = ('a, Error.t) result

  let current t = t.current_state

  let make tick bars content =
    let config = State_config.{ placeholder = true } in
    Result.return
      {
        current_state = Initialize;
        bars;
        current_tick = tick;
        content;
        config;
        cash = 100000.0;
        orders = [];
      }

  let pp fmt t =
    Format.fprintf fmt "V3State(tick=%d, state=%a, cash=%.2f)" t.current_tick
      pp_state t.current_state t.cash

  let show t = Format.asprintf "%a" pp t
  let cash t = Result.return t.cash
  let time t = Bars.timestamp t.bars
  let orders t = Vector.of_list [ t.orders ]
  let data t instrument = Bars.get t.bars instrument

  let value t =
    let portfolio_value =
      List.fold_left
        (fun acc (order : Order.t) ->
          if Order.Status.equal (Pmutex.get order.status) Order.Status.Filled
          then
            match Bars.get t.bars order.symbol with
            | Ok data -> (
              let current_price = Bars.Data.get data Last t.current_tick in
              let order_value = float_of_int order.qty *. current_price in
              match order.side with
              | Buy -> acc +. order_value
              | Sell -> acc -. order_value)
            | Error _ -> acc
          else acc)
        0.0 t.orders
    in
    Result.return (t.cash +. portfolio_value)

  let listen t = { t with current_state = Listening }

  let place_order t (order : Order.t) =
    let ( let* ) = Result.( let* ) in
    let* data = Bars.get t.bars order.symbol in
    let current_price = Bars.Data.get data Last t.current_tick in
    let order_value = float_of_int order.qty *. current_price in

    let _ = Pmutex.set order.status Order.Status.Filled in

    match order.side with
    | Buy ->
      if t.cash >=. order_value then
        let new_cash = t.cash -. order_value in
        Result.return { t with cash = new_cash; orders = order :: t.orders }
      else Error.fatal "Insufficient cash for buy order"
    | Sell ->
      let current_qty =
        List.fold_left
          (fun acc (ord : Order.t) ->
            if
              Order.Status.equal (Pmutex.get ord.status) Order.Status.Filled
              && Instrument.equal ord.symbol order.symbol
            then
              match ord.side with
              | Buy -> acc + ord.qty
              | Sell -> acc - ord.qty
            else acc)
          0 t.orders
      in
      if current_qty >= order.qty then
        let new_cash = t.cash +. order_value in
        Result.return { t with cash = new_cash; orders = order :: t.orders }
      else Error.fatal "Insufficient shares for sell order"

  let tick t = t.current_tick
  let options t = t.config
end
