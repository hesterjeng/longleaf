open Trading_types

module Status = struct
  type t = Placed of (int * Order.t) | Waiting [@@deriving show]
end

type state = Status.t State.t

module BuyReason = struct
  type t = Above3StdBollinger of string

  let make (state : state) symbol =
    let open Option.Infix in
    let current_price = Bars.Latest.get state.latest symbol |> Item.last in
    let* upper_bb =
      Indicators.indicator state.indicators symbol
        Indicators.Point.upper_bollinger_100_3
    in
    let sma75spy =
      Indicators.indicator state.indicators "SPY" Indicators.Point.sma_75
      |> Option.get_exn_or "Must be able to get SPY SMA 75"
    in
    let spy_price = Bars.Latest.get state.latest "SPY" |> Item.last in
    let pass = current_price >=. upper_bb && not (spy_price <=. sma75spy) in
    match pass with true -> Some (Above3StdBollinger symbol) | false -> None
end

module SellReason = struct
  type t = Below1StdBollinger of string
  [@@deriving show { with_path = false }]

  let make (state : state) symbol =
    let open Option.Infix in
    let current_price = Bars.Latest.get state.latest symbol |> Item.last in
    let* lower_bb =
      Indicators.indicator state.indicators symbol
        Indicators.Point.lower_bollinger_100_1
    in
    let* sma75spy =
      Indicators.indicator state.indicators "SPY" Indicators.Point.sma_75
    in
    let spy_price = Bars.Latest.get state.latest "SPY" |> Item.last in
    let pass = current_price <=. lower_bb && not (spy_price <=. sma75spy) in
    match pass with true -> Some (Below1StdBollinger symbol) | false -> None
end

module Make (Backend : Backend.S) : Strategy.S = struct
  module SU = Strategy_utils.Make (Backend)

  module Order = struct
    include Trading_types.Order

    let of_buy_reason (state : state) (x : BuyReason.t) =
      match x with
      | Above3StdBollinger symbol ->
          let current_cash = Backend.get_cash () in
          let item = Bars.Latest.get state.latest symbol in
          let side = Side.Buy in
          let tif = TimeInForce.GoodTillCanceled in
          let order_type = OrderType.Market in
          let price = Item.last item in
          let qty = Math.qty ~current_cash ~pct:1.0 ~price in
          let timestamp = Item.timestamp item in
          let reason =
            [
              Format.asprintf "Buying (%a): Above 3 std bollinger band" Time.pp
                timestamp;
            ]
          in
          Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp
            ~reason ~profit:None

    let of_sell_reason ~(buying_order : Order.t) (state : state)
        (x : SellReason.t) =
      match x with
      | Below1StdBollinger symbol ->
          let item = Bars.Latest.get state.latest symbol in
          let side = Side.Sell in
          let tif = TimeInForce.GoodTillCanceled in
          let order_type = OrderType.Market in
          let price = Item.last item in
          let qty = buying_order.qty in
          let timestamp = Item.timestamp item in
          let profit =
            Float.of_int buying_order.qty *. (price -. buying_order.price)
          in
          let reason =
            [
              Format.asprintf "Selling (%a): Below 1 std bollinger band" Time.pp
                timestamp;
            ]
          in
          Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp
            ~reason ~profit:(Some profit)
  end

  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let init_state = Backend.init_state Status.Waiting

  let buy (state : state) =
    let open Result.Infix in
    let passes = List.filter_map (BuyReason.make state) Backend.symbols in
    let orders = List.map (Order.of_buy_reason state) passes in
    (* Just select the first one for now *)
    let+ content =
      match List.head_opt orders with
      | None -> Ok state.content
      | Some order ->
          let* () = Backend.place_order state order in
          Result.return @@ Status.Placed (0, order)
    in
    { state with State.current = `Listening; content }

  let sell (state : state) ~(buying_order : Order.t) ~time_held =
    let open Result.Infix in
    match SellReason.make state buying_order.symbol with
    | None ->
        Result.return
        @@ {
             state with
             State.current = `Listening;
             content = Status.Placed (time_held + 1, buying_order);
           }
    | Some reason ->
        let order = Order.of_sell_reason ~buying_order state reason in
        let* () = Backend.place_order state order in
        Result.return
        @@ { state with State.current = `Listening; content = Status.Waiting }

  let step (state : state) =
    let current = state.current in
    (* Eio.traceln "@[buylowbollinger: %a@]@." State.pp_state current; *)
    match current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        (* Eio.traceln "@[%a@]@." DT_Status.pp state.content; *)
        match state.content with
        | Waiting -> buy state
        | Placed (time_held, buying_order) ->
            sell ~buying_order ~time_held state)

  let run () = SU.run ~init_state step
end
