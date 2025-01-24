open Trading_types

module type TRIGGER = sig
  val make : 'a State.t -> string -> Signal.Flag.t

  (* val to_order : *)
  (*   price:float -> *)
  (*   current_cash:float -> *)
  (*   symbol:string -> *)
  (*   timestamp:Time.t -> *)
  (*   Signal.Flag.t -> *)
  (*   Order.t option *)
end

module Buy : TRIGGER = struct
  let make (state : 'a State.t) symbol =
    let price = Signal.Indicator.price state symbol in
    Signal.Flag.conjunction state
    @@ [ Signal.Indicator.lower_bb symbol Below price ]
end

module Sell : TRIGGER = struct
  let make (state : 'a State.t) symbol =
    let price = Signal.Indicator.price state symbol in
    Signal.Flag.conjunction state
    @@ [ Signal.Indicator.lower_bb symbol Above price ]
end

module Make
    (Backend : Backend.S)
    (BuyTrigger : TRIGGER)
    (SellTrigger : TRIGGER) : Strategy.S = struct
  module SU = Strategy_utils.Make (Backend)


  module Status = struct
    type t = Order.t list [@@deriving show]
  end

  type state = Status.t State.t
  (* module Order = struct *)
  (*   include Trading_types.Order *)

  (*   let of_buy_reason (state : state) (x : BuyReason.t) = *)
  (*     let ( let+ ) = Option.( let+ ) in *)
  (*     match x with *)
  (*     | { symbol; _ } -> *)
  (*         let current_cash = Backend.get_cash () in *)
  (*         let item = Bars.Latest.get state.latest symbol in *)
  (*         let side = Side.Buy in *)
  (*         let tif = TimeInForce.GoodTillCanceled in *)
  (*         let order_type = OrderType.Market in *)
  (*         let price = Item.last item in *)
  (*         let+ qty = *)
  (*           match Math.qty ~current_cash ~pct:1.0 ~price with *)
  (*           | 0 -> None *)
  (*           | qty -> Some qty *)
  (*         in *)
  (*         let timestamp = Item.timestamp item in *)
  (*         let reason = *)
  (*           [ *)
  (*             Format.asprintf "Buying %d (%a): Above 3 std bollinger band" qty *)
  (*               Time.pp timestamp; *)
  (*           ] *)
  (*         in *)
  (*         Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp *)
  (*           ~reason ~profit:None *)

  (*   let of_sell_reason ~(buying_order : Order.t) (state : state) *)
  (*       (x : SellReason.t) = *)
  (*     match x with *)
  (*     | Below1StdBollinger symbol -> *)
  (*         let item = Bars.Latest.get state.latest symbol in *)
  (*         let side = Side.Sell in *)
  (*         let tif = TimeInForce.GoodTillCanceled in *)
  (*         let order_type = OrderType.Market in *)
  (*         let price = Item.last item in *)
  (*         let qty = buying_order.qty in *)
  (*         let timestamp = Item.timestamp item in *)
  (*         let profit = *)
  (*           Float.of_int buying_order.qty *. (price -. buying_order.price) *)
  (*         in *)
  (*         let reason = *)
  (*           [ *)
  (*             Format.asprintf "Selling %d (%a): Below 1 std bollinger band" qty *)
  (*               Time.pp timestamp; *)
  (*           ] *)
  (*         in *)
  (*         Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp *)
  (*           ~reason ~profit:(Some profit) *)
  (* end *)

  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let init_state = Backend.init_state []

  let buy (state : state) =
    let open Result.Infix in
    let passes = List.filter_map (BuyReason.make state) Backend.symbols in
    (* Select the symbol that is most above its bollinger band *)
    let selected =
      List.fold_left
        (fun (previous_opt : BuyReason.t option) (x : BuyReason.t) ->
          match previous_opt with
          | None -> Some x
          | Some previous ->
              if previous.amt_above <=. x.amt_above then Some previous
              else Some x)
        None passes
      |> fun selection -> Option.bind selection (Order.of_buy_reason state)
    in
    let+ content =
      match selected with
      | None -> Ok state.content
      | Some choice ->
          let* () = Backend.place_order state choice in
          Result.return @@ (choice :: state.content)
    in
    { state with State.current = `Listening; content }

  let sell (state : state) ~(buying_order : Order.t) =
    let open Result.Infix in
    match SellTrigger.make state buying_order.symbol with
    | Fail _ ->
        Result.return
        @@ { state with State.current = `Listening; content = [ buying_order ] }
    | Pass reason ->
        let price = Signal.Indicator.price state buying_order.symbol in
        let timestamp = Signal.Indicator.timestamp state buying_order.symbol in
        let order : Order.t =
          Order.make ~symbol:buying_order.symbol ~side:Sell
            ~tif:GoodTillCanceled ~order_type:Market ~qty:buying_order.qty
            ~price ~reason ~timestamp
            ~profit:
              (Option.return
              @@ (Float.of_int buying_order.qty *. (price -. buying_order.price))
              )
        in
        let* () = Backend.place_order state order in
        let new_content =
          List.filter (fun x -> not @@ Order.equal x buying_order) state.content
        in
        Result.return
        @@ { state with State.current = `Listening; content = new_content }

  let sell_fold state buying_order =
    let ( let* ) = Result.( let* ) in
    let* state = state in
    let* res = sell state ~buying_order in
    Result.return res

  let step (state : state) =
    let ( let* ) = Result.( let* ) in
    let current = state.current in
    (* Eio.traceln "@[buylowbollinger: %a@]@." State.pp_state current; *)
    match current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        let positions = state.content in
        let length = List.length positions in
        (* Eio.traceln "%d positions" length; *)
        match length with
        | 0 ->
            let* sold_high =
              List.fold_left sell_fold (Ok state) state.content
            in
            let* purchase = buy sold_high in
            Result.return purchase
        | _ -> List.fold_left sell_fold (Ok state) state.content)

  let run () = SU.run ~init_state step
end
