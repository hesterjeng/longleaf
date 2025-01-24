module type TRIGGER = sig
  val make : 'a State.t -> string -> Signal.Flag.t
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
    (BuyTrigger : TRIGGER)
    (SellTrigger : TRIGGER)
    (Backend : Backend.S) : Strategy.S = struct
  module SU = Strategy_utils.Make (Backend)

  module Status = struct
    type t = Order.t list [@@deriving show]
  end

  type state = Status.t State.t

  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let init_state = Backend.init_state []

  let buy (state : state) =
    let open Result.Infix in
    (* let passes = List.filter_map (BuyReason.make state) Backend.symbols in *)
    let passes =
      List.filter_map
        (fun symbol ->
          match Buy.make state symbol with
          | Pass reason -> Some (symbol, reason)
          | Fail _ -> None)
        Backend.symbols
    in
    let selected = Util.random_choose_opt passes in
    let+ content =
      match selected with
      | None -> Ok state.content
      | Some (symbol, reason) ->
          let price = Signal.Indicator.price state symbol in
          let timestamp = Signal.Indicator.timestamp state symbol in
          let current_cash = Backend.get_cash () in
          let qty = Util.qty ~current_cash ~price ~pct:1.0 in
          let order : Order.t =
            Order.make ~symbol ~side:Buy ~tif:GoodTillCanceled
              ~order_type:Market ~qty ~price ~reason ~timestamp ~profit:None
          in
          let* () = Backend.place_order state order in
          Result.return @@ (order :: state.content)
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
