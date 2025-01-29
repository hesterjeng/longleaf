(* This module contains a functor for making strategies that buy at most n stock at a time. *)
(* The stocks to buy will be filtered by a pass function, then the n with the highest score will *)
(* be selected for purchase. *)
(* Any held stock that meets the sell criterion will be sold. *)
(* This template will only hold n at a time. *)

module Buy_trigger = struct
  module type S = sig
    val make : 'a State.t -> string list -> Signal.t list
    val num_positions : int
  end

  module type INPUT = sig
    val pass : 'a State.t -> string -> Signal.Flag.t
    val score : 'a State.t -> string -> float
    val num_positions : int
  end

  (* Using the Input module, create a module with a function to select the stocks *)
  (* with the highest score for buying. *)
  module Make (Input : INPUT) = struct
    let make state symbols =
      List.filter_map
        (fun symbol ->
          match Input.pass state symbol with
          | Pass reason ->
              let score = Input.score state symbol in
              Some { Signal.symbol; reason; score }
          | Fail _ -> None)
        symbols
      |> List.sort Signal.compare |> List.rev

    let num_positions = Input.num_positions
  end
end

module Sell_trigger = struct
  (* Pass if we meet the sell conditions, sell otherwise *)
  module type S = sig
    val make : 'a State.t -> buying_order:Order.t -> Signal.Flag.t
  end
end

module Make
    (Buy : Buy_trigger.S)
    (Sell : Sell_trigger.S)
    (Backend : Backend.S) : Strategy.S = struct
  module SU = Strategy_utils.Make (Backend)

  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let init_state = Backend.init_state []

  let buy (state : 'a State.t) =
    let open Result.Infix in
    let held_symbols = Backend.Backend_position.symbols () in
    let potential_buys =
      (* Get the potential symbols to purchase and don't rebuy into ones we already hold *)
      Buy.make state Backend.symbols
      |> List.filter (fun (x : Signal.t) ->
             not @@ List.mem x.symbol held_symbols)
    in
    let num_held_currently = List.length state.active_orders in
    assert (Buy.num_positions >= 0);
    assert (Buy.num_positions >= num_held_currently);
    let selected =
      (* Only buy up to the number of positions we are allowed to take *)
      (* I/e if we already have 2 positions and are only allowed 5, only do 3 new ones. *)
      List.take
        (Buy.num_positions - List.length state.active_orders)
        potential_buys
    in
    match selected with
    | [] -> Result.return { state with State.current = `Listening }
    | selected ->
        let current_cash = Backend.get_cash () in
        let pct = 1.0 /. Float.of_int (List.length selected) in
        let place_order state (signal : Signal.t) =
          let* state = state in
          let symbol = signal.symbol in
          let reason = signal.reason in
          let price = State.price state symbol in
          let timestamp = State.timestamp state symbol in
          let qty = Util.qty ~current_cash ~price ~pct in
          (* assert (qty <> 0); *)
          match qty with
          | 0 -> Result.return { state with State.current = `Listening }
          | qty ->
              let order : Order.t =
                Order.make ~symbol ~side:Buy ~tif:GoodTillCanceled
                  ~tick:state.tick ~order_type:Market ~qty ~price ~reason
                  ~timestamp ~profit:None
              in
              let+ () = Backend.place_order state order in
              {
                state with
                State.current = `Listening;
                active_orders = order :: state.active_orders;
              }
        in
        List.fold_left place_order (Ok state) selected

  let sell (state : 'a State.t) ~(buying_order : Order.t) =
    let open Result.Infix in
    match Sell.make state ~buying_order with
    | Fail _ -> Result.return @@ { state with State.current = `Listening }
    | Pass reason ->
        let price = State.price state buying_order.symbol in
        let timestamp = State.timestamp state buying_order.symbol in
        assert (buying_order.qty <> 0);
        let reason =
          ("Sell reason:" :: reason) @ ("Buy reason:" :: buying_order.reason)
        in
        let order : Order.t =
          Order.make ~tick:state.tick ~symbol:buying_order.symbol ~side:Sell
            ~tif:GoodTillCanceled ~order_type:Market ~qty:buying_order.qty
            ~price ~reason ~timestamp
            ~profit:
              (Option.return
              @@ (Float.of_int buying_order.qty *. (price -. buying_order.price))
              )
        in
        let* () = Backend.place_order state order in
        let new_active_orders =
          List.filter
            (fun x -> not @@ Order.equal x buying_order)
            state.active_orders
        in
        Result.return
        @@ {
             state with
             State.current = `Listening;
             active_orders = new_active_orders;
           }

  let sell_fold state buying_order =
    let ( let* ) = Result.( let* ) in
    let* state = state in
    let* res = sell state ~buying_order in
    Result.return res

  let step (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    let current = state.current in
    match current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering ->
        let* sold_state =
          List.fold_left sell_fold (Ok state) state.active_orders
        in
        let* complete = buy sold_state in
        Result.return { complete with tick = complete.tick + 1 }

  let run () = SU.run ~init_state step
end
