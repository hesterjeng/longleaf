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
      |> List.take Input.num_positions

    let num_positions = Input.num_positions
  end
end

module Sell_trigger = struct
  (* Pass if we meet the sell conditions, sell otherwise *)
  module type S = sig
    val make : 'a State.t -> string -> Signal.Flag.t
  end
end

module Make
    (Buy : Buy_trigger.S)
    (Sell : Sell_trigger.S)
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
    assert (Buy.num_positions >= 0);
    let n_passes = Buy.make state Backend.symbols in
    let selected =
      (* Only buy up to the number of positions we are allowed to take *)
      (* I/e if we already have 2 positions and are only allowed 5, only do 3 new ones. *)
      List.take
        (Buy.num_positions
        - (List.length @@ Backend.Backend_position.symbols ()))
        n_passes
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
          let order : Order.t =
            Order.make ~symbol ~side:Buy ~tif:GoodTillCanceled
              ~order_type:Market ~qty ~price ~reason ~timestamp ~profit:None
          in
          let+ () = Backend.place_order state order in
          {
            state with
            State.current = `Listening;
            content = order :: state.content;
          }
        in
        List.fold_left place_order (Ok state) selected

  let sell (state : state) ~(buying_order : Order.t) =
    let open Result.Infix in
    match Sell.make state buying_order.symbol with
    | Fail _ ->
        Result.return
        @@ { state with State.current = `Listening; content = [ buying_order ] }
    | Pass reason ->
        let price = State.price state buying_order.symbol in
        let timestamp = State.timestamp state buying_order.symbol in
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
    match current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering ->
        let* sold_state = List.fold_left sell_fold (Ok state) state.content in
        let* complete = buy sold_state in
        Result.return complete

  let run () = SU.run ~init_state step
end
