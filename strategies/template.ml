(** This module contains a functor for making strategies that buy at most n
    stock at a time. This strategy will only buy symbols and sell them. It will
    not reenter positions that are currently active. The stocks to buy will be
    filtered by a pass function, then the n with the highest score will be
    selected for purchase. Any held stock that meets the sell criterion will be
    sold. This template will only hold n at a time. *)

module Error = Longleaf_lib.Error

(** Used as a functor argument to instantiate the strategy tmeplate. *)
module Buy_trigger = struct
  (** Module type for result of the Buy_trigger Make functor. This is used by
      the Template.Make functor. *)
  module type S = sig
    val make :
      'a State.t -> Instrument.t list -> (Signal.t list, Error.t) result

    val num_positions : int
  end

  (** The user provides a module of this type in their strategy. *)
  module type INPUT = sig
    val pass : 'a State.t -> Instrument.t -> (Signal.t option, Error.t) result
    (** Return Pass for a symbol if we want to buy it. Otherwise it returns Fail
        and we do nothing.*)

    val score : 'a State.t -> Instrument.t -> (float, Error.t) result
    (** Used to determine the symbol(s) to buy if multiple Pass. Higher is
        better. *)

    val num_positions : int
    (** The maximum number of positions the strategy will hold. If there are
        multiple positions that can be taken at a give tick, those with the best
        score are selected and cash is allocated equally to each symbol to take
        the position.*)
  end

  (** Functor whose result is used to instantiate the strategy template. *)
  module Make (Input : INPUT) : S = struct
    let make state symbols =
      let ( let* ) = Result.( let* ) in
      let fold = fun f -> Result.fold_l f [] symbols in
      let* l =
        fold @@ fun acc symbol ->
        let* signal = Input.pass state symbol in
        match signal with
        | Some s -> Result.return @@ (s :: acc)
        | None -> Result.return acc
      in
      Result.return @@ List.rev @@ List.sort Signal.compare l

    let num_positions = Input.num_positions
  end
end

module Sell_trigger = struct
  (** The user provides a module of this type to determine when to exit a
      position *)
  module type S = sig
    val make :
      'a State.t -> buying_order:Order.t -> (Signal.Flag.t, Error.t) result
    (** Return Pass if we want to exit the position corresponding to
        buying_order. If we return Fail, do nothing.*)
  end
end

(** Partially instantiate this functor with a Buy_trigger.S and Sell_trigger.S
    in your strategy file (see template_example.ml). Afterwards, a hook/handler
    must be added using the partially instantiated functor to start the strategy
    with any backend. See longleaf_strategies.ml.*)
module Make
    (Buy : Buy_trigger.S)
    (Sell : Sell_trigger.S)
    (Backend : Backend.S) : Strategy.S = struct
  module SU = Strategy_utils.Make (Backend)

  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let init_state = Backend.init_state []

  let buy ~held_symbols (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    let potential_buys =
      List.filter (fun s -> not @@ List.mem s held_symbols) Backend.symbols
      |> Buy.make state
    in
    let state =
      State.replace_stats state
      @@ Stats.add_possible_positions state.stats potential_buys
    in
    let num_held_currently = List.length @@ state.order_history.active in
    (* Eio.traceln "%d %a" Buy.num_positions (List.pp Order.pp) *)
    (*   state.order_history.active; *)
    assert (Buy.num_positions >= 0);
    assert (Buy.num_positions >= num_held_currently);
    let selected =
      (* Only buy up to the number of positions we are allowed to take *)
      (* I/e if we already have 2 positions and are only allowed 5, only do 3 new ones. *)
      List.take (Buy.num_positions - num_held_currently) potential_buys
    in
    let* res =
      match selected with
      | [] -> Result.return state
      | selected ->
          let current_cash = Backend_position.get_cash state.positions in
          let pct = 1.0 /. Float.of_int (List.length selected) in
          assert (pct >=. 0.0 && pct <=. 1.0);
          let place_order state (signal : Signal.t) =
            let* state = state in
            let symbol = signal.symbol in
            let reason = signal.reason in
            let* price = State.price state symbol in
            let* timestamp = State.timestamp state symbol in
            let qty = Util.qty ~current_cash ~price ~pct in
            (* assert (qty <> 0); *)
            match qty with
            | 0 -> Result.return @@ State.listen state
            | qty ->
                let order : Order.t =
                  Order.make ~symbol ~side:Buy ~tif:GoodTillCanceled
                    ~tick:state.tick ~order_type:Market ~qty ~price ~reason
                    ~timestamp ~profit:None
                in
                let state =
                  State.replace_stats state
                  @@ Stats.increment_position_ratio state.stats
                in
                let* state = Backend.place_order state order in
                let state = State.activate_order state order in
                Result.return state
          in
          List.fold_left place_order (Ok state) selected
    in
    Result.return @@ State.listen res

  let sell (state : 'a State.t) ~(buying_order : Order.t) =
    let open Result.Infix in
    match Sell.make state ~buying_order with
    | Ok (Fail _) -> Result.return @@ State.listen state
    | Error e ->
        Eio.traceln "[error] %a" Error.pp e;
        Result.return @@ State.listen state
    | Ok (Pass reason) ->
        let* price = State.price state buying_order.symbol in
        let* timestamp = State.timestamp state buying_order.symbol in
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
        let* state = Backend.place_order state order in
        let state = State.deactivate_order state buying_order in
        Result.return @@ State.listen state

  let sell_fold state buying_order =
    let ( let* ) = Result.( let* ) in
    let* state = state in
    let* res = sell state ~buying_order in
    Result.return res

  let step (state : 'a State.t) =
    let ( let* ) = Result.( let* ) in
    match state.current with
    | Ordering ->
        let held_symbols = Backend_position.symbols state.positions in
        let* sold_state =
          List.fold_left sell_fold (Ok state) state.order_history.active
        in
        let* complete = buy ~held_symbols sold_state in
        Result.return { complete with tick = complete.tick + 1 }
    | _ -> SU.handle_nonlogical_state state

  let run () = SU.run ~init_state step
end
