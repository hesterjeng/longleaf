module Buy_trigger = struct
  module type S = sig
    val make : 'a State.t -> string list -> Signal.t option
  end

  module type INPUT = sig
    val pass : 'a State.t -> string -> Signal.Flag.t
    val score : 'a State.t -> string -> float
  end

  module Make (Input : INPUT) = struct
    let make state symbols : Signal.t option =
      let passes =
        List.filter_map
          (fun s ->
            match Input.pass state s with
            | Pass reason -> Some (s, reason, Input.score state s)
            | Fail _ -> None)
          symbols
      in
      let selected =
        List.fold_left
          (fun acc (symbol, reason, score) ->
            match acc with
            | None -> Some (symbol, reason, score)
            | Some (prev_sym, prev_res, prev_score) ->
                if prev_score >=. score then
                  Some (prev_sym, prev_res, prev_score)
                else Some (symbol, reason, score))
          None passes
      in
      match selected with
      | Some (symbol, reason, _) -> Some { symbol; reason }
      | None -> None
  end
end

module Sell_trigger = struct
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
    let selected = Buy.make state Backend.symbols in
    let+ content =
      match selected with
      | None -> Ok state.content
      | Some { symbol; reason } ->
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
    match Sell.make state buying_order.symbol with
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
    match current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        let positions = state.content in
        let length = List.length positions in
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
