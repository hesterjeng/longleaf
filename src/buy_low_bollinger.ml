module Conditions = struct
  module Parameters = struct
    (* let min_dip = 0.98 *)
    (* let lower_now_band = 0.99 *)
    (* let upper_now_band = 1.01 *)
    let stop_loss_multiplier = 0.96
    let profit_multiplier = 1.04
    let max_holding_period = 300
    let window_size = 20
    let lookback = 120

    (* The amount of periods necessary between now and the triggering peak *)
    let min_time_gap = max_holding_period
  end

  module P = Parameters

  (* type t = Pass of Item.t | Fail of string *)

  (* let is_pass = function Pass x -> Some x | _ -> None *)
  (* let find_pass (l : t Iter.t) = Iter.find_map is_pass l *)
  (* let init l = Iter.map (fun x -> Pass x) l *)

  (* let map (f : Item.t -> t) (l : t Iter.t) = *)
  (*   Iter.map (function Pass x -> f x | Fail s -> Fail s) l *)

  (* Is the current price below the bollinger band? *)
  let below_bollinger (indicators : Indicators.t) symbol
      (current_price : Item.t) =
    let last_price = Item.last current_price in
    let lower_bollinger =
      Indicators.get indicators symbol
      |> Option.get_exn_or "low_bollinger: expected to get indicators"
      |> Vector.top
      |> Option.get_exn_or
           "low_bollinger: expected to have data present in indicator"
      |> Indicators.Point.lower_bollinger
    in
    match last_price <=. lower_bollinger with
    | true -> Some current_price
    | false -> None

  module Sell_reason = struct
    type t =
      | Profited of float
      | HoldingPeriod of float
      | StopLoss of float
      | Hold
    [@@deriving show { with_path = false }]

    let make ~time_held ~current_price ~(buying_order : Trading_types.Order.t)
        ~price_difference =
      if current_price >. P.profit_multiplier *. buying_order.price then
        Profited price_difference
      else if time_held > P.max_holding_period then
        HoldingPeriod price_difference
      else if current_price <. P.stop_loss_multiplier *. buying_order.price then
        StopLoss price_difference
      else Hold
  end
end

module BuyLowBollinger (Backend : Backend.S) : Strategy.S = struct
  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  open Trading_types

  module DT_Status = struct
    type t = Placed of (int * Order.t) | Waiting [@@deriving show]
  end

  type state = DT_Status.t State.t

  let init_state = Backend.init_state DT_Status.Waiting

  module SU = Strategy_utils.Make (Backend)
  module P = Conditions.P

  let consider_buying ~(history : Bars.t) ~(state : state)
      ~(qty : string -> int) symbol : Order.t option =
    let open Option.Infix in
    (* let* price_history = Bars.get history symbol in *)
    let most_recent_price = Bars.Latest.get state.latest symbol in
    let* (_ : Item.t) =
      Conditions.below_bollinger state.indicators symbol most_recent_price
    in
    let order =
      let side = Side.Buy in
      let tif = TimeInForce.GoodTillCanceled in
      let order_type = OrderType.Market in
      let qty = qty symbol in
      let price = Item.last most_recent_price in
      let timestamp = Item.timestamp most_recent_price in
      let reason =
        [
          Format.asprintf "Buying (%a): Below bollinger band" Time.pp timestamp;
        ]
      in
      Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp ~reason
        ~profit:None
    in
    (* Eio.traceln "@[%a@]@." Order.pp order; *)
    Some order

  (* The maximum amount of a share that can be purchased at the current price with *)
  (*  pct of cash available *)
  let qty (state : state) pct symbol =
    let cash_available = Backend.get_cash () in
    match cash_available >=. 0.0 with
    | true ->
        let tenp = cash_available *. pct in
        let current_price = Item.last @@ Bars.Latest.get state.latest symbol in
        let max_amt = tenp /. current_price in
        if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
    | false -> 0

  (* Check if we meet the conditions for placing a short for one of our symbols. *)
  (* If not, return the state unchanged, except we are now listening. *)
  let place_buy ~(state : state) =
    let ( let* ) = Result.( let* ) in
    let short_opt =
      consider_buying ~history:state.bars ~state ~qty:(qty state 0.9)
    in
    let possibilities = List.map short_opt Backend.symbols in
    let choice = Option.choice possibilities in
    let* new_status =
      match choice with
      | None -> Ok state.content
      | Some order ->
          let* () = Backend.place_order state order in
          Result.return @@ DT_Status.Placed (0, order)
    in
    Result.return
    @@ { state with State.current = `Listening; content = new_status }

  let exit_position ~(state : state) time_held (buying_order : Order.t) =
    let ( let* ) = Result.( let* ) in
    let current_bar = Bars.Latest.get state.latest buying_order.symbol in
    let current_price = Item.last current_bar in
    let timestamp = Item.timestamp current_bar in
    let price_difference = buying_order.price -. current_price in
    let cover_reason =
      Conditions.Sell_reason.make ~time_held ~current_price ~buying_order
        ~price_difference
    in
    match cover_reason with
    | Profited _ | HoldingPeriod _ | StopLoss _ ->
        let profit =
          Float.of_int buying_order.qty *. (current_price -. buying_order.price)
        in
        let reason =
          Format.asprintf "Exiting because of %a. Profit: %f"
            Conditions.Sell_reason.pp cover_reason profit
          :: buying_order.reason
        in
        (* Eio.traceln "@[Profit from covering: %f@]@." profit; *)
        let* () =
          Backend.place_order state
          @@ Order.make ~symbol:buying_order.symbol ~side:Side.Sell
               ~tif:buying_order.tif ~order_type:buying_order.order_type
               ~qty:buying_order.qty ~price:current_price ~timestamp ~reason
               ~profit:(Some profit)
        in
        Result.return
        @@ {
             state with
             State.current = `Listening;
             content = DT_Status.Waiting;
           }
    | Hold ->
        (* Eio.traceln "@[Holding...@]@."; *)
        Result.return
        @@ {
             state with
             State.current = `Listening;
             content = DT_Status.Placed (time_held + 1, buying_order);
           }

  let step (state : state) =
    let current = state.current in
    (* Eio.traceln "@[%a@]@." State.pp_state current; *)
    match current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        (* Eio.traceln "@[%a@]@." DT_Status.pp state.content; *)
        match state.content with
        | Waiting -> place_buy ~state
        | Placed (time_held, order) -> exit_position ~state time_held order)

  let run () = SU.run ~init_state step
end
