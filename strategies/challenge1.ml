open Trading_types

module DT_Status = struct
  type t = Placed of (int * Order.t) | Waiting [@@deriving show]
end

type state = DT_Status.t State.t

module Conditions = struct
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
    | true -> Some (lower_bollinger -. last_price)
    | false -> None

  module Sell_reason = struct
    type t =
      | Profited of float
      | HoldingPeriod of float
      | StopLoss of float
      | FSO_High of float
      | HoldBelowBollinger
      | Hold
    [@@deriving show { with_path = false }]
  end
end

module Make (Backend : Backend.S) : Strategy.S = struct
  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let init_state = Backend.init_state DT_Status.Waiting

  let step (state : state) =
    let current = state.current in
    (* Eio.traceln "@[buylowbollinger: %a@]@." State.pp_state current; *)
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
