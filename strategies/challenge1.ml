open Trading_types

module Status = struct
  type t = Placed of (int * Order.t) | Waiting [@@deriving show]
end

type state = Status.t State.t

module BuyReason = struct
  type t = Above3StdBollinger

  let make (state : state) symbol =
    let open Option.Infix in
    let current_price = Bars.Latest.get state.latest symbol |> Item.last in
    let* upper_bb =
      Indicators.indicator state.indicators symbol
        Indicators.Point.upper_bollinger_100_3
    in
    let* sma75 =
      Indicators.indicator state.indicators symbol Indicators.Point.sma_75
    in
    let pass = current_price >=. upper_bb && not (current_price <=. sma75) in
    match pass with true -> Some Above3StdBollinger | false -> None
end

module Sell_reason = struct
  type t = Below1StdBollinger [@@deriving show { with_path = false }]

  let make (state : state) symbol =
    let open Option.Infix in
    let current_price = Bars.Latest.get state.latest symbol |> Item.last in
    let* lower_bb =
      Indicators.indicator state.indicators symbol
        Indicators.Point.lower_bollinger_100_1
    in
    let* sma75 =
      Indicators.indicator state.indicators symbol Indicators.Point.sma_75
    in
    let pass = current_price <=. lower_bb && not (current_price <=. sma75) in
    match pass with true -> Some Below1StdBollinger | false -> None
end

module Order = struct

  include Trading_types.Order

  let of_make_reason (x : )

end

module Make (Backend : Backend.S) : Strategy.S = struct
  module SU = Strategy_utils.Make (Backend)

  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  let init_state = Backend.init_state Status.Waiting

  let buy (state : state) =
    let passes = List.filter_map (BuyReason.make state) Backend.symbols in
    (* Just select the first one for now *)
    let selected = List.head_opt passes in
    ()

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
