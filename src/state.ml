module Order = Trading_types.Order

type nonlogical_state =
  [ `Initialize | `Listening | `Liquidate | `Finished of string ]
[@@deriving show { with_path = false }]

type logical_state = [ `Ordering ] [@@deriving show { with_path = false }]

type state = [ nonlogical_state | logical_state ]
[@@deriving show { with_path = false }]

type 'a t = {
  current : state;
  bars : Bars.t;
  latest_bars : Bars.t;
  order_history : Order_history.t;
  content : 'a;
}

let record_order state time order = Hashtbl.add state.order_history time order
