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
  latest : Bars.latest;
  order_history : Order_history.t;
  content : 'a;
}

let record_order state order = Order_history.add state.order_history order
let map (f : 'a -> 'b) (x : 'a t) = { x with content = f x.content }
let ( >|= ) x f = map f x
let ( let+ ) = ( >|= )
