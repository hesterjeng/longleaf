type nonlogical_state =
  [ `Initialize
  | `Listening
  | `Liquidate
  | `LiquidateContinue
  | `Continue
  | `BeginShutdown
  | `Finished of string ]
[@@deriving show { with_path = false }]

type logical_state = [ `Ordering ] [@@deriving show { with_path = false }]

type state = [ nonlogical_state | logical_state ]
[@@deriving show { with_path = false }]

type 'a t = {
  current : state;
  (* Hashtable of latest bars *)
  latest : Bars.Latest.t;
  (* Vector of orders made *)
  order_history : Order_history.t;
  (* List of statistics about portfolio value in reverse order *)
  stats : Stats.t;
  (* These are mutable hashtables tracking data *)
  indicators : Indicators.t;
  bars : Bars.t;
  (* Wildcard content for individual strategies to use *)
  content : 'a;
}

let listen (x : _ t) = { x with current = `Listening }
let record_order state order = Order_history.add state.order_history order
let map (f : 'a -> 'b) (x : 'a t) = { x with content = f x.content }
let ( >|= ) x f = map f x
let ( let+ ) = ( >|= )
let price (state : 't) symbol = Bars.Latest.get state.latest symbol |> Item.last

let timestamp (state : 'a t) symbol =
  Bars.Latest.get state.latest symbol |> Item.timestamp
