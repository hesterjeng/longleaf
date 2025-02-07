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
  active_orders : Order.t list;
  tick : int;
  content : 'a;
}

let listen (x : _ t) = { x with current = `Listening }
let record_order state order = Order_history.add state.order_history order
let map (f : 'a -> 'b) (x : 'a t) = { x with content = f x.content }
let ( >|= ) x f = map f x
let ( let+ ) = ( >|= )

(* let price state symbol = *)
(*   match Bars.Latest.get_opt state.latest symbol with *)
(*   | Some x -> Result.return @@ Item.last x *)
(*   | None -> Result.fail @@ Error.MissingData {symbol; state} *)

let price (state : 'a t) symbol =
  Bars.Latest.get state.latest symbol |> Item.last
(* match Bars.Latest.get_opt state.latest symbol with *)
(* | Some x -> Option.return @@ Item.last x *)
(* | None -> *)
(*     Eio.traceln "[error] Missing information for symbol %s?" symbol; *)
(*     None *)

let timestamp (state : 'a t) symbol =
  Bars.Latest.get state.latest symbol |> Item.timestamp

(* let ( let$ ) x f = *)
(*   match f with *)
