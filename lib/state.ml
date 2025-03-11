type state =
  | Initialize
  | Listening
  | Ordering
  | Liquidate
  | LiquidateContinue
  | Continue
  | BeginShutdown
  | Finished of string
[@@deriving show { with_path = false }]

type 'a t = {
  current : state;
  (* Hashtable of latest bars *)
  latest : Bars.Latest.t;
  (* Vector of orders made *)
  order_history : Order.History.t;
  (* List of statistics about portfolio value in reverse order *)
  stats : Stats.t;
  positions : Backend_position.t;
  (* These are mutable hashtables tracking data *)
  indicators : Indicators.t;
  (* Historical data *)
  bars : Bars.t;
  (* The current tick the state machine is on *)
  tick : int;
  tick_length : float;
  (* Wildcard content for individual strategies to use *)
  content : 'a;
}

let listen (x : _ t) = { x with current = Listening }

let record_order state order =
  let ( let* ) = Result.( let* ) in
  let* () = Bars.add_order order state.bars in
  let new_h = Order.History.add state.order_history order in
  Result.return @@ { state with order_history = new_h }

let activate_order state order =
  let new_h =
    { state.order_history with active = order :: state.order_history.active }
  in
  { state with order_history = new_h }

let place_order (state : 'a t) (order : Order.t) =
  let ( let* ) = Result.( let* ) in
  Eio.traceln "@[%a@]@." Order.pp order;
  let* state = record_order state order in
  let* new_positions = Backend_position.execute_order state.positions order in
  Result.return { state with positions = new_positions }

let deactivate_order state order =
  let new_h =
    {
      state.order_history with
      active =
        List.filter
          (fun x -> not @@ Order.equal x order)
          state.order_history.active;
    }
  in
  { state with order_history = new_h }

let replace_stats x stats = { x with stats }
let map (f : 'a -> 'b) (x : 'a t) = { x with content = f x.content }
let ( >|= ) x f = map f x
let ( let+ ) = ( >|= )

let price (state : 'a t) symbol =
  Bars.Latest.get state.latest symbol |> Item.last

let volume (state : 'a t) symbol =
  Bars.Latest.get state.latest symbol |> Item.volume

let timestamp (state : 'a t) symbol =
  Bars.Latest.get state.latest symbol |> Item.timestamp
