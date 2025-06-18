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
  (* indicators : Indicators.t; *)
  (* Historical data *)
  bars : Bars.t;
  time : Time.t;
  (* The current tick the state machine is on *)
  tick : int;
  tick_length : float;
  (* Wildcard content for individual strategies to use *)
  content : 'a;
}

let listen (x : _ t) = { x with current = Listening }

(* let indicators (state : _ t) symbol = *)
(*   (\* Eio.traceln "state: %a" Time.pp state.time; *\) *)
(*   Indicators.get_top state.indicators ~time:state.time symbol *)

(* let indicators_back_n (state : _ t) symbol n = *)
(*   let ( let* ) = Option.( let* ) in *)
(*   let* bars = Bars.get state.bars symbol in *)
(*   let length = Bars.length state.bars in *)
(*   let* time = *)
(*     try *)
(*       Vector.get bars (length - (n + 1)) |> Item.timestamp |> Option.return *)
(*     with *)
(*     | _ -> None *)
(*   in *)
(*   let* res = *)
(*     Option.of_result @@ Indicators.get_top state.indicators ~time symbol *)
(*   in *)
(*   Option.return res *)

let record_order state order =
  let ( let* ) = Result.( let* ) in
  (* let* () = Bars.add_order order state.bars in *)
  let new_h = Order.History.add state.order_history order in
  Result.return @@ { state with order_history = new_h }

let activate_order state order =
  let new_h =
    { state.order_history with active = order :: state.order_history.active }
  in
  { state with order_history = new_h }

let place_order (state : 'a t) (order : Order.t) =
  let ( let* ) = Result.( let* ) in
  (* Eio.traceln "Placing order: @[%a@]@." Order.pp order; *)
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

(* let price (state : 'a t) symbol = *)
(*   Result.map Data.Column.last_exn @@ Bars.Latest.get state.latest symbol *)

let price (state : 'a t) symbol =
  let ( let* ) = Result.( let* ) in
  let* col = Bars.Latest.get state.latest symbol in
  let price = Data.Column.last_exn col in
  Result.return price

let timestamp (state : 'a t) symbol =
  let ( let* ) = Result.( let* ) in
  let* col = Bars.Latest.get state.latest symbol in
  let* time = Data.Column.timestamp col in
  Result.return time

(* let volume (state : 'a t) symbol = *)
(*   Result.map Item.volume @@ Bars.Latest.get state.latest symbol *)

(* let timestamp (state : 'a t) symbol = *)
(*   Result.map Item.timestamp @@ Bars.Latest.get state.latest symbol *)
