module Data = Bars.Data
module Order_record = Order_record
module IdMap = Core.IdMap
module SymbolMap = Core.SymbolMap
module Core = Core
(* module Core = Core *)

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
  (* Trading state containing orders, positions, portfolio, and stats *)
  trading_state : Core.t;
  (* Historical data *)
  bars : Bars.t;
  time : Time.t;
  (* The current tick the state machine is on *)
  tick : int;
  tick_length : float;
  (* Wildcard content for individual strategies to use *)
  content : 'a;
}

let empty () : unit t =
  {
    current = Initialize;
    trading_state = Core.empty ();
    bars = Bars.empty ();
    time = Time.get_todays_date ();
    tick = 0;
    tick_length = 0.0;
    content = ();
  }

let pp_simple : 'a t Format.printer =
 fun fmt x ->
  let current = Bars.get_current x.bars |> Result.get_exn in
  Format.fprintf fmt "%a %d %d" Time.pp x.time x.tick current

let listen (x : _ t) = { x with current = Listening }

(* Generate order ID using UUID for uniqueness *)
let generate_order_id () = Order_id.generate ()

let record_order state order =
  let order_id = generate_order_id () in
  let new_trading_state = Core.add_order state.trading_state order_id order in
  Result.return ({ state with trading_state = new_trading_state }, order_id)

let price (state : 'a t) symbol =
  let ( let* ) = Result.( let* ) in
  (* let* col = *)
  let* data = Bars.get state.bars symbol in
  let price = Data.get data Last state.tick in
  Result.return price

let activate_order state order_id =
  let new_trading_state = Core.activate_order state.trading_state order_id in
  { state with trading_state = new_trading_state }

let place_order (state : 'a t) (order : Order.t) =
  let ( let* ) = Result.( let* ) in
  (* Eio.traceln "Placing order: @[%a@]@." Order.pp order; *)
  let* new_state, order_id = record_order state order in
  let* current_price = price new_state order.symbol in
  let* new_trading_state =
    Core.execute_order_against_position new_state.trading_state order_id
      order.qty current_price
  in
  Result.return { new_state with trading_state = new_trading_state }

(* Stats are now integrated into trading_state *)
let replace_stats x _stats = x

(* Helper functions to access trading state data *)
let get_cash state = Core.get_cash state.trading_state
let get_positions state = state.trading_state.positions

let get_position state symbol =
  Core.SymbolMap.find_opt symbol state.trading_state.positions

let get_qty state symbol = Core.qty state.trading_state symbol
let get_symbols state = Core.symbols state.trading_state
let is_portfolio_empty state = Core.is_empty state.trading_state
let portfolio_value state = Core.value state.trading_state state.bars
let map (f : 'a -> 'b) (x : 'a t) = { x with content = f x.content }
let ( >|= ) x f = map f x
let ( let+ ) = ( >|= )

(* let price (state : 'a t) symbol = *)
(*   Result.map Data.Column.last_exn @@ Bars.Latest.get state.latest symbol *)

let timestamp (state : 'a t) symbol =
  let ( let* ) = Result.( let* ) in
  let* data = Bars.get state.bars symbol in
  let time = Data.get data Time state.tick in
  match Ptime.of_float_s time with
  | Some t -> Result.return t
  | None -> Error.fatal "Illegal timestamp (State.timestamp)"

let get_active_orders x = Core.get_active_orders x.trading_state
let get_pending_orders x = Core.get_pending_orders x.trading_state
