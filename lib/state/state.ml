module Mode = Mode
module Config = Config
module Stats = Stats

type 'a t = {
  current_state : Mode.t;
  bars : Bars.t;
  current_tick : int;
  content : 'a;
  config : Config.t;
  cash : float;
  history : Order.t list Vector.vector;
  positions : Positions.t;
}
[@@warning "-69"]

let empty () : unit t =
  {
    current_state = Initialize;
    bars = Bars.empty ();
    current_tick = 0;
    config = { placeholder = false };
    cash = 0.0;
    history = Vector.create ();
    positions = Positions.empty;
    content = ();
  }

let set x mode = { x with current_state = mode }
let increment_tick x = { x with current_tick = x.current_tick + 1 }
let bars x = x.bars
let cost_basis x = Positions.cost_basis x.positions

type 'a res = ('a, Error.t) result

let current t = t.current_state

let make tick bars content =
  let ( let* ) = Result.( let* ) in
  let* length = Bars.length bars in
  let config = Config.{ placeholder = true } in
  Result.return
    {
      current_state = Initialize;
      bars;
      current_tick = tick;
      content;
      config;
      cash = 100000.0;
      history = Vector.init length (fun _ -> []);
      positions = Positions.empty;
    }

let pp fmt t =
  Format.fprintf fmt "V3State(tick=%d, state=%a, cash=%.2f)" t.current_tick
    Mode.pp t.current_state t.cash

let show t = Format.asprintf "%a" pp t
let cash t = t.cash
let time t = Bars.timestamp t.bars
let history t = Vector.freeze t.history
let positions x = x.positions

(* let symbols x = List.map (fun (x : Order.t) -> x.symbol) @@ positions x *)
let held_symbols x = Positions.symbols @@ positions x
let data t instrument = Bars.get t.bars instrument

let value t =
  let ( let* ) = Result.( let* ) in
  let* portfolio_value =
    Positions.fold (positions t) (Ok 0.0) @@ fun instrument orders acc ->
    let* _ = acc in
    let* data = data t instrument in
    let current_price = Bars.Data.get data Last t.current_tick in
    Result.return
    @@ List.fold_left
         (fun acc (order : Order.t) ->
           let order_value = float_of_int order.qty *. current_price in
           order_value +. acc)
         0.0 orders
  in
  Result.return (t.cash +. portfolio_value)

let listen t = { t with current_state = Listening }
let liquidate t = { t with current_state = Liquidate }
let qty (t : 'a t) instrument = Positions.qty t.positions instrument

let place_order t (order : Order.t) =
  let ( let* ) = Result.( let* ) in
  let* data = Bars.get t.bars order.symbol in
  let current_price = Bars.Data.get data Last t.current_tick in
  let order_value = float_of_int order.qty *. current_price in
  let tick = t.current_tick in

  let _ = Pmutex.set order.status Order.Status.Filled in
  let positions = t.positions in
  let positions = Positions.update positions order in
  match order.side with
  | Buy ->
    if t.cash >=. order_value then (
      let new_cash = t.cash -. order_value in
      let current_tick_orders = order :: Vector.get t.history tick in
      Vector.set t.history tick current_tick_orders;
      Result.return { t with cash = new_cash; positions })
    else Error.fatal "Insufficient cash for buy order"
  | Sell ->
    let qty_held = qty t order.symbol in
    if qty_held >= order.qty then (
      let new_cash = t.cash +. order_value in
      let current_tick_orders = order :: Vector.get t.history tick in
      Vector.set t.history tick current_tick_orders;
      Result.return { t with cash = new_cash; positions })
    else Error.fatal "Insufficient shares for sell order"

let tick t = t.current_tick
let options t = t.config

let stats x =
  let bars = bars x in
  Stats.make x.history bars
