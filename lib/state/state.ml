module Mode = Mode
module Config = Config
module Stats = Stats
module Bars = Longleaf_bars
module Pmutex = Longleaf_util.Pmutex

type t = {
  (* current_state : Mode.t; *)
  bars : Bars.t;
  current_tick : int;
  orders_placed : int;
  config : Config.t;
  cash : float;
  positions : Positions.t;
  value_history : (Time.t * float) list;
}
[@@deriving fields] [@@warning "-69"]

let empty runtype (indicators : Tacaml.t list) : t =
  {
    (* current_state = `Initialize; *)
    bars = Bars.empty ();
    current_tick = 0;
    orders_placed = 0;
    config =
      {
        placeholder = false;
        indicator_config = Indicators_config.make runtype indicators;
      };
    cash = 0.0;
    positions = Positions.empty;
    value_history = [];
  }

let set_tick x current_tick = { x with current_tick }
let bars x = x.bars
let value_history x = x.value_history
let cost_basis x = Positions.cost_basis x.positions

type 'a res = ('a, Error.t) result

let config x = x.config

let make current_tick bars indicator_config cash =
  let config = Config.{ placeholder = true; indicator_config } in
  Result.return
    {
      bars;
      current_tick;
      orders_placed = 0;
      config;
      cash;
      positions = Positions.empty;
      value_history = [];
    }

let pp : t Format.printer =
 fun fmt t ->
  Format.fprintf fmt "V3State(tick=%d, cash=%.2f)" t.current_tick t.cash

let show t = Format.asprintf "%a" pp t
let cash t = t.cash
let time t = Bars.timestamp t.bars t.current_tick
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

let increment_tick x =
  let ( let* ) = Result.( let* ) in
  let* value = value x in
  let* time = time x in
  Result.return
  @@ {
       x with
       current_tick = x.current_tick + 1;
       value_history = (time, value) :: x.value_history;
     }

let qty (t : t) instrument = Positions.qty t.positions instrument

let place_order t (order : Order.t) =
  let ( let* ) = Result.( let* ) in
  let* data = Bars.get t.bars order.symbol in
  let current_price = Bars.Data.get data Last t.current_tick in
  let order_value = float_of_int order.qty *. current_price in
  let tick = t.current_tick in

  let _ = Pmutex.set order.status Order.Status.Filled in
  let positions = t.positions in
  let positions = Positions.update positions order in
  (* Add order to data structure for visualization *)
  let* () = Bars.Data.add_order data tick order in
  match order.side with
  | Buy ->
    if t.cash >=. order_value then
      let new_cash = t.cash -. order_value in
      Result.return
        {
          t with
          cash = new_cash;
          positions;
          orders_placed = t.orders_placed + 1;
        }
    else Error.fatal "Insufficient cash for buy order"
  | Sell ->
    let qty_held = qty t order.symbol in
    if qty_held >= order.qty then
      let new_cash = t.cash +. order_value in
      Result.return
        {
          t with
          cash = new_cash;
          positions;
          orders_placed = t.orders_placed + 1;
        }
    else Error.fatal "Insufficient shares for sell order"

let tick t = t.current_tick
let options t = t.config

let stats x =
  let bars = bars x in
  let positions = positions x in
  Stats.from_positions positions bars

let grow x =
  let ( let* ) = Result.( let* ) in
  let* bars = Bars.grow x.bars in
  Result.return @@ { x with bars }

let orders_placed x = x.orders_placed

module Conv = struct
  let to_tearsheet_json x =
    let vh = List.rev (value_history x) in (* Reverse to get chronological order *)
    match vh with
    | [] -> `Assoc [("returns", `List []); ("dates", `List [])]
    | _ ->
      let values = List.map snd vh in
      let dates = List.map (fun (t, _) -> Time.to_string t) vh in
      
      (* Calculate returns from consecutive portfolio values *)
      let rec calc_returns acc = function
        | [] | [_] -> List.rev acc (* Need at least 2 values *)
        | prev :: (curr :: _ as rest) ->
          let return_pct = (curr -. prev) /. prev in
          calc_returns (return_pct :: acc) rest
      in
      let returns = calc_returns [] values in
      
      (* Format for JSON *)
      let returns_json = List.map (fun r -> `Float r) returns in
      let dates_json = List.map (fun d -> `String d) (List.tl dates) in (* Skip first date since we have n-1 returns *)
      
      `Assoc [
        ("returns", `List returns_json);
        ("dates", `List dates_json)
      ]
end
