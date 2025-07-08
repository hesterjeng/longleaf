open Trading_types

type order = {
  symbol : Instrument.t;
  side : Side.t;
  tif : TimeInForce.t;
  order_type : OrderType.t;
  qty : int;
  tick : int;
  price : float;
  (* instrument : Instrument.t; *)
  timestamp : Time.t;
  reason : string list;
  (* This is the expected profit of this trade, if it is closing a known position *)
  profit : float option;
  id : string Pmutex.t;
  status : Status.t Pmutex.t;
}
[@@deriving show, yojson]

type t = order [@@deriving show, yojson]

let timestamp x = x.timestamp

let equal x y =
  Instrument.equal x.symbol y.symbol
  && x.qty = y.qty
  && Float.equal x.price y.price
  && Side.equal x.side y.side
  && Ptime.equal x.timestamp y.timestamp

let make ~symbol ~tick ~side ~tif ~order_type ~qty ~price ~timestamp ~reason
    ~profit =
  {
    symbol;
    side;
    tif;
    tick;
    order_type;
    qty;
    price;
    timestamp;
    reason;
    profit;
    id = Pmutex.make "id_not_set";
    status = Pmutex.make Status.New;
  }

let cmp_profit x y =
  match (x.profit, y.profit) with
  | Some x, Some y -> Float.compare x y
  | _ -> 0

let cmp_timestamp x y = Ptime.compare x.timestamp y.timestamp

module History = struct
  module Ptime_map = Map.Make (Ptime)

  type nonrec t = { all : order Ptime_map.t; active : order Ptime_map.t }

  let yojson_of_t (h : t) : Yojson.Safe.t =
    let l = Ptime_map.bindings h.all |> List.map snd in
    `List (List.map yojson_of_order l)

  let add (h : t) (order_item : order) =
    let key = timestamp order_item in
    { all = Ptime_map.add key order_item h.all; active = h.active }

  let add_active (h : t) (order_item : order) =
    let key = timestamp order_item in
    { h with active = Ptime_map.add key order_item h.active }

  let remove_active (h : t) (order_item : order) =
    let key = timestamp order_item in
    { h with active = Ptime_map.remove key h.active }

  let empty = { all = Ptime_map.empty; active = Ptime_map.empty }
  let length h = Ptime_map.cardinal h.all
  let inactive h = Ptime_map.bindings h.all |> List.map snd
  let active h = Ptime_map.bindings h.active |> List.map snd
end
