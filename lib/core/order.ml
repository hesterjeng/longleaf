open Trading_types
module Pmutex = Longleaf_util.Pmutex

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

(* Status submodule for convenience - re-exports Trading_types.Status *)
module Status = Trading_types.Status
