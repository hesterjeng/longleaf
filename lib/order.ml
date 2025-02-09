open Trading_types

type t = {
  symbol : string;
  side : Side.t;
  tif : TimeInForce.t;
  order_type : OrderType.t;
  qty : int;
  tick : int;
  price : float;
  timestamp : Time.t;
  reason : string list;
  (* This is the expected profit of this trade, if it is closing a known position *)
  profit : float option;
  id : string Pmutex.t;
  status : Status.t Pmutex.t;
}
[@@deriving show, yojson]

let timestamp x = x.timestamp

let equal x y =
  String.equal x.symbol y.symbol
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
  match (x.profit, y.profit) with Some x, Some y -> Float.compare x y | _ -> 0

let cmp_timestamp x y = Ptime.compare x.timestamp y.timestamp

module History = struct
  type nonrec t = { all : t list; active : t list }

  let sort h = List.sort cmp_timestamp h
  let inactive h = h.all
  let active h = h.active

  let yojson_of_t (h : t) : Yojson.Safe.t =
    let l = h.all in
    `List (List.map yojson_of_t l)

  let add x order = { x with active = order :: x.active }
  let empty = { all = []; active = [] }
  let length h = List.length h.all
end
