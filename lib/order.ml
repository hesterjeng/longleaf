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

(* let default_buy ?profit ~current_cash ~price ~reason ~timestamp symbol = *)
(*   let ( let+ ) = Option.( let+ ) in *)
(*   let+ qty = *)
(*     Util.qty ~current_cash ~pct:1.0 ~price |> function *)
(*     | 0 -> None *)
(*     | qty -> Some qty *)
(*   in *)
(*   make ~price ~qty ~reason ~timestamp ~symbol ~side:Buy ~tif:GoodTillCanceled *)
(*     ~order_type:Market ~profit *)

(* let default_sell ?profit ~current_cash ~price ~reason ~timestamp symbol = *)
(*   let ( let+ ) = Option.( let+ ) in *)
(*   let+ qty = *)
(*     Util.qty ~current_cash ~pct:1.0 ~price |> function *)
(*     | 0 -> None *)
(*     | qty -> Some qty *)
(*   in *)
(*   make ~price ~qty ~reason ~timestamp ~symbol ~side:Sell ~tif:GoodTillCanceled *)
(*     ~order_type:Market ~profit *)

let cmp_profit x y =
  match (x.profit, y.profit) with Some x, Some y -> Float.compare x y | _ -> 0

let cmp_timestamp x y = Ptime.compare x.timestamp y.timestamp
