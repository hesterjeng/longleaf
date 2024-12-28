module Order = Trading_types.Order

type item = {
  time : Time.t;
  value : float;
  buy_order : Order.t option;
  sell_order : Order.t option;
}
[@@deriving yojson, show]

type t = item list [@@deriving yojson, show]

let empty = []
let append (x : item) (l : t) = x :: l
let compare x y = Ptime.compare x.time y.time
let sort (x : t) = List.sort compare x

let add_orders (orders : Order_history.t) (x : t) =
  List.map
    (fun item ->
      let time = item.time in
      let order = Order_history.find time orders in
      match order with
      | Some order -> (
          match order.side with
          | Buy -> { item with buy_order = Some order }
          | Sell -> { item with sell_order = Some order })
      | None -> item)
    x
