module Order = Trading_types.Order

type item = { time : Time.t; value : float; order : Order.t option }
[@@deriving yojson]

type t = item list [@@deriving yojson]

let empty = []
let append (x : item) (l : t) = x :: l
let compare x y = Ptime.compare x.time y.time
let sort (x : t) = List.sort compare x

let add_orders (orders : Order_history.t) (x : t) =
  List.map
    (fun item ->
      let time = item.time in
      { item with order = Order_history.find time orders })
    x
