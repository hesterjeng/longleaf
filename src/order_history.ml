module Order = Trading_types.Order

type order_history = (Time.t * Order.t) list [@@deriving show, yojson]
type t = (Time.t, Order.t) Hashtbl.t

let yojson_of_t (x : t) =
  let l = Hashtbl.to_list x in
  yojson_of_order_history l
