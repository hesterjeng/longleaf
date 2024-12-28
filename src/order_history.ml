module Order = Trading_types.Order

(* type order_history = (Time.t * Order.t) list [@@deriving show, yojson] *)
(* type t = (Time.t, Order.t) Hashtbl.t *)

(* let yojson_of_t (x : t) = *)
(*   let l = Hashtbl.to_list x in *)
(*   yojson_of_order_history l *)

(* let add (order_history : t) time order = Hashtbl.replace order_history time order *)

type order_history = Order.t list [@@deriving show, yojson]
type t = Order.t Vector.vector

let yojson_of_t (x : t) =
  let l = Vector.to_list x in
  yojson_of_order_history l

let add (order_history : t) order = Vector.push order_history order

let find (time : Time.t) (x : t) =
  Vector.find (fun order -> Ptime.equal (Order.timestamp order) time) x
