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
  let stats_array = Array.of_list x in
  let stats_times = Array.map (fun x -> x.time) stats_array |> Array.to_list in
  let () =
    Vector.iter
      (fun order ->
        let closest_stat_time =
          Time.find_closest (Order.timestamp order) stats_times
        in
        Array.map_inplace
          (fun (item : item) ->
            if Ptime.equal item.time closest_stat_time then
              match order.side with
              | Buy -> { item with buy_order = Some order }
              | Sell -> { item with sell_order = Some order }
            else item)
          stats_array)
      orders
  in
  Array.to_list stats_array
