type item = {
  time : Time.t;
  value : float;
  risk_free_value : float;
  orders : Order.t list;
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
              { item with orders = order :: item.orders }
            else item)
          stats_array)
      orders
  in
  Array.to_list stats_array

let risk_free_value stats tick =
  let interest_per_tick = 0.017 *. (tick /. 23400.0) in
  (* Eio.traceln "stats.ml: ipt %f" interest_per_tick; *)
  let prev_risk_free =
    stats |> List.head_opt |> function
    | Some x -> x.risk_free_value
    | None -> 100000.0
  in
  let res = prev_risk_free *. (1.0 +. (interest_per_tick /. 100.0)) in
  (* Eio.traceln "stats.ml: risk free value %f" res; *)
  res
