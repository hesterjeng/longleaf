type item = {
  time : Time.t;
  value : float;
  risk_free_value : float;
  orders : Order.t list;
  order_history : (Order_history.t[@opaque]);
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

let sharpe_ratio (stats : t) =
  let final : item =
    List.head_opt stats
    |> Option.get_exn_or
         "stats.ml: Expected to get final element of stats in backtest"
  in
  Eio.traceln "%a" pp_item final;
  let values =
    List.map (fun x -> x.value -. x.risk_free_value) stats |> Array.of_list
  in
  let std = Owl_stats.std values in
  let sharpe = (final.value -. final.risk_free_value) /. std in
  Eio.traceln "@[SR: %f@]@." sharpe;
  sharpe
