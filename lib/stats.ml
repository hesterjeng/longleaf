module PositionRatio = struct
  type t = { positions_taken : int; positions_possible : int }
  [@@deriving yojson, show]

  let none = { positions_taken = 0; positions_possible = 0 }

  let add_possible_positions (x : t) candidates =
    {
      x with
      positions_possible = x.positions_possible + List.length candidates;
    }

  let increment (x : t) = { x with positions_taken = x.positions_taken + 1 }
end

type item = {
  time : Time.t;
  value : float;
  cash : float;
  risk_free_value : float;
  orders : Order.t list;
}
[@@deriving yojson, show]

type t = { history : item list; position_ratio : PositionRatio.t }
[@@deriving yojson, show]

let empty () : t = { position_ratio = PositionRatio.none; history = [] }

let cons (x : t) (i : item) =
  { history = i :: x.history; position_ratio = x.position_ratio }

let compare x y = Ptime.compare x.time y.time
let sort (x : t) = { x with history = List.sort compare x.history }

let add_possible_positions (x : t) candidates =
  {
    x with
    position_ratio =
      PositionRatio.add_possible_positions x.position_ratio candidates;
  }

let increment_position_ratio (x : t) =
  { x with position_ratio = PositionRatio.increment x.position_ratio }

(* FIXME: This seems strange, why are we converting to an array and then unconverting? *)
(* BUG: very high computational complexity *)
let add_orders (h : Order.History.t) (x : t) : t =
  let orders = Order.History.inactive h in
  let stats_array = Array.of_list x.history in
  let stats_times = Array.map (fun x -> x.time) stats_array |> Array.to_list in
  let () =
    List.iter
      (fun order ->
        let closest_stat_time =
          ( Time.find_closest (Order.timestamp order) stats_times
          |> Option.map Time.of_float_res
          |> function
            | Some x -> x
            | None -> Error.fatal "No time found in Stats.add_order" )
          |> function
          | Ok x -> x
          | Error _ -> invalid_arg "No time found in Stats.add_order"
        in
        Array.map_inplace
          (fun (item : item) ->
            if Ptime.equal item.time closest_stat_time then
              { item with orders = order :: item.orders }
            else item)
          stats_array)
      orders
  in
  let history = Array.to_list stats_array in
  { x with history }

let risk_free_value (stats : t) tick =
  let interest_per_tick = 0.017 *. (tick /. 23400.0) in
  (* Eio.traceln "stats.ml: ipt %f" interest_per_tick; *)
  let prev_risk_free =
    stats.history |> List.head_opt |> function
    | Some x -> x.risk_free_value
    | None -> 100000.0
  in
  let res = prev_risk_free *. (1.0 +. (interest_per_tick /. 100.0)) in
  (* Eio.traceln "stats.ml: risk free value %f" res; *)
  res
