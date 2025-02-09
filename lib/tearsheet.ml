type t = {
  num_orders : int;
  sharpe_ratio : float;
  win_percentage : float;
  average_trade_net : float;
  average_profit : float;
  average_loss : float;
  stddev_returns : float;
  profit_factor : float;
  biggest_winner : Order.t option;
  biggest_loser : Order.t option;
}
[@@deriving show]

(* let num_orders (x : Order.History.t) = Vector.length x *)

let win_percentage (h : Order.History.t) =
  let orders = Order.History.inactive h in
  let fold (winners, losers) (x : Order.t) =
    match x.profit with
    | None -> (winners, losers)
    | Some f when f >=. 0.0 -> (winners +. 1.0, losers)
    | Some f when f <. 0.0 -> (winners, losers +. 1.0)
    | Some _ -> (winners, losers)
  in
  let winners, losers = List.fold_left fold (0.0, 0.0) orders in
  let win_percentage = winners /. (winners +. losers) in
  win_percentage

let sharpe_ratio (stats : Stats.t) =
  let final : Stats.item =
    List.head_opt stats
    |> Option.get_exn_or
         "stats.ml: Expected to get final element of stats in backtest"
  in
  Eio.traceln "%a" Stats.pp_item final;
  let values =
    List.map (fun (x : Stats.item) -> x.value -. x.risk_free_value) stats
    |> Array.of_list
  in
  let std = Owl_stats.std values in
  let sharpe = (final.value -. final.risk_free_value) /. std in
  sharpe

let average_trade_net (h : Order.History.t) =
  let ( let+ ) = Option.( let+ ) in
  let nets =
    List.filter_map
      (fun (x : Order.t) ->
        let+ profit = x.profit in
        profit)
      h.all
    |> Array.of_list
  in
  Owl_stats.mean nets

let average_profit (h : Order.History.t) =
  let ( let* ) = Option.( let* ) in
  let nets =
    List.filter_map
      (fun (x : Order.t) ->
        let* profit = x.profit in
        match profit >=. 0.0 with true -> Some profit | false -> None)
      h.all
    |> Array.of_list
  in
  Owl_stats.mean nets

let average_loss (h : Order.History.t) =
  let ( let* ) = Option.( let* ) in
  let nets =
    List.filter_map
      (fun (x : Order.t) ->
        let* profit = x.profit in
        match profit <=. 0.0 with true -> Some profit | false -> None)
      h.all
    |> Array.of_list
  in
  Owl_stats.mean nets

let stddev_returns (stats : Stats.t) =
  let returns =
    List.map (fun (x : Stats.item) -> x.value) stats |> Array.of_list
  in
  Owl_stats.std returns

let profit_factor (h : Order.History.t) =
  let profits =
    List.fold_left
      (fun acc (x : Order.t) ->
        match x.profit with
        | None -> acc
        | Some f when f >=. 0.0 -> acc +. f
        | _ -> acc)
      0.0 h.all
  in
  let losses =
    List.fold_left
      (fun acc (x : Order.t) ->
        match x.profit with
        | None -> acc
        | Some f when f <=. 0.0 -> acc +. f
        | _ -> acc)
      0.0 h.all
  in
  profits /. (-1.0 *. losses)

let biggest (h : Order.History.t) =
  let sorted =
    h.all
    |> List.filter_map (fun (o : Order.t) ->
           match o.profit with Some _ -> Some o | None -> None)
    |> List.sort Order.cmp_profit
  in
  let biggest_loser = List.head_opt sorted in
  let biggest_winner = List.last_opt sorted in
  (biggest_winner, biggest_loser)

let make (state : 'a State.t) : t =
  let h = state.order_history in
  assert (List.is_empty h.active);
  let stats = state.stats in
  let biggest_winner, biggest_loser = biggest h in
  {
    num_orders = Order.History.length h;
    sharpe_ratio = sharpe_ratio stats;
    win_percentage = win_percentage h;
    average_trade_net = average_trade_net h;
    average_profit = average_profit h;
    average_loss = average_loss h;
    profit_factor = profit_factor h;
    stddev_returns = stddev_returns stats;
    biggest_winner;
    biggest_loser;
  }
