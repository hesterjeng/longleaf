type t = {
  num_orders : int;
  sharpe_ratio : float;
  win_loss_ratio : float;
  average_trade_net : float;
  average_profit : float;
  average_loss : float;
  stddev_returns : float;
  profit_factor : float;
  biggest_winner : Order.t option;
  biggest_loser : Order.t option;
}
[@@deriving show]

let num_orders (x : Order_history.t) = Vector.length x

let win_loss_ratio (h : Order_history.t) =
  let fold (winners, losers) (x : Order.t) =
    match x.profit with
    | None -> (winners, losers)
    | Some f when f >=. 0.0 -> (winners +. 1.0, losers)
    | Some f when f <. 0.0 -> (winners, losers +. 1.0)
    | Some _ -> (winners, losers)
  in
  let winners, losers = Vector.fold fold (0.0, 0.0) h in
  let winlossratio = winners /. losers in
  winlossratio

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

let average_trade_net (h : Order_history.t) =
  let ( let+ ) = Option.( let+ ) in
  let nets =
    Vector.filter_map
      (fun (x : Order.t) ->
        let+ profit = x.profit in
        profit)
      h
    |> Vector.to_array
  in
  Owl_stats.mean nets

let average_profit (h : Order_history.t) =
  let ( let* ) = Option.( let* ) in
  let nets =
    Vector.filter_map
      (fun (x : Order.t) ->
        let* profit = x.profit in
        match profit >=. 0.0 with true -> Some profit | false -> None)
      h
    |> Vector.to_array
  in
  Owl_stats.mean nets

let average_loss (h : Order_history.t) =
  let ( let* ) = Option.( let* ) in
  let nets =
    Vector.filter_map
      (fun (x : Order.t) ->
        let* profit = x.profit in
        match profit <=. 0.0 with true -> Some profit | false -> None)
      h
    |> Vector.to_array
  in
  Owl_stats.mean nets

let stddev_returns (stats : Stats.t) =
  let returns =
    List.map (fun (x : Stats.item) -> x.value) stats |> Array.of_list
  in
  Owl_stats.std returns

let profit_factor (h : Order_history.t) =
  let profits =
    Vector.fold
      (fun acc (x : Order.t) ->
        match x.profit with
        | None -> acc
        | Some f when f >=. 0.0 -> acc +. f
        | _ -> acc)
      0.0 h
  in
  let losses =
    Vector.fold
      (fun acc (x : Order.t) ->
        match x.profit with
        | None -> acc
        | Some f when f <=. 0.0 -> acc +. f
        | _ -> acc)
      0.0 h
  in
  profits /. (-1.0 *. losses)

let biggest (state : 'a State.t) =
  let ordered_orders =
    state.order_history
    |> Vector.filter_map (fun (o : Order.t) ->
           match o.profit with Some _ -> Some o | None -> None)
    |> Vector.sort Order.cmp_profit
  in
  let biggest_loser =
    try Option.return @@ Vector.get ordered_orders 0 with _ -> None
  in
  let biggest_winner = Vector.pop ordered_orders in
  (biggest_winner, biggest_loser)

let make (state : 'a State.t) : t =
  let h = state.order_history in
  let stats = state.stats in
  let biggest_winner, biggest_loser = biggest state in
  {
    num_orders = num_orders h;
    sharpe_ratio = sharpe_ratio stats;
    win_loss_ratio = win_loss_ratio h;
    average_trade_net = average_trade_net h;
    average_profit = average_profit h;
    average_loss = average_loss h;
    profit_factor = profit_factor h;
    stddev_returns = stddev_returns stats;
    biggest_winner;
    biggest_loser;
  }
