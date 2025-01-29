type t = {
  num_orders : int;
  sharpe_ratio : float;
  win_loss_ratio : float;
  average_trade_net : float;
  average_profit : float;
  average_loss : float;
  stddev_returns : float;
  profit_factor : float;
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
  profits /. losses

let make (h : Order_history.t) (stats : Stats.t) : t =
  {
    num_orders = num_orders h;
    sharpe_ratio = sharpe_ratio stats;
    win_loss_ratio = win_loss_ratio h;
    average_trade_net = average_trade_net h;
    average_profit = average_profit h;
    average_loss = average_loss h;
    profit_factor = profit_factor h;
    stddev_returns = stddev_returns stats;
  }
