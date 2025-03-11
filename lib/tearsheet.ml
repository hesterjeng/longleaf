type t = {
  position_taken_ratio : float;
  num_orders : int;
  sharpe_ratio : float;
  win_percentage : float;
  average_trade_net : float;
  average_profit : float;
  average_loss : float;
  stddev_returns : float;
  profit_factor : float;
  annualized_value : float;
  compound_growth_rate : float;
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
    List.head_opt stats.history
    |> Option.get_exn_or
         "stats.ml: Expected to get final element of stats in backtest"
  in
  let values =
    List.map
      (fun (x : Stats.item) -> x.value -. x.risk_free_value)
      stats.history
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
    List.map (fun (x : Stats.item) -> x.value) stats.history |> Array.of_list
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

(* let annualized_value_naive (state : 'a State.t) = *)
(*   (\* 23400 seconds per trading day *\) *)
(*   (\* 251 trading days per year *\) *)
(*   (\* ~ 5873400 trading seconds per year *\) *)
(*   let ticks_per_year = 5873400 |> Float.of_int in *)
(*   let ending_tick = state.tick |> Float.of_int in *)
(*   let profit = Backend_position.get_cash state.positions -. 100000.0 in *)
(*   assert (Backend_position.is_empty state.positions); *)
(*   profit /. ending_tick *. ticks_per_year *)

let compound_growth_rate (state : 'a State.t) =
  (* 23400 seconds per trading day *)
  (* 251 trading days per year *)
  (* ~ 5873400 trading seconds per year *)
  assert (Backend_position.is_empty state.positions);
  let ( ^ ) = Owl_maths.pow in
  let ticks_per_year = 5873400.0 /. state.tick_length in
  let exponent = ticks_per_year /. Float.of_int state.tick in
  let ratio = Backend_position.get_cash state.positions /. 100000.0 in
  let cagr = (ratio ^ exponent) -. 1.0 in
  cagr

(* let annualized_value (state : 'a State.t) = *)
(*   let ticks_per_year = 5873400.0 /. state.tick_length in *)
(*   let tick = state.tick |> Float.of_int in *)
(*   let ending_cash = Backend_position.get_cash state.positions in *)
(*   let starting_cash = 100000.0 in *)
(*   0.0 *)

let annualized_value (state : 'a State.t) =
  let cgr = compound_growth_rate state in
  100000.0 *. cgr

let make (state : 'a State.t) : t =
  let h = state.order_history in
  let stats = state.stats in
  let position_taken_ratio =
    Float.of_int state.stats.position_ratio.positions_taken
    /. Float.of_int state.stats.position_ratio.positions_possible
  in
  let biggest_winner, biggest_loser = biggest h in
  {
    num_orders = Order.History.length h;
    sharpe_ratio = sharpe_ratio stats;
    win_percentage = win_percentage h;
    average_trade_net = average_trade_net h;
    average_profit = average_profit h;
    average_loss = average_loss h;
    annualized_value = annualized_value state;
    compound_growth_rate = compound_growth_rate state;
    profit_factor = profit_factor h;
    stddev_returns = stddev_returns stats;
    biggest_winner;
    biggest_loser;
    position_taken_ratio;
  }
