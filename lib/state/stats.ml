type t = {
  num_orders : int;
  num_buy_orders : int;
  num_sell_orders : int;
  total_volume : int;
  total_cash_traded : float;
  symbols_traded : int;
  profit_loss : float;
}

module Bars = Longleaf_bars

let num_orders arr =
  let lengths = Vector.map List.length arr in
  Vector.fold ( + ) 0 lengths

let calculate_order_stats orders =
  (* Single fold to compute all statistics at once *)
  let num_orders, num_buy_orders, num_sell_orders, total_volume,
      total_cash_traded, symbols_list, profit_loss =
    Vector.fold
      (fun (n, nb, ns, tv, tc, syms, pl) order_list ->
        List.fold_left
          (fun (n, nb, ns, tv, tc, syms, pl) (order : Order.t) ->
            let n = n + 1 in
            let nb = if Trading_types.Side.equal order.side Buy then nb + 1 else nb in
            let ns = if Trading_types.Side.equal order.side Sell then ns + 1 else ns in
            let tv = tv + order.qty in
            let tc = tc +. (order.price *. float_of_int order.qty) in
            let syms = order.symbol :: syms in
            let pl = match order.profit with Some p -> pl +. p | None -> pl in
            (n, nb, ns, tv, tc, syms, pl))
          (n, nb, ns, tv, tc, syms, pl)
          order_list)
      (0, 0, 0, 0, 0.0, [], 0.0)
      orders
  in
  let symbols_traded =
    symbols_list
    |> List.uniq ~eq:Instrument.equal
    |> List.length
  in
  ( num_orders,
    num_buy_orders,
    num_sell_orders,
    total_volume,
    total_cash_traded,
    symbols_traded,
    profit_loss )

let make orders (_x : Bars.t) =
  let ( num_orders,
        num_buy_orders,
        num_sell_orders,
        total_volume,
        total_cash_traded,
        symbols_traded,
        profit_loss ) =
    calculate_order_stats orders
  in
  {
    num_orders;
    num_buy_orders;
    num_sell_orders;
    total_volume;
    total_cash_traded;
    symbols_traded;
    profit_loss;
  }

let from_positions (positions : Positions.t) (bars : Bars.t) =
  (* Create a vector from positions data *)
  let all_orders = ref [] in

  (* Collect all orders from positions *)
  Positions.fold positions () (fun _symbol order_list () ->
      all_orders := order_list @ !all_orders);

  (* Determine the maximum tick to size our vector *)
  let max_tick =
    if List.is_empty !all_orders then 0
    else
      List.fold_left
        (fun acc (order : Order.t) -> max acc order.tick)
        0 !all_orders
  in

  (* Create vector with appropriate size *)
  let orders_by_tick = Vector.init (max_tick + 1) (fun _ -> []) in

  (* Group orders by tick *)
  List.iter
    (fun (order : Order.t) ->
      let current_orders = Vector.get orders_by_tick order.tick in
      Vector.set orders_by_tick order.tick (order :: current_orders))
    !all_orders;

  make orders_by_tick bars

(** TradeStats - Per-trade statistical analysis for edge detection *)
module TradeStats = struct
  type t = {
    num_trades : int;
    num_winners : int;
    num_losers : int;
    num_breakeven : int;
    win_rate : float;
    loss_rate : float;
    avg_return : float;
    median_return : float;
    avg_winner : float;
    avg_loser : float;
    profit_factor : float;  (* Total wins / Total losses *)
    expectancy : float;  (* Expected value per trade *)
    std_dev : float;  (* Standard deviation of returns *)
    sharpe : float;  (* Sharpe ratio = mean / std_dev *)
    max_winner : float;
    max_loser : float;
    total_profit : float;
    total_loss : float;
    (* Statistical significance *)
    t_statistic : float;  (* T-test for mean > 0 *)
    p_value : float option;  (* Probability result is due to chance *)
  }
  [@@deriving show, yojson]

  (** Compute median of a sorted list *)
  let median (sorted_list : float list) : float =
    let len = List.length sorted_list in
    if len = 0 then 0.0
    else if len mod 2 = 1 then List.nth sorted_list (len / 2)
    else
      let mid = len / 2 in
      let lower = List.nth sorted_list (mid - 1) in
      let upper = List.nth sorted_list mid in
      (lower +. upper) /. 2.0

  (** Approximate error function for normal CDF *)
  let erf_approx (z : float) : float =
    (* Abramowitz and Stegun approximation *)
    let a1 = 0.254829592 in
    let a2 = -0.284496736 in
    let a3 = 1.421413741 in
    let a4 = -1.453152027 in
    let a5 = 1.061405429 in
    let p = 0.3275911 in
    let sign = if Float.compare z 0.0 < 0 then -1.0 else 1.0 in
    let z_abs = abs_float z in
    let t = 1.0 /. (1.0 +. (p *. z_abs)) in
    let y =
      1.0
      -. (((((a5 *. t) +. a4) *. t +. a3) *. t +. a2) *. t +. a1)
         *. t
         *. exp (-.(z_abs *. z_abs))
    in
    sign *. y

  (** Student's t-distribution CDF approximation for p-value *)
  let t_distribution_cdf (t : float) (df : int) : float =
    (* Simple approximation using normal distribution for df > 30 *)
    (* For smaller df, this is less accurate but gives rough estimate *)
    if df > 30 then
      (* Use normal approximation *)
      let x = t /. sqrt (float_of_int df /. (float_of_int df -. 2.0)) in
      0.5 *. (1.0 +. erf_approx (x /. sqrt 2.0))
    else
      (* For small samples, return conservative estimate *)
      0.5  (* Conservative estimate - need more trades for reliable p-value *)

  (** Compute all trade statistics from a list of orders *)
  let compute (orders : Order.t list) : t option =
    (* Single pass: extract closed trades and compute basic stats simultaneously *)
    let num_trades, num_winners, num_losers, num_breakeven,
        total_profit, total_loss, total_return,
        max_winner, max_loser, closed_trades =
      List.fold_left
        (fun (nt, nw, nl, nb, tp, tl, tr, mw, ml, ct) (order : Order.t) ->
          match order.profit with
          | None -> (nt, nw, nl, nb, tp, tl, tr, mw, ml, ct)
          | Some p ->
              let nt = nt + 1 in
              let nw, nb, nl, tp, tl, mw, ml =
                if Float.compare p 0.0 > 0 then
                  (nw + 1, nb, nl, tp +. p, tl,
                   (if Float.compare p mw > 0 then p else mw), ml)
                else if Float.equal p 0.0 then
                  (nw, nb + 1, nl, tp, tl, mw, ml)
                else
                  (nw, nb, nl + 1, tp, tl +. (Float.abs p),
                   mw, (if Float.compare p ml < 0 then p else ml))
              in
              let tr = tr +. p in
              let ct = p :: ct in
              (nt, nw, nl, nb, tp, tl, tr, mw, ml, ct))
        (0, 0, 0, 0, 0.0, 0.0, 0.0, neg_infinity, infinity, [])
        orders
    in

    if num_trades = 0 then None
    else
      let win_rate = float_of_int num_winners /. float_of_int num_trades in
      let loss_rate = float_of_int num_losers /. float_of_int num_trades in

      let avg_return = total_return /. float_of_int num_trades in

      let avg_winner =
        if num_winners > 0 then total_profit /. float_of_int num_winners
        else 0.0
      in

      let avg_loser =
        if num_losers > 0 then total_loss /. float_of_int num_losers else 0.0
      in

      (* Median requires sorting - unavoidable *)
      let sorted_returns = List.sort Float.compare closed_trades in
      let median_return = median sorted_returns in

      let profit_factor =
        if Float.compare total_loss 0.0 > 0 then total_profit /. total_loss else
        if Float.compare total_profit 0.0 > 0 then infinity
        else 0.0
      in

      let expectancy =
        (win_rate *. avg_winner) -. (loss_rate *. avg_loser)
      in

      (* Standard deviation - single pass *)
      let variance =
        List.fold_left
          (fun acc r -> acc +. ((r -. avg_return) ** 2.0))
          0.0 closed_trades
        /. float_of_int num_trades
      in
      let std_dev = sqrt variance in

      let sharpe =
        if Float.compare std_dev 0.0 > 0 then avg_return /. std_dev else 0.0
      in

      let max_winner = if num_winners > 0 then max_winner else 0.0 in
      let max_loser = if num_losers > 0 then max_loser else 0.0 in

      let t_statistic =
        if Float.compare std_dev 0.0 > 0 && num_trades > 1 then
          avg_return /. (std_dev /. sqrt (float_of_int num_trades))
        else 0.0
      in

      let p_value =
        if num_trades > 1 then
          let df = num_trades - 1 in
          let cdf = t_distribution_cdf t_statistic df in
          Some (1.0 -. cdf)
        else None
      in

      Some
        {
          num_trades;
          num_winners;
          num_losers;
          num_breakeven;
          win_rate;
          loss_rate;
          avg_return;
          median_return;
          avg_winner;
          avg_loser;
          profit_factor;
          expectancy;
          std_dev;
          sharpe;
          max_winner;
          max_loser;
          total_profit;
          total_loss;
          t_statistic;
          p_value;
        }

  (** Pretty-print trade statistics *)
  let to_string (stats : t) : string =
    Printf.sprintf
      {|Trade Statistics:
  Total Trades: %d (W: %d, L: %d, BE: %d)
  Win Rate: %.2f%% | Loss Rate: %.2f%%

  Returns:
    Average: $%.2f | Median: $%.2f
    Std Dev: $%.2f | Sharpe: %.3f

  Winners:
    Avg Winner: $%.2f | Max Winner: $%.2f
    Total Profit: $%.2f

  Losers:
    Avg Loser: $%.2f | Max Loser: $%.2f
    Total Loss: $%.2f

  Edge Metrics:
    Profit Factor: %.3f (wins/losses ratio)
    Expectancy: $%.2f per trade

  Statistical Significance:
    T-statistic: %.3f
    P-value: %s
    Edge is %s
|}
      stats.num_trades stats.num_winners stats.num_losers stats.num_breakeven
      (stats.win_rate *. 100.0)
      (stats.loss_rate *. 100.0)
      stats.avg_return stats.median_return stats.std_dev stats.sharpe
      stats.avg_winner stats.max_winner stats.total_profit stats.avg_loser
      stats.max_loser stats.total_loss stats.profit_factor stats.expectancy
      stats.t_statistic
      (match stats.p_value with
      | Some p -> Printf.sprintf "%.4f (%.2f%% chance of luck)" p (p *. 100.0)
      | None -> "N/A (not enough trades)")
      (match stats.p_value with
      | Some p when Float.compare p 0.05 < 0 -> "statistically significant (p < 0.05)"
      | Some p when Float.compare p 0.10 < 0 -> "marginally significant (p < 0.10)"
      | Some _ -> "NOT statistically significant (p >= 0.10)"
      | None -> "unknown (need more trades)")

  (** Check if strategy has a real edge based on multiple criteria *)
  (* Note: Sharpe ratio is timeframe-sensitive (per-trade vs daily vs annual) so we exclude it *)
  let has_edge ?(min_trades = 30) ?(min_win_rate = 0.50)
      ?(max_p_value = 0.05) (stats : t) : bool =
    stats.num_trades >= min_trades
    && Float.compare stats.win_rate min_win_rate >= 0
    && Float.compare stats.expectancy 0.0 > 0
    && Float.compare stats.profit_factor 1.0 > 0
    &&
    match stats.p_value with
    | Some p -> Float.compare p max_p_value <= 0
    | None -> false  (* Not enough data to confirm edge *)
end
