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
  let all_orders = Vector.fold (fun acc order_list -> order_list @ acc) [] orders in
  let num_orders = List.length all_orders in
  let num_buy_orders = List.count (fun (order : Order.t) -> 
    Trading_types.Side.equal order.side Buy) all_orders in
  let num_sell_orders = List.count (fun (order : Order.t) -> 
    Trading_types.Side.equal order.side Sell) all_orders in
  let total_volume = List.fold_left (fun acc (order : Order.t) -> 
    acc + order.qty) 0 all_orders in
  let total_cash_traded = List.fold_left (fun acc (order : Order.t) -> 
    acc +. (order.price *. float_of_int order.qty)) 0.0 all_orders in
  let symbols_traded = all_orders 
    |> List.map (fun (order : Order.t) -> order.symbol) 
    |> List.uniq ~eq:Instrument.equal 
    |> List.length in
  let profit_loss = List.fold_left (fun acc (order : Order.t) ->
    match order.profit with
    | Some p -> acc +. p
    | None -> acc) 0.0 all_orders in
  (num_orders, num_buy_orders, num_sell_orders, total_volume, total_cash_traded, symbols_traded, profit_loss)

let make orders (_x : Bars.t) =
  let (num_orders, num_buy_orders, num_sell_orders, total_volume, total_cash_traded, symbols_traded, profit_loss) = 
    calculate_order_stats orders in
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
    else List.fold_left (fun acc (order : Order.t) -> 
      max acc order.tick) 0 !all_orders in
  
  (* Create vector with appropriate size *)
  let orders_by_tick = Vector.init (max_tick + 1) (fun _ -> []) in
  
  (* Group orders by tick *)
  List.iter (fun (order : Order.t) ->
    let current_orders = Vector.get orders_by_tick order.tick in
    Vector.set orders_by_tick order.tick (order :: current_orders)
  ) !all_orders;
  
  make orders_by_tick bars
