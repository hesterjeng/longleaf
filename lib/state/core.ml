(* Order Record Module *)
module Order_record = struct
  type t = {
    order : Order.t;
    id : Order_id.t;
    status : Order.Status.t;
    filled_qty : int;
    remaining_qty : int;
  }

  let pp fmt record =
    Format.fprintf fmt
      "{ order = %a; id = %a; status = %a; filled_qty = %d; remaining_qty = %d \
       }"
      Order.pp record.order Order_id.pp record.id Order.Status.pp record.status
      record.filled_qty record.remaining_qty

  let make order id =
    {
      order;
      id;
      status = Order.Status.New;
      filled_qty = 0;
      remaining_qty = order.qty;
    }

  let update_status record status = { record with status }

  let fill record qty =
    let new_filled = record.filled_qty + qty in
    let new_remaining = record.remaining_qty - qty in
    let new_status =
      if new_remaining <= 0 then Order.Status.Filled
      else Order.Status.Partially_filled
    in
    {
      record with
      filled_qty = new_filled;
      remaining_qty = new_remaining;
      status = new_status;
    }
end

(* Position Module *)
module Position = struct
  type t = {
    symbol : Instrument.t;
    quantity : int;
    avg_price : float;
    opening_orders : Order_id.t list;
    closing_orders : Order_id.t list;
    unrealized_pnl : float;
    realized_pnl : float;
  }

  let pp fmt pos =
    Format.fprintf fmt
      "{ symbol = %a; quantity = %d; avg_price = %.2f; realized_pnl = %.2f }"
      Instrument.pp pos.symbol pos.quantity pos.avg_price pos.realized_pnl

  let make_new symbol quantity avg_price order_id =
    {
      symbol;
      quantity;
      avg_price;
      opening_orders = [ order_id ];
      closing_orders = [];
      unrealized_pnl = 0.0;
      realized_pnl = 0.0;
    }

  let add_to_position pos quantity price order_id =
    let new_qty = pos.quantity + quantity in
    let new_avg_price =
      ((pos.avg_price *. Float.of_int pos.quantity)
      +. (price *. Float.of_int quantity))
      /. Float.of_int new_qty
    in
    {
      pos with
      quantity = new_qty;
      avg_price = new_avg_price;
      opening_orders = order_id :: pos.opening_orders;
    }

  let reduce_position pos quantity price order_id =
    let new_qty = pos.quantity - quantity in
    let realized_pnl = Float.of_int quantity *. (price -. pos.avg_price) in
    {
      pos with
      quantity = new_qty;
      closing_orders = order_id :: pos.closing_orders;
      realized_pnl = pos.realized_pnl +. realized_pnl;
    }

  let close_position pos price order_id =
    let realized_pnl = Float.of_int pos.quantity *. (price -. pos.avg_price) in
    {
      pos with
      quantity = 0;
      (* Set quantity to 0 when position is closed *)
      realized_pnl = pos.realized_pnl +. realized_pnl;
      closing_orders = order_id :: pos.closing_orders;
    }

  let is_empty pos = pos.quantity = 0
end

(* Portfolio Snapshot Module *)
module Portfolio_snapshot = struct
  type t = {
    time : Time.t;
    cash : float;
    portfolio_value : float;
    risk_free_value : float;
    orders : Order.t list;
  }

  let make time cash portfolio_value risk_free_value orders =
    { time; cash; portfolio_value; risk_free_value; orders }
end

(* Maps *)
module IdMap = Map.Make (Order_id)
module SymbolMap = Map.Make (Instrument)

(* Main Trading State Module *)
type t = {
  orders : Order_record.t IdMap.t;
  pending_orders : Order_id.t list;
  active_orders : Order_id.t list;
  positions : Position.t SymbolMap.t;
  closed_positions : (Position.t * Time.t) list;
  cash : float;
  portfolio_history : Portfolio_snapshot.t list;
  positions_taken : int;
  positions_possible : int;
}

let pp fmt state =
  Format.fprintf fmt
    "{ orders = <map>; pending_orders = [%d items]; active_orders = [%d \
     items]; positions = <map>; cash = %.2f; ... }"
    (List.length state.pending_orders)
    (List.length state.active_orders)
    state.cash

let empty () : t =
  {
    orders = IdMap.empty;
    pending_orders = [];
    active_orders = [];
    positions = SymbolMap.empty;
    closed_positions = [];
    cash = 100000.0;
    portfolio_history = [];
    positions_taken = 0;
    positions_possible = 0;
  }

(* Order Management Functions *)
let add_order state order_id order =
  let order_record = Order_record.make order order_id in
  let new_orders = IdMap.add order_id order_record state.orders in
  let new_pending = order_id :: state.pending_orders in
  { state with orders = new_orders; pending_orders = new_pending }

let activate_order state order_id =
  match IdMap.find_opt order_id state.orders with
  | Some order_rec when Order.Status.is_pending order_rec.status ->
    let updated_record =
      Order_record.update_status order_rec Order.Status.Accepted
    in
    let new_orders = IdMap.add order_id updated_record state.orders in
    let new_pending =
      List.filter
        (fun id -> not (Order_id.equal id order_id))
        state.pending_orders
    in
    let new_active = order_id :: state.active_orders in
    {
      state with
      orders = new_orders;
      pending_orders = new_pending;
      active_orders = new_active;
    }
  | _ -> state

let get_pending_orders state =
  List.filter_map
    (fun id -> IdMap.find_opt id state.orders)
    state.pending_orders

let get_active_orders state =
  List.filter_map (fun id -> IdMap.find_opt id state.orders) state.active_orders

(* Position Query Functions *)
let get_position_orders state symbol =
  match SymbolMap.find_opt symbol state.positions with
  | Some pos ->
    let opening =
      List.filter_map
        (fun id -> IdMap.find_opt id state.orders)
        pos.Position.opening_orders
    in
    let closing =
      List.filter_map
        (fun id -> IdMap.find_opt id state.orders)
        pos.Position.closing_orders
    in
    (opening, closing)
  | None -> ([], [])

let get_order_position state order_id =
  SymbolMap.fold
    (fun symbol pos acc ->
      match acc with
      | Some _ -> acc (* Already found *)
      | None ->
        if
          List.mem order_id pos.Position.opening_orders
          || List.mem order_id pos.Position.closing_orders
        then Some (symbol, pos)
        else None)
    state.positions None

(* Portfolio Functions *)
let get_cash state =
  let pending_buy_value =
    List.fold_left
      (fun acc id ->
        match IdMap.find_opt id state.orders with
        | Some { order; status; _ } ->
          if
            (Order.Status.is_pending status || Order.Status.is_active status)
            && Trading_types.Side.equal order.side Buy
          then acc +. (order.price *. Float.of_int order.qty)
          else acc
        | None -> acc)
      0.0 state.pending_orders
  in
  state.cash -. pending_buy_value

let qty state symbol =
  match SymbolMap.find_opt symbol state.positions with
  | Some pos -> pos.Position.quantity
  | None -> 0

let symbols state =
  SymbolMap.fold
    (fun symbol pos acc ->
      if not (Position.is_empty pos) then symbol :: acc else acc)
    state.positions []

let is_empty state =
  SymbolMap.for_all (fun _ pos -> Position.is_empty pos) state.positions

let value state (bars : Bars.t) =
  let ( let* ) = Result.( let* ) in
  SymbolMap.fold
    (fun symbol pos acc ->
      match acc with
      | Error _ as e -> e
      | Ok current_value ->
        let* data = Bars.get bars symbol in
        let symbol_price = Bars.Data.get_top data Last in
        let symbol_value = Float.of_int pos.Position.quantity *. symbol_price in
        Result.return (current_value +. symbol_value))
    state.positions (Ok state.cash)

(* Order Execution Functions *)
let execute_order_against_position state order_id filled_qty current_price =
  let ( let* ) = Result.( let* ) in
  match IdMap.find_opt order_id state.orders with
  | None -> Error.fatal "Order not found"
  | Some order_rec ->
    let order = order_rec.order in
    let updated_order_rec = Order_record.fill order_rec filled_qty in
    let new_orders = IdMap.add order_id updated_order_rec state.orders in

    let* new_positions, new_closed_positions =
      match (SymbolMap.find_opt order.symbol state.positions, order.side) with
      | None, side when Trading_types.Side.equal side Buy ->
        (* Opening new position *)
        let new_pos =
          Position.make_new order.symbol filled_qty current_price order_id
        in
        Result.return
          ( SymbolMap.add order.symbol new_pos state.positions,
            state.closed_positions )
      | Some pos, side when Trading_types.Side.equal side Buy ->
        (* Adding to existing position *)
        let updated_pos =
          Position.add_to_position pos filled_qty current_price order_id
        in
        Result.return
          ( SymbolMap.add order.symbol updated_pos state.positions,
            state.closed_positions )
      | Some pos, side when Trading_types.Side.equal side Sell ->
        (* Reducing/closing position *)
        let new_qty = pos.Position.quantity - filled_qty in
        if new_qty <= 0 then
          (* Position closed *)
          let closed_pos = Position.close_position pos current_price order_id in
          Result.return
            ( SymbolMap.remove order.symbol state.positions,
              (closed_pos, order.timestamp) :: state.closed_positions )
        else
          (* Position reduced *)
          let updated_pos =
            Position.reduce_position pos filled_qty current_price order_id
          in
          Result.return
            ( SymbolMap.add order.symbol updated_pos state.positions,
              state.closed_positions )
      | None, side when Trading_types.Side.equal side Sell ->
        Error.fatal "Cannot sell without existing position"
      | _ -> Error.fatal "Invalid order side"
    in

    let cash_change =
      if Trading_types.Side.equal order.side Buy then
        -.(current_price *. Float.of_int filled_qty)
      else current_price *. Float.of_int filled_qty
    in

    let new_active =
      if Order.Status.is_filled updated_order_rec.status then
        List.filter
          (fun id -> not (Order_id.equal id order_id))
          state.active_orders
      else state.active_orders
    in

    Result.return
      {
        state with
        orders = new_orders;
        positions = new_positions;
        closed_positions = new_closed_positions;
        cash = state.cash +. cash_change;
        active_orders = new_active;
      }

(* Statistics Functions *)
let record_portfolio_snapshot state time current_portfolio_value risk_free_value
    =
  let orders =
    List.filter_map
      (fun id ->
        match IdMap.find_opt id state.orders with
        | Some order_rec -> Some order_rec.order
        | None -> None)
      state.active_orders
  in
  let snapshot =
    Portfolio_snapshot.make time state.cash current_portfolio_value
      risk_free_value orders
  in
  { state with portfolio_history = snapshot :: state.portfolio_history }

let add_possible_positions state count =
  { state with positions_possible = state.positions_possible + count }

let increment_positions_taken state =
  { state with positions_taken = state.positions_taken + 1 }
