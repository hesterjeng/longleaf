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
  (* Allocate cash immediately for buy orders *)
  let new_cash =
    if Trading_types.Side.equal order.side Buy then
      state.cash -. (order.price *. Float.of_int order.qty)
    else state.cash
  in
  {
    state with
    orders = new_orders;
    pending_orders = new_pending;
    cash = new_cash;
  }

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

let cancel_order state order_id =
  match IdMap.find_opt order_id state.orders with
  | Some order_rec
    when Order.Status.is_pending order_rec.status
         || Order.Status.is_active order_rec.status ->
    let order = order_rec.order in
    let updated_record =
      Order_record.update_status order_rec Order.Status.Canceled
    in
    let new_orders = IdMap.add order_id updated_record state.orders in
    let new_pending =
      List.filter
        (fun id -> not (Order_id.equal id order_id))
        state.pending_orders
    in
    let new_active =
      List.filter
        (fun id -> not (Order_id.equal id order_id))
        state.active_orders
    in
    (* Return allocated cash for buy orders *)
    let returned_cash =
      if Trading_types.Side.equal order.side Buy then
        order.price *. Float.of_int order.qty
      else 0.0
    in
    {
      state with
      orders = new_orders;
      pending_orders = new_pending;
      active_orders = new_active;
      cash = state.cash +. returned_cash;
    }
  | _ -> state

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
let get_cash state = state.cash

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
        if new_qty = 0 then
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
        (* For buy orders: return the difference between allocated and actual cost *)
        (* We already allocated order.price * order.qty, so return the difference *)
        (order.price *. Float.of_int filled_qty)
        -. (current_price *. Float.of_int filled_qty)
      else
        (* For sell orders: add the proceeds *)
        current_price *. Float.of_int filled_qty
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
