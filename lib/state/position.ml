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
