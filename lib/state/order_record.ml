type t = {
  order : Order.t;
  id : Order_id.t;
  status : Order.Status.t;
  filled_qty : int;
  remaining_qty : int;
}
[@@deriving show]

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
