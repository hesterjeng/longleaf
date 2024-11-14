(* A module for containing the information about the current position for backtesting *)
(* Maybe also for keeping track of things during live trading, but ideally we should *)
(* be using the true information from the server for this rather than keeping track. *)
(* TODO: *)
(* Maybe a warning if the position the brokerage thinks we have and this diverges is a good idea. *)

module Order = Trading_types.Order

module Make () = struct
  type t = { position : (string, int) Hashtbl.t; mutable cash : float }
  [@@deriving show]

  let make () = { position = Hashtbl.create 0; cash = 100000.0 }
  let set_cash x amt = x.cash <- amt
  let pos = make ()

  let execute_order (order : Order.t) =
    let symbol = order.symbol in
    let qty = order.qty in
    let price = order.price in
    let current_amt =
      Hashtbl.get pos.position symbol |> Option.get_or ~default:0
    in
    match (order.side, order.order_type) with
    | Buy, Market ->
        Hashtbl.replace pos.position symbol (current_amt + qty);
        set_cash pos @@ (pos.cash -. (price *. Float.of_int qty))
    | Sell, Market ->
        Hashtbl.replace pos.position symbol (current_amt + qty);
        set_cash pos @@ (pos.cash +. (price *. Float.of_int qty))
    | _ ->
        invalid_arg
        @@ Format.asprintf "@[Unsupported order: %a@]@." Order.pp order
end
