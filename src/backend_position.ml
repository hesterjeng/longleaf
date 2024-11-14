(* A module for containing the information about the current position for backtesting *)
(* Maybe also for keeping track of things during live trading, but ideally we should *)
(* be using the true information from the server for this rather than keeping track. *)
(* TODO: *)
(* Maybe a warning if the position the brokerage thinks we have and this diverges is a good idea. *)

module Order = Trading_types.Order

type t = { position : (string, int) Hashtbl.t; mutable cash : float }
[@@deriving show]

let make () = { position = Hashtbl.create 0; cash = 100000.0 }
let set_cash x amt = x.cash <- amt

let execute_order pos (order : Order.t) =
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

let liquidate position bars =
  let open Trading_types in
  Hashtbl.iter
    (fun symbol qty ->
      if qty = 0 then ()
      else
        let side = if qty >= 0 then Side.Sell else Side.Buy in
        let order : Order.t =
          {
            symbol;
            side;
            tif = TimeInForce.GoodTillCanceled;
            order_type = OrderType.Market;
            qty = Int.abs qty;
            price = (Bars.price bars symbol).close;
          }
        in
        Eio.traceln "@[%a@]@." Order.pp order;
        execute_order position order)
    position.position
