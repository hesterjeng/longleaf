module Portfolio = struct
  type t = (Instrument.t * int) list [@@deriving show]

  let qty p symbol =
    List.Assoc.get ~eq:Instrument.equal symbol p |> Option.get_or ~default:0
end

(* A module for containing the information about the current position for backtesting *)
(* Maybe also for keeping track of things during live trading, but ideally we should *)
(* be using the true information from the server for this rather than keeping track. *)
(* TODO: *)
(* Maybe a warning if the position the brokerage thinks we have and this diverges is a good idea. *)

type t = {
  portfolio : Portfolio.t;
  cash : float;
  live_orders : Order.t list; (* contracts : Contract.Position.t list; *)
}
[@@deriving show]

let make () = { portfolio = []; cash = 100000.0; live_orders = [] }
let set_cash x cash = { x with cash }

let get_cash pos =
  pos.cash
  -. List.fold_left
       (fun value (o : Order.t) ->
         match o.side with
         | Buy -> value +. (o.price *. Int.to_float o.qty)
         | Sell -> 0.0)
       0.0 pos.live_orders

let symbols pos =
  List.filter_map
    (fun (sym, (p : Position.t)) ->
      match p.qty with 0 -> None | _ -> Some sym)
    pos.positions

let value pos (latest : Bars.Latest.t) =
  let ( let* ) = Result.( let* ) in
  (fun f -> List.fold_left f (Ok pos.cash) pos.positions)
  @@ fun previous_value (symbol, p) ->
  let* previous_value = previous_value in
  let* item = Bars.Latest.get latest symbol in
  let symbol_price = Item.last item in
  let symbol_value = Float.of_int p.qty *. symbol_price in
  Result.return @@ (symbol_value +. previous_value)

let is_empty (x : t) =
  List.fold_left
    (fun acc (_, (p : Position.t)) -> acc && p.qty = 0)
    true x.positions

let execute_order (pos : t) (order : Order.t) =
  let symbol = order.symbol in
  let price = order.price in
  (* let current_amt = Positions.get pos.positions symbol in *)
  let current_amt = Portfolio.qty pos.positions symbol in
  let order_qty = order.qty in
  match (order.side, order.order_type) with
  | Buy, Market ->
      let positions = set_qty pos symbol (current_amt + order_qty) in
      let res =
        set_cash pos @@ (pos.cash -. (price *. Float.of_int order_qty))
      in
      Ok { res with positions }
  | Sell, Market ->
      let positions =
        Positions.set symbol (current_amt - order_qty) pos.positions
      in
      let res =
        set_cash pos @@ (pos.cash +. (price *. Float.of_int order_qty))
      in
      Ok { res with positions }
  | Buy, Stop | Sell, Stop ->
      Result.return @@ { pos with live_orders = order :: pos.live_orders }
  | _ -> Result.fail @@ `UnsupportedOrder order

(* Execute Stop/Limit orders in the live field.  Market orders should not be in the live field. *)
let update (x : t) ~(previous : Bars.Latest.t) (latest : Bars.Latest.t) =
  let ( let* ) = Result.( let* ) in
  (* Fold with an empty list of live orders as the accumulator. *)
  let fold f =
    List.fold_left f (Ok { x with live_orders = [] }) x.live_orders
  in
  fold @@ fun positions order ->
  assert (not @@ Trading_types.OrderType.equal order.order_type Market);
  let* positions = positions in
  let order_price = order.price in
  let symbol = order.symbol in
  let* current_price = Bars.Latest.get latest symbol |> Result.map Item.last in
  let* previous_price =
    Bars.Latest.get previous symbol |> Result.map Item.last
  in
  let crossing x0 x1 =
    (x0 <=. order_price && x1 >=. order_price)
    || (x0 >=. order_price && x1 <=. order_price)
  in
  (* Check to see if we have crossed the stop/limit order price threshold. *)
  (*  If so, execute the order as a market order.  Otherwise, add the order to the live order list.*)
  match crossing previous_price current_price with
  | true ->
      (* The order has been triggered, execute it now *)
      let market_order : Order.t = { order with order_type = Market } in
      let* positions = execute_order positions market_order in
      Result.return positions
  | false ->
      (* The order was not activated, put it in the live orders list *)
      Result.return
        { positions with live_orders = order :: positions.live_orders }

let liquidate pos (bars : Bars.Latest.t) =
  let open Trading_types in
  let ( let* ) = Result.( let* ) in
  let fold f = List.fold_left f (Ok pos) pos.positions in
  fold @@ fun ok (symbol, qty) ->
  let* pos = ok in
  match qty with
  | 0 -> Ok pos
  | qty ->
      let side = if qty >= 0 then Side.Sell else Side.Buy in
      let* latest = Bars.Latest.get bars symbol in
      let order : Order.t =
        let tif = TimeInForce.GoodTillCanceled in
        let order_type = OrderType.Market in
        let qty = Int.abs qty in
        let price = Item.last latest in
        let timestamp = Item.timestamp latest in
        Order.make ~tick:(-1) ~symbol ~side ~tif ~order_type ~qty ~price
          ~timestamp ~profit:None ~reason:[ "Liquidating" ]
      in
      Eio.traceln "@[%a@]@." Order.pp order;
      let* res = execute_order pos order in
      Ok res
