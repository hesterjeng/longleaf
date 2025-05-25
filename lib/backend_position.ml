module Portfolio = struct
  type t = (Instrument.t * int) list [@@deriving show]

  let qty p symbol =
    List.Assoc.get ~eq:Instrument.equal symbol p |> Option.get_or ~default:0

  let set_qty (portfolio : t) symbol qty =
    List.Assoc.set ~eq:Instrument.equal symbol qty portfolio
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

let qty (pos : t) symbol =
  List.find_map
    (fun (instrument, qty) ->
      match Instrument.equal instrument symbol with
      | true -> Some qty
      | false -> None)
    pos.portfolio
  |> Option.get_or ~default:0

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
    (fun (sym, qty) ->
      match qty with
      | 0 -> None
      | _ -> Some sym)
    pos.portfolio

let value pos (latest : Bars.Latest.t) =
  let ( let* ) = Result.( let* ) in
  (fun f -> List.fold_left f (Ok pos.cash) pos.portfolio)
  @@ fun previous_value (instrument, qty) ->
  match qty with
  | 0 -> previous_value
  | n ->
    let* previous_value = previous_value in
    let* item = Bars.Latest.get latest instrument in
    let symbol_price = Item.last item in
    let symbol_value = Float.of_int n *. symbol_price in
    Result.return @@ (symbol_value +. previous_value)

let is_empty (x : t) =
  List.fold_left (fun acc (_, qty) -> acc && qty = 0) true x.portfolio

let execute_order (pos : t) (order : Order.t) : (t, Error.t) result =
  let symbol = order.symbol in
  let price = order.price in
  let current_amt = Portfolio.qty pos.portfolio symbol in
  let order_qty = order.qty in
  match (order.side, order.order_type) with
  | Buy, Market ->
    let portfolio =
      Portfolio.set_qty pos.portfolio symbol (current_amt + order_qty)
    in
    let res = set_cash pos @@ (pos.cash -. (price *. Float.of_int order_qty)) in
    Ok { res with portfolio }
  | Sell, Market ->
    let portfolio =
      Portfolio.set_qty pos.portfolio symbol (current_amt - order_qty)
    in
    let res = set_cash pos @@ (pos.cash +. (price *. Float.of_int order_qty)) in
    Ok { res with portfolio }
  | Buy, Stop
  | Sell, Stop ->
    Result.return @@ { pos with live_orders = order :: pos.live_orders }
  | _ -> Result.fail @@ `UnsupportedOrder (Order.show order)

(* Execute Stop/Limit orders in the live field.  Market orders should not be in the live field. *)
let update (x : t) ~(previous : Bars.Latest.t) (latest : Bars.Latest.t) =
  let ( let* ) = Result.( let* ) in
  let ( let@ ) = Fun.( let@ ) in
  (* Fold with an empty list of live orders as the accumulator. *)
  let@ positions f =
    List.fold_left f (Ok { x with live_orders = [] }) x.live_orders
  in
  fun order ->
    assert (not @@ Trading_types.OrderType.equal order.order_type Market);
    let* positions = positions in
    let order_price = order.price in
    let symbol = order.symbol in
    let* current_price =
      Bars.Latest.get latest symbol |> Result.map Item.last
    in
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
  let ( let@ ) = Fun.( let@ ) in
  let@ ok f = List.fold_left f (Ok pos) pos.portfolio in
  fun (instrument, qty) ->
    let* pos = ok in
    match qty with
    | 0 -> Ok pos
    | qty ->
      let side = if qty >= 0 then Side.Sell else Side.Buy in
      let* latest = Bars.Latest.get bars instrument in
      let order : Order.t =
        let tif = TimeInForce.GoodTillCanceled in
        let order_type = OrderType.Market in
        let qty = Int.abs qty in
        let price = Item.last latest in
        let timestamp = Item.timestamp latest in
        Order.make ~tick:(-1) ~symbol:instrument ~side ~tif ~order_type ~qty
          ~price ~timestamp ~profit:None ~reason:[ "Liquidating" ]
      in
      Eio.traceln "Liquidation @[%a@]@." Order.pp order;
      let* res = execute_order pos order in
      Ok res
