type pos = (string, int) Hashtbl.t [@@deriving show]

(* A module for containing the information about the current position for backtesting *)
(* Maybe also for keeping track of things during live trading, but ideally we should *)
(* be using the true information from the server for this rather than keeping track. *)
(* TODO: *)
(* Maybe a warning if the position the brokerage thinks we have and this diverges is a good idea. *)

type t = { position : pos; cash : float; live_orders : Order.t list }
[@@deriving show]

let make () = { position = Hashtbl.create 5; cash = 100000.0; live_orders = [] }
let set_cash x cash = { x with cash }

let get_cash pos =
  pos.cash
  -. List.fold_left
       (fun value (o : Order.t) ->
         match o.side with
         | Buy -> value +. (o.price *. Int.to_float o.qty)
         | Sell -> 0.0)
       0.0 pos.live_orders

let get_position pos = pos.position

let symbols pos =
  Hashtbl.to_iter pos.position
  |> Iter.filter_map (fun (symbol, value) ->
         if value <> 0 then Some symbol else None)
  |> Iter.to_list

let qty pos symbol = Hashtbl.get_or pos.position ~default:0 symbol

let value pos (latest : Bars.Latest.t) =
  let ( let* ) = Result.( let* ) in
  (fun f -> Hashtbl.fold f pos.position (Ok pos.cash))
  @@ fun symbol qty previous_value ->
  let* previous_value = previous_value in
  let* item = Bars.Latest.get latest symbol in
  let symbol_price = Item.last item in
  let symbol_value = Float.of_int qty *. symbol_price in
  Result.return @@ (symbol_value +. previous_value)

let is_empty (x : t) =
  Hashtbl.fold (fun _ qty acc -> acc && qty = 0) x.position true

let mem (x : t) symbol =
  let found = Hashtbl.get x.position symbol in
  match found with Some 0 | None -> false | Some _ -> true

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
      let res = set_cash pos @@ (pos.cash -. (price *. Float.of_int qty)) in
      Ok res
  | Sell, Market ->
      Hashtbl.replace pos.position symbol (current_amt - qty);
      let res = set_cash pos @@ (pos.cash +. (price *. Float.of_int qty)) in
      Ok res
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
  fold @@ fun position order ->
  assert (not @@ Trading_types.OrderType.equal order.order_type Market);
  let* position = position in
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
      let* position = execute_order position market_order in
      Result.return position
  | false ->
      (* The order was not activated, put it in the live orders list *)
      Result.return
        { position with live_orders = order :: position.live_orders }

let liquidate pos (bars : Bars.Latest.t) =
  let open Trading_types in
  let ( let* ) = Result.( let* ) in
  let fold f = Hashtbl.fold f pos.position (Ok pos) in
  fold @@ fun symbol qty ok ->
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
