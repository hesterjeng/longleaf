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
  let triggered_orders, live_orders =
    List.partition
      (fun (o : Order.t) ->
        assert (not @@ Trading_types.OrderType.equal o.order_type Market);
        let order_price = o.price in
        let symbol = o.symbol in
        let current_price latest =
          Bars.Latest.get_opt latest symbol |> function
          | Some x -> x
          | None ->
              invalid_arg
              @@ Format.asprintf
                   "Missing data for symbol %s when updating backend position"
                   symbol
        in
        let previous_price = current_price previous |> Item.last in
        let current_price = current_price latest |> Item.last in
        let crossing x0 x1 =
          (x0 <=. order_price && x1 >=. order_price)
          || (x0 >=. order_price && x1 <=. order_price)
        in
        let triggered = crossing previous_price current_price in
        triggered)
      x.live_orders
  in
  let* orders_executed =
    List.fold_left
      (fun acc o ->
        Eio.traceln "[backend_position] Converting order to Market order.";
        let market_order : Order.t = { o with order_type = Market } in
        let* acc = acc in
        let acc = execute_order acc market_order in
        acc)
      (Ok x) triggered_orders
  in
  Result.return { orders_executed with live_orders }

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
