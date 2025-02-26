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
  +. List.fold_left
       (fun value (o : Order.t) ->
         match o.side with
         | Buy -> value +. (o.price *. Int.to_float o.qty)
         | Sell -> value)
       0.0 pos.live_orders

let get_position pos = pos.position

let symbols pos =
  Hashtbl.to_iter pos.position
  |> Iter.filter_map (fun (symbol, value) ->
         if value <> 0 then Some symbol else None)
  |> Iter.to_list

let qty pos symbol = Hashtbl.get_or pos.position ~default:0 symbol

let value pos (latest : Bars.Latest.t) =
  (fun f -> Hashtbl.fold f pos.position pos.cash)
  @@ fun symbol qty previous_value ->
  let symbol_price = Item.last @@ Bars.Latest.get latest symbol in
  let symbol_value = Float.of_int qty *. symbol_price in
  symbol_value +. previous_value

let is_empty (x : t) =
  Hashtbl.fold (fun _ qty acc -> acc && qty = 0) x.position true

let mem (x : t) symbol =
  let found = Hashtbl.get x.position symbol in
  match found with Some 0 | None -> false | Some _ -> true

let update (x : t) (latest : Bars.Latest.t) =
  let triggered_orders, live_orders =
    List.partition
      (fun (o : Order.t) ->
        let price = o.price in
        let symbol = o.symbol in
        let current_price =
          Bars.Latest.get_opt latest symbol |> function
          | Some x -> x
          | None ->
              invalid_arg
              @@ Format.asprintf
                   "Missing data for symbol %s when updating backend position"
                   symbol
        in
        let previous_price = 0.0 in
        false)
      x.live_orders
  in
  ()

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
      let latest = Bars.Latest.get bars symbol in
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
