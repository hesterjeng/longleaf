type pos = (string, int) Hashtbl.t [@@deriving show]

(* A module for containing the information about the current position for backtesting *)
(* Maybe also for keeping track of things during live trading, but ideally we should *)
(* be using the true information from the server for this rather than keeping track. *)
(* TODO: *)
(* Maybe a warning if the position the brokerage thinks we have and this diverges is a good idea. *)

type t = { position : pos; cash : float } [@@deriving show]

let make () = { position = Hashtbl.create 5; cash = 100000.0 }
let set_cash x cash = { x with cash }
let get_cash pos = pos.cash
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
