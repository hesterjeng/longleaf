module Order = Trading_types.Order

type pos = (string, int) Hashtbl.t [@@deriving show]

module type S = sig
  val execute_order : _ State.t -> Order.t -> (unit, string) Result.t
  val liquidate : _ State.t -> Bars.Latest.t -> (unit, string) Result.t
  val get_cash : unit -> float
  val set_cash : float -> unit
  val symbols : unit -> string list
  val qty : string -> int
end

module Generative () : S = struct
  (* A module for containing the information about the current position for backtesting *)
  (* Maybe also for keeping track of things during live trading, but ideally we should *)
  (* be using the true information from the server for this rather than keeping track. *)
  (* TODO: *)
  (* Maybe a warning if the position the brokerage thinks we have and this diverges is a good idea. *)

  module Order = Trading_types.Order

  type t = { position : pos; mutable cash : float } [@@deriving show]

  let make () = { position = Hashtbl.create 0; cash = 100000.0 }
  let pos = make ()
  let set_cash amt = pos.cash <- amt
  let get_cash () = pos.cash
  let get_position () = pos.position
  let symbols () = Hashtbl.keys_list pos.position
  let qty symbol = Hashtbl.get_or pos.position ~default:0 symbol

  let execute_order state (order : Order.t) =
    State.record_order state order;
    let symbol = order.symbol in
    let qty = order.qty in
    let price = order.price in
    let current_amt =
      Hashtbl.get pos.position symbol |> Option.get_or ~default:0
    in
    match (order.side, order.order_type) with
    | Buy, Market ->
        Hashtbl.replace pos.position symbol (current_amt + qty);
        set_cash @@ (pos.cash -. (price *. Float.of_int qty));
        Ok ()
    | Sell, Market ->
        Hashtbl.replace pos.position symbol (current_amt - qty);
        set_cash @@ (pos.cash +. (price *. Float.of_int qty));
        Ok ()
    | _ -> Result.fail @@ Format.asprintf "Unsupported order: %a" Order.pp order

  let liquidate state (bars : Bars.Latest.t) =
    let open Trading_types in
    let ( let* ) = Result.( let* ) in
    let fold f = Hashtbl.fold f pos.position (Ok ()) in
    fold @@ fun symbol qty ok ->
    let* () = ok in
    match qty with
    | 0 -> Ok ()
    | qty ->
        let side = if qty >= 0 then Side.Sell else Side.Buy in
        let latest = Bars.Latest.get bars symbol in
        let order : Order.t =
          let tif = TimeInForce.GoodTillCanceled in
          let order_type = OrderType.Market in
          let qty = Int.abs qty in
          let price = Item.last latest in
          let timestamp = Item.timestamp latest in
          Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp
            ~profit:None ~reason:"Liquidating"
        in
        Eio.traceln "@[%a@]@." Order.pp order;
        let* () = execute_order state order in
        Ok ()
end
