module Order = Trading_types.Order

type pos = (string, int) Hashtbl.t [@@deriving show]

module type S = sig
  val execute_order : _ State.t -> Time.t -> Order.t -> unit
  val liquidate : _ State.t -> Bars.t -> unit
  val get_cash : unit -> float
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
  let set_cash x amt = x.cash <- amt
  let get_cash () = pos.cash
  let get_position () = pos.position
  let symbols () = Hashtbl.keys_list pos.position
  let qty symbol = Hashtbl.get_or pos.position ~default:0 symbol

  let execute_order state time (order : Order.t) =
    State.record_order state time order;
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
        Hashtbl.replace pos.position symbol (current_amt - qty);
        set_cash pos @@ (pos.cash +. (price *. Float.of_int qty))
    | _ ->
        invalid_arg
        @@ Format.asprintf "@[Unsupported order: %a@]@." Order.pp order

  let liquidate state bars =
    let open Trading_types in
    Hashtbl.iter
      (fun symbol qty ->
        if qty = 0 then ()
        else
          let side = if qty >= 0 then Side.Sell else Side.Buy in
          let latest = Bars.price bars symbol in
          let order : Order.t =
            let tif = TimeInForce.GoodTillCanceled in
            let order_type = OrderType.Market in
            let qty = Int.abs qty in
            let price = latest.close in
            Order.make ~symbol ~side ~tif ~order_type ~qty ~price
          in
          Eio.traceln "@[%a@]@." Order.pp order;
          execute_order state latest.timestamp order)
      pos.position
end
