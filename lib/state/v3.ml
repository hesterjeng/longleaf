module State_config = struct
  type t = { placeholder : bool }
end

(* type position = (int * Order.t list) *)

module PosMap = Map.Make (Instrument)

module Positions = struct
  type t = Order.t list PosMap.t

  let get (pos : t) symbol = PosMap.get symbol pos |> Option.get_or ~default:[]
  (* Result.o *)
  (* |> Result.map_err (fun e -> `FatalError e) *)

  let symbols x = PosMap.keys x |> Iter.to_list

  let fold (pos : t) acc (f : Instrument.t -> Order.t list -> 'a -> 'a) =
    PosMap.fold f pos acc

  let qty (t : t) instrument =
    let pos = get t instrument in
    let res =
      List.fold_left
        (fun acc (order : Order.t) ->
          match order.side with
          | Buy -> acc + order.qty
          | Sell -> acc - order.qty)
        0 pos
    in
    res

  let update (pos : t) (order : Order.t) =
    let symbol = order.symbol in
    let new_pos =
      PosMap.update symbol
        (function
          | Some l -> Some (order :: l)
          | None -> Some [ order ])
        pos
    in
    let qty = qty new_pos symbol in
    match qty with
    | 0 -> PosMap.remove symbol new_pos
    | _ -> new_pos

  let cost_basis (pos : t) symbol =
    let pos = get pos symbol in
    List.fold_left
      (fun acc (order : Order.t) ->
        match order.side with
        | Buy -> acc -. (order.price *. float_of_int order.qty)
        | Sell -> acc +. (order.price *. float_of_int order.qty))
      0.0 pos
end

module type S = sig
  type 'a t
  type 'a res = ('a, Error.t) result

  (* The current "state of the state" *)
  val current : 'a t -> Mode.t

  (* Create a new state with index, data, and content *)
  val make : int -> Bars.t -> 'a -> 'a t res

  (* Print basic info about the state *)
  val pp : 'a t Format.printer

  (* Export the state to a simple string *)
  val show : 'a t -> string

  (* Get the amount of cash available.  This value is updated only when orders are executed.*)
  val cash : 'a t -> float

  (* Get the time of the state machine.  Uses Bars.timestamp *)
  val time : 'a t -> Time.t res

  (* Get list of active orders *)
  val history : 'a t -> Order.t list Vector.ro_vector

  (* Cost basis to enter the corresponding position *)
  val cost_basis : 'a t -> Instrument.t -> float

  (* Get list of active symbols *)
  val held_symbols : 'a t -> Instrument.t list

  (* Get qty of instrument in current position *)
  val qty : 'a t -> Instrument.t -> int

  (* Get the data of the state for this symbol using Bars.get. *)
  val data : 'a t -> Instrument.t -> (Bars.Data.t, Error.t) result

  (* Get the bars *)
  val bars : 'a t -> Bars.t

  (* Get the value of the state's portfolio, plus the cash on hand *)
  val value : 'a t -> float res

  (* The same state but with with x.state set to Listening *)
  val listen : 'a t -> 'a t

  (* The same state but with with x.state set to Listening *)
  val liquidate : 'a t -> 'a t

  (* Place an order.  Assume that orders are filled completely and instantly at the current price. *)
  val place_order : 'a t -> Order.t -> 'a t res

  (* Current tick of the state machine (corresponds to the Bars.t index) *)
  val tick : 'a t -> int
  val increment_tick : 'a t -> 'a t
  val set : 'a t -> Mode.t -> 'a t

  (* Return the options record *)
  val options : 'a t -> State_config.t
  val empty : unit -> unit t
end

module V3_impl : S = struct
  type 'a t = {
    current_state : Mode.t;
    bars : Bars.t;
    current_tick : int;
    content : 'a;
    config : State_config.t;
    cash : float;
    history : Order.t list Vector.vector;
    positions : Positions.t;
  }
  [@@warning "-69"]

  let empty () : unit t =
    {
      current_state = Initialize;
      bars = Bars.empty ();
      current_tick = 0;
      config = { placeholder = false };
      cash = 0.0;
      history = Vector.create ();
      positions = PosMap.empty;
      content = ();
    }

  let set x mode = { x with current_state = mode }
  let increment_tick x = { x with current_tick = x.current_tick + 1 }
  let bars x = x.bars
  let cost_basis x = Positions.cost_basis x.positions

  type 'a res = ('a, Error.t) result

  let current t = t.current_state

  let make tick bars content =
    let ( let* ) = Result.( let* ) in
    let* length = Bars.length bars in
    let config = State_config.{ placeholder = true } in
    Result.return
      {
        current_state = Initialize;
        bars;
        current_tick = tick;
        content;
        config;
        cash = 100000.0;
        history = Vector.init length (fun _ -> []);
        positions = PosMap.empty;
      }

  let pp fmt t =
    Format.fprintf fmt "V3State(tick=%d, state=%a, cash=%.2f)" t.current_tick
      Mode.pp t.current_state t.cash

  let show t = Format.asprintf "%a" pp t
  let cash t = t.cash
  let time t = Bars.timestamp t.bars
  let history t = Vector.freeze t.history
  let positions x = x.positions

  (* let symbols x = List.map (fun (x : Order.t) -> x.symbol) @@ positions x *)
  let held_symbols x = Positions.symbols @@ positions x
  let data t instrument = Bars.get t.bars instrument

  let value t =
    let ( let* ) = Result.( let* ) in
    let* portfolio_value =
      Positions.fold (positions t) (Ok 0.0) @@ fun instrument orders acc ->
      let* _ = acc in
      let* data = data t instrument in
      let current_price = Bars.Data.get data Last t.current_tick in
      Result.return
      @@ List.fold_left
           (fun acc (order : Order.t) ->
             let order_value = float_of_int order.qty *. current_price in
             order_value +. acc)
           0.0 orders
    in
    Result.return (t.cash +. portfolio_value)

  let listen t = { t with current_state = Listening }
  let liquidate t = { t with current_state = Liquidate }
  let qty (t : 'a t) instrument = Positions.qty t.positions instrument

  let place_order t (order : Order.t) =
    let ( let* ) = Result.( let* ) in
    let* data = Bars.get t.bars order.symbol in
    let current_price = Bars.Data.get data Last t.current_tick in
    let order_value = float_of_int order.qty *. current_price in
    let tick = t.current_tick in

    let _ = Pmutex.set order.status Order.Status.Filled in
    let positions = t.positions in
    let positions = Positions.update positions order in
    match order.side with
    | Buy ->
      if t.cash >=. order_value then (
        let new_cash = t.cash -. order_value in
        let current_tick_orders = order :: Vector.get t.history tick in
        Vector.set t.history tick current_tick_orders;
        Result.return { t with cash = new_cash; positions })
      else Error.fatal "Insufficient cash for buy order"
    | Sell ->
      let qty_held = qty t order.symbol in
      if qty_held >= order.qty then (
        let new_cash = t.cash +. order_value in
        let current_tick_orders = order :: Vector.get t.history tick in
        Vector.set t.history tick current_tick_orders;
        Result.return { t with cash = new_cash; positions })
      else Error.fatal "Insufficient shares for sell order"

  let tick t = t.current_tick
  let options t = t.config
end
