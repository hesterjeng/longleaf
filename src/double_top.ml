[@@@warning "+32"]

module Item = Bars.Item

module Math = struct
  open Option.Infix

  type critical_point = Min of float | Max of float

  let select ~(ord : 'b Ord.t) ~(get : 'a -> 'b) (l : 'a Iter.t) =
    let+ hd = Iter.head l in
    Iter.fold
      (fun x1 x2 ->
        let c = ord (get x1) (get x2) in
        if c = 1 then x1 else x2)
      hd l

  let max_close (l : Item.t Iter.t) =
    match
      select ~ord:Float.compare ~get:(fun (x : Item.t) -> Item.last x) l
    with
    | Some max -> max
    | None -> invalid_arg "Cannot find maximum of empty list"

  let min_close (l : Item.t Iter.t) =
    match
      select ~ord:Float.compare ~get:(fun (x : Item.t) -> Item.last x) l
    with
    | Some max -> max
    | None -> invalid_arg "Cannot find maximum of empty list"

  (* Create a list of indices of size, for length *)
  (* Creates slices *)
  let cut_into_ranges ~length ~size =
    let rec aux start acc =
      if start >= length then List.rev acc
      else
        let next_start = min (start + size) length in
        let current_size =
          if start + size > length then length - start else size
        in
        aux next_start ((start, current_size) :: acc)
    in
    aux 0 [] |> Iter.of_list

  (* This should have fewer datapoints with larger window size. *)
  let window_map ~window_size ~(choose : 'a Iter.t -> 'a) (l : _ Vector.t) =
    let length = Vector.length l in
    let slices = cut_into_ranges ~length ~size:window_size in
    Iter.map (fun (x, y) -> Vector.slice_iter l x y |> choose) slices

  let find_local_maxima ~window_size (l : _ Vector.t) =
    window_map ~window_size ~choose:max_close l

  let find_local_minima ~window_size l =
    window_map ~window_size ~choose:min_close l

  let most_recent_maxima ~window_size l =
    max_close @@ Iter.rev @@ Iter.take window_size l
end

module Conditions = struct
  module Parameters = struct
    let min_dip = 0.98
    let lower_now_band = 0.99
    let upper_now_band = 1.01
    let stop_loss_multiplier = 1.02
    let profit_multiplier = 0.96
    let max_holding_period = 30
    let window_size = 100
  end

  module P = Parameters

  type t = Pass of Item.t | Fail of string

  (* 1) There must be a point less than 80% of the critical point before the first max *)
  (* 2) There must be a local minimum 80% of the first local max between it and now *)
  (* 3) The current price must be within 5% of that previous maximum *)

  let is_pass = function Pass x -> Some x | _ -> None
  let find_pass (l : t Iter.t) = Iter.find_map is_pass l
  let init l = Iter.map (fun x -> Pass x) l

  let map (f : Item.t -> t) (l : t Iter.t) =
    Iter.map (function Pass x -> f x | Fail s -> Fail s) l

  (* There must have been a rise *)
  let check1 ~price_history (current_max : Item.t) : t =
    Vector.find
      (fun (x : Item.t) ->
        let current_max_last = Item.last current_max in
        let x_last = Item.last x in
        let current_max_time = Item.timestamp current_max in
        let x_time = Item.timestamp x in
        x_last <. P.min_dip *. current_max_last
        && Ptime.compare current_max_time x_time = 1)
      price_history
    |> function
    | Some _ -> Pass current_max
    | None -> Fail "check1"

  (* There must be a sufficient dip *)
  let check2 ~minima ~(most_recent_price : Item.t) (current_max : Item.t) : t =
    Iter.find_pred
      (fun (x : Item.t) ->
        let current_max_last, x_last =
          Pair.map_same Item.last (current_max, x)
        in
        let current_max_time, x_time =
          Pair.map_same Item.timestamp (current_max, x)
        in
        let most_recent_time = Item.timestamp most_recent_price in
        x_last <. P.min_dip *. current_max_last
        && Ptime.compare x_time current_max_time = 1
        && Ptime.compare most_recent_time x_time = 1)
      minima
    |> function
    | Some _ -> Pass current_max
    | None -> Fail "check2"

  (* The current price should be within bands *)
  let check3 ~(most_recent_price : Item.t) (current_max : Item.t) : t =
    let current_price = Item.last most_recent_price in
    let current_max_price = Item.last current_max in
    let lower = P.lower_now_band *. current_max_price in
    let upper = P.upper_now_band *. current_max_price in
    if lower <. current_price && current_price <. upper then Pass current_max
    else Fail "check3"

  (* The current volume should be less than the previous *)
  let check4 ~(most_recent_price : Item.t) (current_max : Item.t) : t =
    let prev_volume = Item.volume current_max in
    let most_recent_volume = Item.volume most_recent_price in
    if prev_volume > most_recent_volume then Pass current_max else Fail "check4"

  (* There should not be a higher peak between the previous max and the current price *)
  let check5 ~price_history ~most_recent_price current_max : t =
    let trigger_time = Item.timestamp current_max in
    let current_price = Item.last most_recent_price in
    let in_bounds (x : Item.t) =
      let peak_time = Item.timestamp x in
      Ptime.compare peak_time trigger_time = 1
    in
    let is_peak (x : Item.t) =
      let peak_price = Item.last x in
      peak_price >. P.upper_now_band *. current_price
    in
    Vector.find (fun i -> in_bounds i && is_peak i) price_history |> function
    | Some _ -> Fail "check5"
    | None -> Pass current_max

  module Cover_reason = struct
    type t =
      | Profited of float
      | HoldingPeriod of float
      | StopLoss of float
      | Hold
    [@@deriving show { with_path = false }]

    let make ~time_held ~current_price ~(shorting_order : Trading_types.Order.t)
        ~price_difference =
      if current_price <. P.profit_multiplier *. shorting_order.price then
        Profited price_difference
      else if time_held > P.max_holding_period then
        HoldingPeriod price_difference
      else if current_price >. P.stop_loss_multiplier *. shorting_order.price
      then StopLoss price_difference
      else Hold
  end
end

module DoubleTop (Backend : Backend.S) : Strategies.S = struct
  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  open Trading_types

  module DT_Status = struct
    type t = Placed of (int * Order.t) | Waiting [@@deriving show]
  end

  type state = DT_Status.t State.t

  let init_state = Backend.init_state DT_Status.Waiting

  module SU = Strategies.Strategy_utils (Backend)
  module P = Conditions.P

  let consider_shorting ~(history : Bars.t) ~(state : state)
      ~(qty : string -> int) symbol : Order.t option =
    let open Option.Infix in
    let* price_history = Bars.get history symbol in
    let most_recent_price = Bars.Latest.get state.latest symbol in
    let+ (previous_maximum : Item.t) =
      (* FIXME:  We look back for candidates in ALL the historical data! *)
      (* There should be a lookback parameter. *)
      let minima =
        Math.find_local_minima ~window_size:P.window_size price_history
      in
      let maxima =
        Math.find_local_maxima ~window_size:P.window_size price_history
      in
      Conditions.init maxima
      |> Conditions.map (Conditions.check1 ~price_history)
      |> Conditions.map (Conditions.check2 ~most_recent_price ~minima)
      |> Conditions.map (Conditions.check3 ~most_recent_price)
      |> Conditions.map (Conditions.check4 ~most_recent_price)
      |> Conditions.map (Conditions.check5 ~price_history ~most_recent_price)
      |> Conditions.find_pass
    in
    let order =
      let side = Side.Sell in
      let tif = TimeInForce.GoodTillCanceled in
      let order_type = OrderType.Market in
      let qty = qty symbol in
      let price = Item.last most_recent_price in
      let timestamp = Item.timestamp most_recent_price in
      let reason =
        Format.asprintf "Attempt Shorting: Caused by previous maximum %a"
          Time.pp
          (Item.timestamp previous_maximum)
      in
      Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp ~reason
        ~profit:None
    in
    (* Eio.traceln "@[%a@]@." Order.pp order; *)
    order

  (* The maximum amount of a share that can be purchased at the current price with *)
  (*  pct of cash available *)
  let qty (state : state) pct symbol =
    let cash_available = Backend.get_cash () in
    match cash_available >=. 0.0 with
    | true ->
        let tenp = cash_available *. pct in
        let current_price = Item.last @@ Bars.Latest.get state.latest symbol in
        let max_amt = tenp /. current_price in
        if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
    | false -> 0

  (* Check if we meet the conditions for placing a short for one of our symbols. *)
  (* If not, return the state unchanged, except we are now listening. *)
  let place_short ~(state : state) =
    let ( let* ) = Result.( let* ) in
    let short_opt =
      consider_shorting ~history:state.bars ~state ~qty:(qty state 0.5)
    in
    let possibilities = List.map short_opt Backend.symbols in
    let choice = Option.choice possibilities in
    let* new_status =
      match choice with
      | None -> Ok state.content
      | Some order ->
          let* () = Backend.place_order state order in
          Result.return @@ DT_Status.Placed (0, order)
    in
    Result.return
    @@ { state with State.current = `Listening; content = new_status }

  let cover_position ~(state : state) time_held (shorting_order : Order.t) =
    let ( let* ) = Result.( let* ) in
    let current_bar = Bars.Latest.get state.latest shorting_order.symbol in
    let current_price = Item.last current_bar in
    let timestamp = Item.timestamp current_bar in
    let price_difference = current_price -. shorting_order.price in
    let cover_reason =
      Conditions.Cover_reason.make ~time_held ~current_price ~shorting_order
        ~price_difference
    in
    match cover_reason with
    | Profited _ | HoldingPeriod _ | StopLoss _ ->
        let profit =
          Float.of_int shorting_order.qty
          *. (shorting_order.price -. current_price)
        in
        let reason =
          Format.asprintf "Covering because of %a. Profit: %f"
            Conditions.Cover_reason.pp cover_reason profit
        in
        (* Eio.traceln "@[Profit from covering: %f@]@." profit; *)
        let* () =
          Backend.place_order state
          @@ Order.make ~symbol:shorting_order.symbol ~side:Side.Buy
               ~tif:shorting_order.tif ~order_type:shorting_order.order_type
               ~qty:shorting_order.qty ~price:current_price ~timestamp ~reason
               ~profit:(Some profit)
        in
        Result.return
        @@ {
             state with
             State.current = `Listening;
             content = DT_Status.Waiting;
           }
    | Hold ->
        (* Eio.traceln "@[Holding...@]@."; *)
        Result.return
        @@ {
             state with
             State.current = `Listening;
             content = DT_Status.Placed (time_held + 1, shorting_order);
           }

  let step (state : state) =
    let current = state.current in
    (* Eio.traceln "@[%a@]@." State.pp_state current; *)
    match current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        (* Eio.traceln "@[%a@]@." DT_Status.pp state.content; *)
        match state.content with
        | Waiting -> place_short ~state
        | Placed (time_held, order) -> cover_position ~state time_held order)

  let run () = SU.run ~init_state step
end
