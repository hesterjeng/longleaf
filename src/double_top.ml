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

module DoubleTop (Backend : Backend.S) : Strategies.S = struct
  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  open Trading_types
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))

  module DT_Status = struct
    type t = Placed of (int * Order.t) | Waiting [@@deriving show]
  end

  type state = DT_Status.t State.t

  let init_state = Backend.init_state DT_Status.Waiting

  module SU = Strategies.Strategy_utils (Backend)

  module Old_params = struct
    (* Profits, but for bad reasons *)
    let min_dip = 0.99
    let lower_now_band = 0.999
    let upper_now_band = 1.001
    let stop_loss_multiplier = 1.02
    let profit_multiplier = 1.04
    let max_holding_period = 24
  end

  (* Profitable? *)
  module Parameters1 = struct
    let min_dip = 0.98
    let lower_now_band = 0.99
    let upper_now_band = 1.01
    let stop_loss_multiplier = 1.02

    (* This should be less than one to indicate *)
    (* that the short was successful, but seems to provide profits at > 1? *)
    (* FIXME: Further investigate why this is profitable??? *)
    (* Maybe it makes us exit shorts that are bad, or there is a problem with *)
    (* how backtesting? *)
    let profit_multiplier = 1.04
    let max_holding_period = 180
  end

  module Parameters = struct
    let min_dip = 0.98
    let lower_now_band = 0.99
    let upper_now_band = 1.01
    let stop_loss_multiplier = 1.02
    let profit_multiplier = 0.96
    let max_holding_period = 30
    let window_size = 100
  end

  open Parameters

  module Conditions = struct
    type t = Pass of Item.t | Fail of string

    (* 1) There must be a point less than 80% of the critical point before the first max *)
    (* 2) There must be a local minimum 80% of the first local max between it and now *)
    (* 3) The current price must be within 5% of that previous maximum *)

    let is_pass = function Pass x -> Some x | _ -> None
    let find_pass (l : t Iter.t) = Iter.find_map is_pass l
    let init l = Iter.map (fun x -> Pass x) l

    let map (f : Item.t -> t) (l : t Iter.t) =
      Iter.map (function Pass x -> f x | fail -> fail) l

    let check1 ~price_history (current_max : Item.t) : t =
      Vector.find
        (fun (x : Item.t) ->
          let current_max_last = Item.last current_max in
          let x_last = Item.last x in
          let current_max_time = Item.timestamp current_max in
          let x_time = Item.timestamp x in
          x_last <. min_dip *. current_max_last
          && Ptime.compare current_max_time x_time = 1)
        price_history
      |> function
      | Some _ -> Pass current_max
      | None -> Fail "check1"

    let check2 ~minima ~(most_recent_price : Item.t) (current_max : Item.t) : t
        =
      (* List.find_opt *)
      Iter.find_pred
        (fun (x : Item.t) ->
          let current_max_last, x_last =
            Pair.map_same Item.last (current_max, x)
          in
          let current_max_time, x_time =
            Pair.map_same Item.timestamp (current_max, x)
          in
          let most_recent_time = Item.timestamp most_recent_price in
          x_last <. min_dip *. current_max_last
          && Ptime.compare x_time current_max_time = 1
          && Ptime.compare most_recent_time x_time = 1)
        minima
      |> function
      | Some _ -> Pass current_max
      | None -> Fail "check2"

    let check3 ~(most_recent_price : Item.t) (current_max : Item.t) : t =
      let current_price = Item.last most_recent_price in
      let current_max_price = Item.last current_max in
      let lower = lower_now_band *. current_max_price in
      let upper = upper_now_band *. current_max_price in
      if lower <. current_price && current_price <. upper then Pass current_max
      else Fail "check3"

    let check4 ~(most_recent_price : Item.t) (current_max : Item.t) : t =
      let prev_volume = Item.volume current_max in
      let most_recent_volume = Item.volume most_recent_price in
      if prev_volume > most_recent_volume then Pass current_max
      else Fail "check4"
  end

  let consider_shorting ~(history : Bars.t) ~(state : state)
      ~(qty : string -> int) symbol : Order.t option =
    let open Option.Infix in
    let price_history = Bars.get history symbol in
    let most_recent_price = Bars.Latest.get state.latest symbol in
    let+ (previous_maximum : Item.t) =
      (* FIXME:  We look back for candidates in ALL the historical data! *)
      (* There should be a lookback parameter. *)
      let minima = Math.find_local_minima ~window_size price_history in
      let maxima = Math.find_local_maxima ~window_size price_history in
      let init = Conditions.init maxima in
      let c1 = Conditions.map (Conditions.check1 ~price_history) init in
      let c2 =
        Conditions.map (Conditions.check2 ~most_recent_price ~minima) c1
      in
      let c3 = Conditions.map (Conditions.check3 ~most_recent_price) c2 in
      let c4 = Conditions.map (Conditions.check4 ~most_recent_price) c3 in
      Conditions.find_pass c4
    in
    let order =
      let prev_max_timestamp = Item.timestamp previous_maximum in
      let side = Side.Sell in
      let tif = TimeInForce.GoodTillCanceled in
      let order_type = OrderType.Market in
      let qty = qty symbol in
      let price = Item.last most_recent_price in
      let timestamp = Item.timestamp most_recent_price in
      Eio.traceln "@[Short triggered by previous local max at %a@]@." Time.pp
        prev_max_timestamp;
      let reason =
        Format.asprintf "Attempt Shorting: Caused by previous maximum %a"
          Time.pp
          (Item.timestamp previous_maximum)
      in
      Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp ~reason
        ~profit:None
    in
    Eio.traceln "@[%a@]@." Order.pp order;
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
    let short_opt =
      consider_shorting ~history:state.bars ~state ~qty:(qty state 0.5)
    in
    let possibilities = List.map short_opt Backend.symbols in
    let choice = Option.choice possibilities in
    let new_status =
      match choice with
      | None -> state.content
      | Some order ->
          Backend.place_order state order;
          Placed (0, order)
    in
    Result.return
    @@ { state with State.current = `Listening; content = new_status }

  type cover_reason =
    | Profited of float
    | HoldingPeriod of float
    | StopLoss of float
    | Hold
  [@@deriving show { with_path = false }]

  let cover_position ~(state : state) time_held (shorting_order : Order.t) =
    let current_bar = Bars.Latest.get state.latest shorting_order.symbol in
    let current_price = Item.last current_bar in
    let timestamp = Item.timestamp current_bar in
    let price_difference = current_price -. shorting_order.price in
    let cover_reason =
      if current_price <. profit_multiplier *. shorting_order.price then
        Profited price_difference
      else if time_held > max_holding_period then HoldingPeriod price_difference
      else if current_price >. stop_loss_multiplier *. shorting_order.price then
        StopLoss price_difference
      else Hold
    in
    match cover_reason with
    | Profited _ | HoldingPeriod _ | StopLoss _ ->
        let profit =
          Float.of_int shorting_order.qty
          *. (shorting_order.price -. current_price)
        in
        let reason =
          Format.asprintf "Covering because of %a. Profit: %f" pp_cover_reason
            cover_reason profit
        in
        Eio.traceln "@[Profit from covering: %f@]@." profit;
        Backend.place_order state
        @@ Order.make ~symbol:shorting_order.symbol ~side:Side.Buy
             ~tif:shorting_order.tif ~order_type:shorting_order.order_type
             ~qty:shorting_order.qty ~price:current_price ~timestamp ~reason
             ~profit:(Some profit);
        Result.return
        @@ {
             state with
             State.current = `Listening;
             content = DT_Status.Waiting;
           }
    | Hold ->
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
