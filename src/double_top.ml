module Math = struct
  module Bar_item = Bars.Bar_item
  open Option.Infix

  type critical_point = Min of float | Max of float

  let select ~(ord : 'b Ord.t) ~(get : 'a -> 'b) (l : 'a list) =
    let+ hd = List.head_opt l in
    List.fold_left
      (fun x1 x2 ->
        let c = ord (get x1) (get x2) in
        if c = 1 then x1 else x2)
      hd l

  let max_close (l : Bar_item.t list) =
    match
      select ~ord:Float.compare ~get:(fun (x : Bar_item.t) -> Bar_item.last x) l
    with
    | Some max -> max
    | None -> invalid_arg "Cannot find maximum of empty list"

  let min_close (l : Bar_item.t list) =
    match
      select ~ord:Float.compare ~get:(fun (x : Bar_item.t) -> Bar_item.last x) l
    with
    | Some max -> max
    | None -> invalid_arg "Cannot find maximum of empty list"

  let window_map ~window_size ~(choose : 'a list -> 'a) (l : _ Vector.t) =
    let l = Vector.to_list l in
    let rec aux acc (l : _ list) =
      match List.length l with
      | 0 -> List.rev acc
      | _ ->
          let window, rest = List.take_drop window_size l in
          aux (choose window :: acc) rest
    in
    aux l []

  let find_local_maxima ~window_size (l : _ Vector.t) =
    window_map ~window_size ~choose:max_close l

  let find_local_minima ~window_size l =
    window_map ~window_size ~choose:min_close l

  let most_recent_maxima ~window_size l =
    max_close @@ List.rev @@ List.take window_size l
end

module DoubleTop (Backend : Backend.S) : Strategies.S = struct
  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  open Trading_types
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))
  module Bar_item = Bars.Bar_item

  module DT_Status = struct
    type t = Placed of (int * Order.t) | Waiting
  end

  type state = DT_Status.t State.t

  (* let init_state = *)
  (*   { *)
  (*     State.current = `Initialize; *)
  (*     bars = Backend.loaded_bars; *)
  (*     latest_bars = Bars.empty; *)
  (*     content = DT_Status.Waiting; *)
  (*     order_history = Vector.create (); *)
  (*   } *)

  let init_state = Backend.init_state DT_Status.Waiting

  module SU = Strategies.Strategy_utils (Backend)

  let min_dip = 0.99
  let lower_now_band = 0.999
  let upper_now_band = 1.001
  let stop_loss_multiplier = 1.02
  let profit_multiplier = 1.04
  let max_holding_period = 24

  module Conditions = struct
    type t = Pass of Bar_item.t | Fail of string

    (* 1) There must be a point less than 80% of the critical point before the first max *)
    (* 2) There must be a local minimum 80% of the first local max between it and now *)
    (* 3) The current price must be within 5% of that previous maximum *)

    let is_pass = function Pass x -> Some x | _ -> None
    let find_pass (l : t list) = List.find_map is_pass l
    let init l = List.map (fun x -> Pass x) l

    let map (f : Bar_item.t -> t) (l : t list) =
      List.map (function Pass x -> f x | fail -> fail) l

    let check1 ~price_history (current_max : Bar_item.t) : t =
      Vector.find
        (fun (x : Bar_item.t) ->
          let current_max_last, x_last =
            Pair.map_same Bar_item.last (current_max, x)
          in
          let current_max_time, x_time =
            Pair.map_same Bar_item.timestamp (current_max, x)
          in
          x_last <. min_dip *. current_max_last
          && Ptime.compare current_max_time x_time = 1)
        price_history
      |> function
      | Some _ -> Pass current_max
      | None -> Fail "check1"

    let check2 ~minima ~(most_recent_price : Bar_item.t)
        (current_max : Bar_item.t) : t =
      List.find_opt
        (fun (x : Bar_item.t) ->
          let current_max_last, x_last =
            Pair.map_same Bar_item.last (current_max, x)
          in
          let current_max_time, x_time =
            Pair.map_same Bar_item.timestamp (current_max, x)
          in
          let most_recent_time = Bar_item.timestamp most_recent_price in
          x_last <. min_dip *. current_max_last
          && Ptime.compare x_time current_max_time = 1
          && Ptime.compare most_recent_time x_time = 1)
        minima
      |> function
      | Some _ -> Pass current_max
      | None -> Fail "check2"

    let check3 ~(most_recent_price : Bar_item.t) (current_max : Bar_item.t) : t
        =
      let current_price = Bar_item.last most_recent_price in
      let current_max_price = Bar_item.last current_max in
      let lower = lower_now_band *. current_max_price in
      let upper = upper_now_band *. current_max_price in
      if lower <. current_price && current_price <. upper then Pass current_max
      else Fail "check3"

    let check4 ~(most_recent_price : Bar_item.t) (current_max : Bar_item.t) : t
        =
      let prev_volume = Bar_item.volume current_max in
      let most_recent_volume = Bar_item.volume most_recent_price in
      if prev_volume > most_recent_volume then Pass current_max
      else Fail "check4"
  end

  let consider_shorting ~(history : Bars.t) ~now ~(qty : string -> int) symbol :
      (Order.t * Bar_item.t) option =
    let open Option.Infix in
    let* price_history = Bars.get history symbol in
    let most_recent_price = Bars.price now symbol in
    let minima = Math.find_local_minima ~window_size:100 price_history in
    let maxima = Math.find_local_maxima ~window_size:100 price_history in
    Eio.traceln "@[Found %d minima and %d maxima@]@." (List.length minima)
      (List.length maxima);
    let selected =
      Conditions.init maxima
      |> Conditions.map (Conditions.check1 ~price_history)
      |> Conditions.map (Conditions.check2 ~most_recent_price ~minima)
      |> Conditions.map (Conditions.check3 ~most_recent_price)
      |> Conditions.map (Conditions.check4 ~most_recent_price)
      |> Conditions.find_pass
    in
    match selected with
    | Some previous_maximum ->
        let order : Order.t =
          let side = Side.Sell in
          let tif = TimeInForce.GoodTillCanceled in
          let order_type = OrderType.Market in
          let qty = qty symbol in
          let price = Bar_item.last most_recent_price in
          let timestamp = Bar_item.timestamp most_recent_price in
          Order.make ~symbol ~side ~tif ~order_type ~qty ~price ~timestamp
        in
        Some (order, previous_maximum)
    | _ -> None

  let place_short ~(state : state) =
    let now =
      Bar_item.timestamp
      @@ Bars.price state.latest_bars (List.hd Backend.symbols)
    in
    let cash_available = Backend.get_cash () in
    let qty symbol =
      match cash_available >=. 0.0 with
      | true ->
          let tenp = cash_available *. 0.5 in
          let current_price =
            Bar_item.last @@ Bars.price state.latest_bars symbol
          in
          let max_amt = tenp /. current_price in
          if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
      | false -> 0
    in
    let short_opt =
      consider_shorting ~history:state.bars ~now:state.latest_bars ~qty
    in
    let possibilities = List.map short_opt Backend.symbols in
    let choice = Option.choice possibilities in
    let new_status =
      match choice with
      | None -> state.content
      | Some (order, trigger) ->
          Eio.traceln "@[Short triggered by previous local max at %a@]@."
            Time.pp
            (Bar_item.timestamp trigger);
          Eio.traceln "@[%a@]@.@[%a@]@." Time.pp now Order.pp order;
          Backend.place_order state order;
          (* let stop_loss : Order.t = *)
          (*   { *)
          (*     order with *)
          (*     side = Buy; *)
          (*     tif = TimeInForce.GoodTillCanceled; *)
          (*     order_type = OrderType.StopLimit; *)
          (*     price = 1.03 *. order.price; *)
          (*   } *)
          (* in *)
          (* let _ = Backend.create_order stop_loss in *)
          Placed (0, order)
    in
    Result.return
    @@ { state with State.current = `Listening; content = new_status }

  type cover_reason = Profited | HoldingPeriod | StopLoss | None
  [@@deriving show { with_path = false }]

  let cover_position ~(state : state) time_held (order : Order.t) =
    let now =
      Bar_item.timestamp
      @@ Bars.price state.latest_bars (List.hd Backend.symbols)
    in
    let cover_order =
      let current_bar = Bars.price state.latest_bars order.symbol in
      let current_price = Bar_item.last current_bar in
      let timestamp = Bar_item.timestamp current_bar in
      let cover_order =
        Order.make ~symbol:order.symbol ~side:Side.Buy ~tif:order.tif
          ~order_type:order.order_type ~qty:order.qty ~price:current_price
          ~timestamp
      in
      let target_price = min_dip *. order.price in
      let cover_reason =
        if current_price <. profit_multiplier *. target_price then Profited
        else if time_held > max_holding_period then HoldingPeriod
        else if current_price >. stop_loss_multiplier *. order.price then
          StopLoss
        else None
      in
      match cover_reason with
      | Profited | HoldingPeriod | StopLoss ->
          Eio.traceln "Covering because of %a" pp_cover_reason cover_reason;
          Some cover_order
      | None -> None
    in
    let new_status =
      match cover_order with
      | None -> DT_Status.Placed (time_held + 1, order)
      | Some order ->
          Eio.traceln "@[%a@]@.@[%a@]@." Time.pp now Order.pp order;
          Backend.place_order state order;
          Waiting
    in
    Result.return
    @@ { state with State.current = `Listening; content = new_status }

  let step (state : state) =
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        match state.content with
        | Waiting -> place_short ~state
        | Placed (time_held, order) -> cover_position ~state time_held order)

  let run () = SU.run ~init_state step
end
