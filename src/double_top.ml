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
      select ~ord:Float.compare ~get:(fun (x : Bar_item.t) -> x.close) l
    with
    | Some max -> max
    | None -> invalid_arg "Cannot find maximum of empty list"

  let min_close (l : Bar_item.t list) =
    match
      select ~ord:Float.compare ~get:(fun (x : Bar_item.t) -> x.close) l
    with
    | Some max -> max
    | None -> invalid_arg "Cannot find maximum of empty list"

  let window_map ~window_size ~(choose : 'a list -> 'a) l =
    let rec aux acc l =
      match l with
      | [] -> List.rev acc
      | l ->
          let window, rest = List.take_drop window_size l in
          aux (choose window :: acc) rest
    in
    aux l []

  let find_local_maxima window_size l =
    window_map ~window_size ~choose:max_close l

  let find_local_minima window_size l =
    window_map ~window_size ~choose:max_close l

  let most_recent_maxima window_size l =
    max_close @@ List.rev @@ List.take window_size l
end

module DoubleTop (Backend : Backend.S) : Strategies.S = struct
  let shutdown () =
    Eio.traceln "Shutdown command NYI";
    ()

  open Trading_types
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))
  module State = Strategies.State
  module Bar_item = Bars.Bar_item

  module DT_Status = struct
    type t = Placed of (int * Order.t) | Waiting
  end

  type state = DT_Status.t State.t

  let init_state =
    {
      State.current = `Initialize;
      bars = Bars.empty;
      content = DT_Status.Waiting;
    }

  module SU = Strategies.Strategy_utils (Backend)

  let min_dip = 0.99
  let lower_now_band = 0.999
  let upper_now_band = 1.001
  let stop_loss_multiplier = 1.01
  let profit_multiplier = 1.0
  let max_holding_period = 24

  module Conditions = struct
    open Option.Infix

    let check1 ~price_history (current_max : Bar_item.t) =
      let* _ =
        List.find_opt
          (fun (x : Bar_item.t) ->
            x.close <. min_dip *. current_max.close
            && Ptime.compare current_max.timestamp x.timestamp = 1)
          price_history
      in
      Some current_max

    let check2 ~minima ~(most_recent_price : Bar_item.t)
        (current_max : Bar_item.t) =
      let* _ =
        List.find_opt
          (fun (x : Bar_item.t) ->
            x.close <. min_dip *. current_max.close
            && Ptime.compare x.timestamp current_max.timestamp = 1
            && Ptime.compare most_recent_price.timestamp x.timestamp = 1)
          minima
      in
      Some current_max

    let check3 ~(most_recent_price : Bar_item.t) (current_max : Bar_item.t) =
      let current_price = most_recent_price.close in
      let lower = lower_now_band *. current_max.close in
      let upper = upper_now_band *. current_max.close in
      if lower <. current_price && current_price <. upper then Some current_max
      else None

    let check4 ~(most_recent_price : Bar_item.t) (current_max : Bar_item.t) =
      let prev_volume = current_max.volume in
      let most_recent_volume = most_recent_price.volume in
      if prev_volume > most_recent_volume then Some current_max else None
  end

  let consider_shorting ~history ~now ~(qty : string -> int) symbol :
      (Order.t * Bar_item.t) option =
    (* 1) There must be a point less than 80% of the critical point before the first max *)
    (* 2) There must be a local minimum 80% of the first local max between it and now *)
    (* 3) The current price must be within 5% of that previous maximum *)
    let open Option.Infix in
    let* price_history = Bars.get history symbol in
    let most_recent_price = Bars.price now symbol in
    let minima = Math.find_local_minima 10 price_history in
    let maxima = Math.find_local_maxima 10 price_history in
    let candidates =
      List.filter_map
        (fun current_max -> Conditions.check1 ~price_history current_max)
        maxima
      |> List.filter_map (fun can ->
             Conditions.check2 ~most_recent_price ~minima can)
      |> List.filter_map (fun can -> Conditions.check3 ~most_recent_price can)
      |> List.filter_map (fun can -> Conditions.check4 ~most_recent_price can)
    in
    let seed = Unix.time () |> fun t -> [| Int.of_float t |] in
    let st = Random.State.make seed (* Random.State.make_self_init () in *) in
    let rand =
      try Option.return @@ List.random_choose candidates st with _ -> None
    in
    match rand with
    | Some previous_maximum ->
        let order : Order.t =
          {
            symbol;
            side = Side.Sell;
            tif = TimeInForce.GoodTillCanceled;
            order_type = OrderType.Market;
            qty = qty symbol;
            price = most_recent_price.close;
          }
        in
        Some (order, previous_maximum)
    | _ -> None

  let place_short ~latest_bars (state : state) =
    let now = (Bars.price latest_bars (List.hd Backend.symbols)).timestamp in
    let cash_available = Backend.get_cash () in
    let qty symbol =
      match cash_available >=. 0.0 with
      | true ->
          let tenp = cash_available *. 0.5 in
          let max_amt = tenp /. (Bars.price latest_bars symbol).close in
          if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
      | false -> 0
    in
    let short_opt =
      consider_shorting ~history:state.bars.bars ~now:latest_bars ~qty
    in
    let possibilities = List.map short_opt Backend.symbols in
    let choice = Option.choice possibilities in
    let new_status =
      match choice with
      | None -> state.content
      | Some (order, trigger) ->
          Log.app (fun k ->
              k "@[Short triggered by previous local max at %a@]@." Time.pp
                trigger.timestamp);
          Log.app (fun k -> k "@[%a@]@.@[%a@]@." Time.pp now Order.pp order);
          let _ = Backend.create_order order in
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
    let new_bars = Bars.combine [ latest_bars; state.bars ] in
    Result.return
    @@ { State.current = `Listening; bars = new_bars; content = new_status }

  type cover_reason = Profited | HoldingPeriod | StopLoss | None
  [@@deriving show { with_path = false }]

  let cover_position bars ~latest_bars time_held (order : Order.t) =
    let now = (Bars.price latest_bars (List.hd Backend.symbols)).timestamp in
    let cover_order =
      let current_price = (Bars.price latest_bars order.symbol).close in
      let cover_order = { order with side = Side.Buy; price = current_price } in
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
          let _ = Backend.create_order order in
          Waiting
    in
    let new_bars = Bars.combine [ latest_bars; bars ] in
    Result.return
    @@ { State.current = `Listening; bars = new_bars; content = new_status }

  let step (state : state) =
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        let open Result.Infix in
        let* latest_bars = Backend.latest_bars Backend.symbols in
        match state.content with
        | Waiting -> place_short ~latest_bars state
        | Placed (time_held, order) ->
            cover_position ~latest_bars state.bars time_held order)

  let run () = SU.run ~init_state step
end
