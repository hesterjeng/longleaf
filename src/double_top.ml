module Math = struct
  module Bar_item = Trading_types.Bars.Bar_item
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
  open Trading_types
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))
  module State = Strategies.State
  module Bar_item = Bars.Bar_item

  type short_placed = Placed of Order.t | Waiting
  type short_status = short_placed ref

  module SU = Strategies.Strategy_utils (Backend)

  let current_status : short_status = ref Waiting

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
    let check1 (current_max : Bar_item.t) =
      let* _ =
        List.find_opt
          (fun (x : Bar_item.t) ->
            x.close <. 0.98 *. current_max.close
            && Ptime.compare current_max.timestamp x.timestamp = 1)
          price_history
      in
      Some current_max
    in
    let check2 (current_max : Bar_item.t) =
      let* _ =
        List.find_opt
          (fun (x : Bar_item.t) ->
            x.close <. 0.98 *. current_max.close
            && Ptime.compare x.timestamp current_max.timestamp = 1
            && Ptime.compare most_recent_price.timestamp x.timestamp = 1)
          minima
      in
      Some current_max
    in
    let check3 (current_max : Bar_item.t) =
      let current_price = most_recent_price.close in
      let lower = 0.99 *. current_max.close in
      let upper = 1.01 *. current_max.close in
      if lower <. current_price && current_price <. upper then Some current_max
      else None
    in
    let candidates =
      List.filter_map check1 maxima
      |> List.filter_map check2 |> List.filter_map check3
    in
    match candidates with
    | [ previous_maximum ] ->
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

  let place_short env (state : Bars.t State.t) =
    let open Result.Infix in
    let* latest = Backend.latest_bars env Backend.symbols in
    let now = (Bars.price latest (List.hd Backend.symbols)).timestamp in
    let cash_available = Backend.get_cash () in
    let qty symbol =
      match cash_available >=. 0.0 with
      | true ->
          let tenp = cash_available *. 0.5 in
          let max_amt = tenp /. (Bars.price latest symbol).close in
          if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
      | false -> 0
    in
    let short_opt =
      consider_shorting ~history:state.content.bars ~now:latest ~qty
    in
    let possibilities = List.map short_opt Backend.symbols in
    let choice = Option.choice possibilities in
    let () =
      match choice with
      | None -> ()
      | Some (order, trigger) ->
          Log.app (fun k ->
              k "@[Short triggered by previous local max at %a@]@." Time.pp
                trigger.timestamp);
          Log.app (fun k -> k "@[%a@]@.@[%a@]@." Time.pp now Order.pp order);
          current_status := Placed order;
          let _ = Backend.create_order env order in
          ()
    in
    let new_bars = Bars.combine [ latest; state.content ] in
    Result.return
    @@ State.continue { state with current = `Listening; content = new_bars }

  let cover_position env (state : Bars.t State.t) (order : Order.t) =
    let open Result.Infix in
    let* latest = Backend.latest_bars env Backend.symbols in
    let now = (Bars.price latest (List.hd Backend.symbols)).timestamp in
    let cover_order =
      let current_price = (Bars.price latest order.symbol).close in
      let target_price = 0.98 *. order.price in
      if current_price <. target_price then
        let cover_order =
          { order with side = Side.Buy; price = current_price }
        in
        Some cover_order
      else None
    in
    let () =
      match cover_order with
      | None -> ()
      | Some order ->
          (* Log.app (fun k -> k "@[%a@]@.@[%a@]@." Time.pp now Order.pp order); *)
          current_status := Waiting;
          let _ = Backend.create_order env order in
          ()
    in
    let new_bars = Bars.combine [ latest; state.content ] in
    Result.return
    @@ State.continue { state with current = `Listening; content = new_bars }

  let step (state : 'a State.t) =
    Log.app (fun k -> k "@[%a@]@." State.pp_state state.current);
    let env = state.env in
    (* Format.printf ".%a" Format.flush (); *)
    match state.current with
    | #State.nonlogical_state as current ->
        SU.handle_nonlogical_state current state
    | `Ordering -> (
        match !current_status with
        | Waiting -> place_short env state
        | Placed order -> cover_position env state order)

  let run = SU.run step
end
