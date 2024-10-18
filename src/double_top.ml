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
  open Lwt_result.Syntax
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))
  module State = Strategies.State
  module Bar_item = Bars.Bar_item

  let consider_shorting ~history ~now ~qty symbol :
      (Order.t * Bar_item.t) option =
    (* 1) There must be a point less than 80% of the critical point before the first max *)
    (* 2) There must be a local minimum 80% of the first local max between it and now *)
    (* 3) The current price must be within 5% of that previous maximum *)
    let open Option.Infix in
    let price_history = Bars.get history symbol in
    let most_recent_price = Bars.price now symbol in
    let minima = Math.find_local_minima 10 price_history in
    let maxima = Math.find_local_maxima 10 price_history in
    let check1 (current_max : Bar_item.t) =
      let* _ =
        List.find_opt
          (fun (x : Bar_item.t) ->
            x.close <. 0.8 *. current_max.close
            && Ptime.compare current_max.timestamp x.timestamp = 1)
          price_history
      in
      Some current_max
    in
    let check2 (current_max : Bar_item.t) =
      let* _ =
        List.find_opt
          (fun (x : Bar_item.t) ->
            x.close <. 0.8 *. current_max.close
            && Ptime.compare x.timestamp current_max.timestamp = 1
            && Ptime.compare most_recent_price.timestamp x.timestamp = 1)
          minima
      in
      Some current_max
    in
    let check3 (current_max : Bar_item.t) =
      let current_price = most_recent_price.close in
      let lower = 0.95 *. current_max.close in
      let upper = 1.05 *. current_max.close in
      if lower <. current_price && current_price <. upper then Some current_max
      else None
    in
    let candidates =
      List.filter_map check1 maxima
      |> List.filter_map check2 |> List.filter_map check3
    in
    match candidates with
    | [ target_maximum ] ->
        let order : Order.t =
          {
            symbol;
            side = Side.Sell;
            tif = TimeInForce.GoodTillCanceled;
            order_type = OrderType.Market;
            qty;
            price = most_recent_price.close;
          }
        in
        Some (order, target_maximum)
    | _ -> None

  let step (state : 'a State.t) : (('a, 'b) State.status, string) Lwt_result.t =
    let env = state.env in
    (* Format.printf "\r\x1b[2K%s%!" (State.show_state state.current); *)
    (* Unix.sleepf 0.01; *)
    match state.current with
    | `Initialize ->
        Lwt_result.return @@ State.continue { state with current = `Listening }
    | `Listening ->
        let* () =
          Strategies.listen_tick Backend.backtesting Backend.Ticker.tick
        in
        Lwt_result.return @@ State.continue { state with current = `Ordering }
    | `Liquidate ->
        let* _ = Backend.liquidate env in
        Lwt_result.return
        @@ State.continue
             { state with current = `Finished "Successfully liquidated" }
    | `Finished code ->
        Strategies.output_data Backend.backtesting Backend.get_cash state;
        Lwt_result.return @@ State.shutdown code
    | `Ordering ->
        let* latest = Backend.latest_bars env Backend.tickers in
        let cash_available = Backend.get_cash () in
        let qty symbol =
          match cash_available >=. 0.0 with
          | true ->
              let tenp = cash_available *. 0.5 in
              let max_amt = tenp /. (Bars.price latest symbol).close in
              if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
          | false -> 0
        in
        let new_bars = Bars.combine [ latest; state.content ] in
        Lwt_result.return
        @@ State.continue
             { state with current = `Listening; content = new_bars }

  let run = Strategies.run step
end
