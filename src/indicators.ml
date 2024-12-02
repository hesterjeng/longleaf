open Bars

let accumulation_distribution_line (l : Item.t list) =
  Eio.traceln
    "@[accumulation_distribution_line was written with Alpaca bar items in \
     mind, not tiingo.@]@.";
  let _, res =
    List.fold_map
      (fun previous_adl (x : Item.t) ->
        let open Float in
        let open Item in
        let close = close x in
        let low = low x in
        let high = high x in
        let volume = volume x in
        let money_flow_multiplier =
          (close - low - (high - close)) / (high - low)
        in
        let money_flow_volume = money_flow_multiplier * Float.of_int volume in
        let adl = previous_adl + money_flow_volume in
        (adl, adl))
      0.0 l
  in
  res

let simple_moving_average (l : Item.t list) =
  Eio.traceln
    "@[simple_moving_average was written with Alpaca bar items in mind, not \
     tiingo.@]@.";
  let n = List.length l in
  let close = List.map (fun (x : Item.t) -> Item.close x) l in
  let sma_i i =
    let start = Int.max (i - n) 0 in
    let range = List.range start i in
    let vals =
      List.map
        (fun i ->
          match List.get_at_idx i close with
          | Some p -> p
          | None ->
              invalid_arg @@ Format.sprintf "Unable to get price at index %d" i)
        range
    in
    List.fold_left (fun x y -> x +. y) 0.0 vals /. Float.of_int i
  in
  List.mapi (fun i _ -> sma_i i) close
