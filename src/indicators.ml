open Trading_types.Bars

let accumulation_distribution_line (l : Bar_item.t list) =
  let _, res =
    List.fold_map
      (fun previous_adl (x : Bar_item.t) ->
        let open Float in
        let money_flow_multiplier =
          (x.close - x.low - (x.high - x.close)) / (x.high - x.low)
        in
        let money_flow_volume = money_flow_multiplier * Float.of_int x.volume in
        let adl = previous_adl + money_flow_volume in
        (adl, adl))
      0.0 l
  in
  res

let simple_moving_average (l : Bar_item.t list) =
  let n = List.length l in
  let close = List.map (fun (x : Bar_item.t) -> x.close) l in
  let sma_i i =
    let start = Int.max (i - n) 0 in
    let range = List.range start i in
    let vals =
      List.map
        (fun i ->
          match List.get_at_idx i close with
          | Some p -> p
          | None -> invalid_arg "Unable to get price at index %d" i)
        range
    in
    List.fold_left (fun x y -> x +. y) 0.0 vals /. Float.of_int i
  in
  List.mapi (fun i _ -> sma_i i) close
