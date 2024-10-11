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
