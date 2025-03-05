let select_midpoint length =
  assert (length >= 8000);
  let start_range = 1000 in
  let end_range = length - 1000 in
  Int.random_range start_range end_range Util.random_state

let top ~(options : Options.t) bars target =
  Eio.traceln "Selecting and creating random slice...";
  let preload = bars in
  let target =
    target |> Option.get_exn_or "Must have target for slice backtesting"
  in
  let combined = Bars.combine [ preload; target ] in
  let combined_length = Bars.length combined in
  let midpoint = select_midpoint combined_length in
  let new_bars, new_target =
    Bars.split ~midpoint ~target_length:options.randomized_backtest_length
      ~combined_length combined
  in
  Eio.traceln "Sorting random slices...";
  Bars.sort Item.compare new_bars;
  Bars.sort (Ord.opp Item.compare) new_target;
  Eio.traceln "Finished creating random slice...";
  (new_bars, new_target)
