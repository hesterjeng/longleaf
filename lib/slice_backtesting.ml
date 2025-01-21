let select_midpoint length =
  assert (length >= 8000);
  let start_range = 1000 in
  let end_range = length - 1000 in
  Int.random_range start_range end_range Util.random_state

let top target_length (module Input : Backend.BACKEND_INPUT) =
  let preload = Input.bars in
  let target =
    Input.target |> Option.get_exn_or "Must have target for slice backtesting"
  in
  let combined = Bars.combine [ preload; target ] in
  let combined_length = Bars.length combined in
  let midpoint = select_midpoint combined_length in
  let new_bars, new_target =
    Bars.split ~midpoint ~target_length ~combined_length combined
  in
  (module struct
    include Input

    let bars = new_bars
    let target = Some new_target
  end : Backend.BACKEND_INPUT)
