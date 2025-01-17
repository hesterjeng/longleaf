module Gaussian = struct
  type t = { mean : float; std : float (* standard deviation *) }

  let of_array (x : float array) =
    let mean = Owl_stats.mean x in
    let std = Owl_stats.std x in
    { mean; std }

  (* Create a Gaussian PDF of the differences of floats in the array *)
  let of_preload (preload : float array) =
    let differences =
      let rhs = Array.append [| 0.0 |] preload in
      let lhs = Array.append preload [| 0.0 |] in
      Array.map2 Float.( - ) rhs lhs
    in
    let len = Array.length differences in
    assert (len >= 4);
    let range = Array.(1 -- (len - 2)) in
    let slice = Array.map (Array.get differences) range in
    of_array slice

  (* sample *)
  let rv (x : t) = Owl_stats.gaussian_rvs ~mu:x.mean ~sigma:x.std

  let next_data_point ~(dist : t) previous =
    let diff = rv dist in
    previous +. diff

  (* Generate an array of floats based on the movements from the input array *)
  let monte_carlo (x : float array) =
    let dist = of_preload x in
    let init = Array.get x 0 in
    let rec make len previous acc =
      assert (len >= 0);
      match len with
      | 0 -> acc
      | i ->
          let new_value = next_data_point ~dist previous in
          make (i - 1) new_value (new_value :: acc)
    in
    let res = make (Array.length x) init [] |> Array.of_list in
    Array.reverse_in_place res;
    res
end

module Item = struct
  let of_item_array (x : Item.t array) (target : Item.t array) =
    let mc = fun x -> Gaussian.monte_carlo x in
    let target_times = Array.map Item.timestamp target in
    let open_arr = Array.map Item.open_ x |> mc in
    let high_arr = Array.map Item.high x |> mc in
    let low_arr = Array.map Item.low x |> mc in
    let close_arr = Array.map Item.close x |> mc in
    let last_arr = Array.map Item.last x |> mc in
    let volume_arr =
      Array.map Item.volume x |> Array.map Float.of_int |> mc
      |> Array.map Float.to_int
    in
    Array.mapi
      (fun i timestamp ->
        let get = Array.get in
        Item.make ~timestamp ~open_:(get open_arr i) ~high:(get high_arr i)
          ~low:(get low_arr i) ~close:(get close_arr i) ~last:(get last_arr i)
          ~volume:(get volume_arr i) ())
      target_times

  let of_item_vector ~preload ~target =
    of_item_array (Vector.to_array preload) (Vector.to_array target)
    |> Vector.of_array
end