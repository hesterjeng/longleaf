open Option.Infix

let select ~(ord : 'b Ord.t) ~(get : 'a -> 'b) (l : 'a Iter.t) =
  let+ hd = Iter.head l in
  Iter.fold
    (fun x1 x2 ->
      let c = ord (get x1) (get x2) in
      if c = 1 then x1 else x2)
    hd l

let max_close (l : Item.t Iter.t) =
  match select ~ord:Float.compare ~get:(fun (x : Item.t) -> Item.last x) l with
  | Some max -> max
  | None -> invalid_arg "Cannot find maximum of empty list"

let min_close (l : Item.t Iter.t) =
  match
    select ~ord:(Ord.opp Float.compare) ~get:(fun (x : Item.t) -> Item.last x) l
  with
  | Some max -> max
  | None -> invalid_arg "Cannot find maximum of empty list"

(* Create a list of indices of size, for length *)
(* Creates slices *)
let cut_into_ranges ~length ~size =
  let rec aux start acc =
    if start >= length then List.rev acc
    else
      let next_start = min (start + size) length in
      let current_size =
        if start + size > length then length - start else size
      in
      aux next_start ((start, current_size) :: acc)
  in
  aux 0 [] |> Iter.of_list

(* This should have fewer datapoints with larger window size. *)
let window_map ~window_size ~(choose : 'a Iter.t -> 'a) (l : _ Vector.t) =
  let length = Vector.length l in
  let slices = cut_into_ranges ~length ~size:window_size in
  Iter.map (fun (x, y) -> Vector.slice_iter l x y |> choose) slices

let find_local_maxima ~window_size (l : _ Vector.t) =
  window_map ~window_size ~choose:max_close l

let find_local_minima ~window_size l =
  window_map ~window_size ~choose:min_close l

let most_recent_maxima ~window_size l =
  max_close @@ Iter.rev @@ Iter.take window_size l

let qty ~current_cash ~pct ~price =
  match current_cash >=. 0.0 with
  | true ->
      let tenp = current_cash *. pct in
      let max_amt = tenp /. price in
      if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
  | false -> 0
