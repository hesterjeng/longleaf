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

let simple_moving_average ?current n (f : 'a -> float) (next : 'a -> 'a option)
    (x : 'a) =
  assert (n > 0);
  let counter, start =
    match current with
    | Some start ->
      assert (n > 1);
      (n - 1, start)
    | None -> (n, 0.0)
  in
  let rec aux n acc x =
    match n with
    | 0 -> acc
    | n -> (
      let curr = f x in
      match next x with
      | None -> curr :: acc
      | Some prev -> aux (n - 1) (curr :: acc) prev)
  in
  let l = aux counter [] x in
  let sum = List.fold_left ( +. ) start l in
  sum /. Float.of_int n

let ema n yesterday_ema today_value =
  (today_value *. (2.0 /. (1.0 +. Float.of_int n)))
  +. (yesterday_ema *. (1.0 -. (2.0 /. (1.0 +. Float.of_int n))))

let wilder n yesterday_ma today_value =
  yesterday_ma -. (yesterday_ma /. Float.of_int n) +. today_value

(* let last_n n (f : 'a -> float) (next : 'a -> 'a option) (x : 'a) *)
(*     = *)
(*   assert (n >= 0); *)
(*   let rec aux n acc x = *)
(*     match n with *)
(*     | 0 -> acc *)
(*     | n -> ( *)
(*         let curr = f x in *)
(*         match next x with *)
(*         | None -> curr :: acc *)
(*         | Some prev -> aux (n - 1) (curr :: acc) prev) *)
(*   in *)
(*   aux n [] x *)
