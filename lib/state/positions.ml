module PosMap = Map.Make (Longleaf_core.Instrument)

type t = Order.t list PosMap.t

let empty = PosMap.empty
let get (pos : t) symbol = PosMap.get symbol pos |> Option.get_or ~default:[]
(* Result.o *)
(* |> Result.map_err (fun e -> `FatalError e) *)

let symbols x = PosMap.keys x |> Iter.to_list

let fold (pos : t) acc (f : Instrument.t -> Order.t list -> 'a -> 'a) =
  PosMap.fold f pos acc

let qty (t : t) instrument =
  let pos = get t instrument in
  let res =
    List.fold_left
      (fun acc (order : Order.t) ->
        match order.side with
        | Buy -> acc + order.qty
        | Sell -> acc - order.qty)
      0 pos
  in
  res

let update (pos : t) (order : Order.t) =
  let symbol = order.symbol in
  let new_pos =
    PosMap.update symbol
      (function
        | Some l -> Some (order :: l)
        | None -> Some [ order ])
      pos
  in
  let qty = qty new_pos symbol in
  match qty with
  | 0 -> PosMap.remove symbol new_pos
  | _ -> new_pos

let cost_basis (pos : t) symbol =
  let pos = get pos symbol in
  List.fold_left
    (fun acc (order : Order.t) ->
      match order.side with
      | Buy -> acc -. (order.price *. float_of_int order.qty)
      | Sell -> acc +. (order.price *. float_of_int order.qty))
    0.0 pos
