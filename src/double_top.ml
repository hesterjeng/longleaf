module Math = struct
  module Bar_item = Trading_types.Bars.Bar_item

  type critical_point =
    | Min of float
    | Max of float

  let local_maxima (l : Bar_item.t list) =
    let rec aux l acc =
      match l with
      | [] -> List.rev acc
      | x :: xs ->
        let acc =
          if
        in
    in
    match l with [] | [ _ ] | [ _; _ ] -> None | _ -> Some (aux l [])
end
