module type INPUT = sig
  type node

  val weight : node -> node -> float
  val neighbors : node -> node list
  val goal : node -> bool
  val heuristic : node -> float
  val pp_node : node Format.printer
  val equal_node : node -> node -> bool
end

module Make (Input : INPUT) = struct
  type t = { nodes : Input.node list; cost : float; f : float }
  [@@deriving show, eq]

  let minimal_path paths =
    let rec aux paths minimum =
      match paths with
      | [] -> minimum
      | path :: xs ->
        (match path.f <=. minimum.f with
        | true -> aux xs path
        | false -> aux xs minimum)
    in
    match paths with
    | [] -> None
    | x :: xs -> Option.return @@ aux xs x

  (* aux paths { nodes = []; cost = Float.max_finite_value } *)

  let extend all path =
    let new_paths =
      match path.nodes with
      | [] -> []
      | x :: _ ->
        let neighbors = Input.neighbors x in
        List.map
          (fun (neighbor : Input.node) ->
            let cost = path.cost +. Input.weight x neighbor in
            {
              nodes = neighbor :: path.nodes;
              cost;
              f = cost +. Input.heuristic neighbor;
            })
          neighbors
    in
    (* Eio.traceln "%a" (List.pp pp) new_paths; *)
    new_paths @ List.filter (fun x -> not @@ equal x path) all

  let top (origin : Input.node) =
    let ( let* ) = Option.( let* ) in
    let initial_paths =
      List.map (fun x ->
          { nodes = [ origin ]; cost = 0.0; f = Input.heuristic x })
      @@ Input.neighbors origin
    in
    let rec aux paths =
      let* minimal_path = minimal_path paths in
      Eio.traceln "Extending %a" pp minimal_path;
      match minimal_path.nodes with
      | x :: _ when Input.goal x -> Option.return minimal_path
      | _ ->
        let paths = extend paths minimal_path in
        aux paths
    in
    aux initial_paths
end
