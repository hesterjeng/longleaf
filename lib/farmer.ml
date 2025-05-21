type tc_action = BuildVillager of int

let gather_rate = 0.32

type resource = Food | Wood | Gold

type state = {
  tick : float;
  food : float;
  wood : float;
  gold : float;
  food_vills : float;
  wood_vills : float;
  gold_vills : float;
  unassigned_vills : float;
  tc_status : tc_action option;
}

let tick (state : state) =
  let food = state.food +. (gather_rate *. state.food_vills) in
  let wood = state.wood +. (gather_rate *. state.wood_vills) in
  let gold = state.gold +. (gather_rate *. state.gold_vills) in
  let tc_status, new_villager =
    match state.tc_status with
    | Some (BuildVillager 0) -> (None, 1.0)
    | Some (BuildVillager n) -> (Option.return @@ BuildVillager (n - 1), 0.0)
    | None -> (None, 0.0)
  in
  {
    state with
    food;
    wood;
    gold;
    tc_status;
    unassigned_vills = new_villager +. state.unassigned_vills;
  }

type action = Nothing | AssignUnassigned of resource | BuildVillager

let do_action state action =
  let ( let* ) = Option.( let* ) in
  match action with
  | Nothing -> Some state
  | AssignUnassigned res -> (
    let* unassigned =
      match state.unassigned_vills with
      | 0.0 -> None
      | n -> Some n
    in
    Option.return
    @@
    match res with
    | Food -> { state with food_vills = unassigned +. state.food_vills }
    | Wood -> { state with wood_vills = unassigned +. state.wood_vills }
    | Gold -> { state with gold_vills = unassigned +. state.gold_vills })
  | BuildVillager ->
    let* food = if state.food >. 50.0 then Some state.food else None in
    Option.return
    @@ { state with tc_status = Some (BuildVillager 25); food = food -. 50.0 }

let next_possible_states state = invalid_arg "nyi"

module Diophantine = struct
  type node = int

  let weight _ _ = 1.0
  let neighbors x = [ x + 2; x - 3 ]
  let goal x = x = 1423
  let heuristic x = Float.of_int @@ Int.abs @@ (x - 1423)
  let pp_node = Int.pp
  let equal_node = Int.equal
end

module Search = Astar.Make (Diophantine)
