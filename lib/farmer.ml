type tc_action =
  | BuildVillager of int

let gather_rate = 0.32

type resource =
  | Food
  | Wood
  | Gold

type state =
  {
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
  let food = state.food +. ( gather_rate *. state.food_vills) in
  let wood = state.wood +. ( gather_rate *. state.wood_vills) in
  let gold = state.gold +. ( gather_rate *. state.gold_vills) in
  let tc_status, new_villager =
    match state.tc_status with
    | Some BuildVillager 0 -> None, 1.0
    | Some BuildVillager n -> Option.return @@ BuildVillager (n - 1), 0.0
    | None -> None, 0.0
  in
  ()

type action =
  | Nothing
  | AssignUnassigned of resource
  | BuildVillager

module A_star = struct
end
