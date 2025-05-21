module Astar = Longleaf_lib.Astar
module Context = Options.Context
module Error = Longleaf_lib.Error
module EnumeratedSignal = Astar_search.EnumeratedSignal

type results = {
  neighbors : node_ list; [@opaque]
  heuristic : float;
  goal : bool;
}

and node_ = {
  buy : EnumeratedSignal.t;
  sell : EnumeratedSignal.t;
  context : Context.t;
  res : results option Pmutex.t;
}
[@@deriving show]

let run (x : node_) =
  Eio.traceln "Running enumerated strategy %a" pp_node_ x;
  let res =
    if EnumeratedSignal.is_empty x.buy || EnumeratedSignal.is_empty x.sell then
      0.0
    else
      Template.Run.run_generic
        (EnumeratedSignal.to_strategy x.buy x.sell)
        x.context
  in
  Eio.traceln "Ending value: %f" res;
  res

module Node : Astar.INPUT with type node = node_ = struct
  type node = node_ [@@deriving show]

  let equal_node x y =
    EnumeratedSignal.equal x.buy y.buy && EnumeratedSignal.equal x.sell y.sell

  let neighbors_ x =
    let buy_neighbors =
      List.map (fun buy -> { x with buy; res = Pmutex.make None })
      @@ EnumeratedSignal.neighbors x.buy
    in
    let sell_neighbors =
      List.map (fun sell -> { x with sell; res = Pmutex.make None })
      @@ EnumeratedSignal.neighbors x.sell
    in
    let res = buy_neighbors @ sell_neighbors in
    Eio.traceln "found %d neighbors" (List.length res);
    res

  type winner = { winner_result : float; winner_node : node_ } [@@deriving show]

  let winner : winner option ref = ref None

  let compute_results x =
    let res = run x in
    let goal = res >=. 200000.0 in
    let winner_val =
      match !winner with
      | Some { winner_result; _ } ->
        if res >=. winner_result then
          Some { winner_result = res; winner_node = x }
        else !winner
      | None -> Some { winner_result = res; winner_node = x }
    in
    Eio.traceln "%a" (Option.pp pp_winner) winner_val;
    winner := winner_val;
    let neighbors = neighbors_ x in
    { neighbors; heuristic = res; goal }

  let goal x =
    match Pmutex.get x.res with
    | Some res -> res.goal
    | None ->
      let results = compute_results x in
      Pmutex.set x.res @@ Option.return results;
      results.goal

  let weight _ _ = 1.0

  let heuristic x =
    match Pmutex.get x.res with
    | Some res -> res.heuristic
    | None ->
      let results = compute_results x in
      Pmutex.set x.res @@ Option.return results;
      results.heuristic

  let neighbors x =
    match Pmutex.get x.res with
    | Some res -> res.neighbors
    | None ->
      let results = compute_results x in
      Pmutex.set x.res @@ Option.return results;
      results.neighbors
end

let empty context : node_ =
  {
    buy = EnumeratedSignal.Empty;
    sell = EnumeratedSignal.Empty;
    res = Pmutex.make None;
    context;
  }

module StrategySearch = Astar.Make (Node)

let top context =
  let res = StrategySearch.top @@ empty context in
  Eio.traceln "%a" (Option.pp StrategySearch.pp) res;
  res
