module Search = Longleaf_lib.Astar
module Context = Options.Context
module Error = Longleaf_lib.Error
module EnumeratedSignal = Astar_search.EnumeratedSignal

type results = { neighbors : node_ list; heuristic : float; goal : bool }

and node_ = {
  buy : EnumeratedSignal.t;
  sell : EnumeratedSignal.t;
  context : Context.t;
  res : results option Pmutex.t;
}
[@@deriving show]

let run (x : node_) =
  Template.Run.run_generic (EnumeratedSignal.to_strategy x.buy x.sell) x.context

module Node : Search.INPUT with type node = node_ = struct
  type node = node_ [@@deriving show]

  let equal_node x y =
    EnumeratedSignal.equal x.buy y.buy && EnumeratedSignal.equal x.sell y.sell

  let goal x =
    match Pmutex.get x.res with
    | Some res -> res.goal
    | None ->
        let res = run x in
        let goal = res >=. 200000.0 in
        Pmutex.set x.res @@ Option.return
        @@ { neighbors = []; heuristic = res; goal };
        goal

  let weight _ _ = 1.0
  let neighbors _ = []
  let heuristic _ = 0.0
end

let empty context : node_ =
  {
    buy = EnumeratedSignal.Empty;
    sell = EnumeratedSignal.Empty;
    res = Pmutex.make None;
    context;
  }

module StrategySearch = Search.Make (Node)

let top context =
  let res = StrategySearch.top @@ empty context in
  res
