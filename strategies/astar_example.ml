let top context =
  let example_node = Astar_run.example_node context in
  Astar_search.EnumeratedSignal.to_strategy example_node.buy example_node.sell
(* module Make = *)
(*   Astar_search.EnumeratedSignal.to_strategy example_node.buy example_node.sell *)
