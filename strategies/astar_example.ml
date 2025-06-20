let top context =
  let example_node = Astar_run.example_node context in
  Astar_search.run_astar context ~buy:example_node.buy ~sell:example_node.sell
