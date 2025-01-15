@format:
	@dune build @fmt --display=quiet --auto-promote;
	@prettier --write lib/javascript/plotly_graph.js;

@deps:
	@odep dune | dot -Tsvg > dune-odep.svg
