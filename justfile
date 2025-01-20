@format:
	@dune build @fmt --display=quiet --auto-promote;
	@prettier --write static/plotly_graph.js;

@deps:
	@odep dune | dot -Tsvg > dune-odep.svg
