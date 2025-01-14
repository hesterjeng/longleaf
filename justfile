@format:
	@dune build @fmt --display=quiet --auto-promote;
	@prettier --write lib/javascript/plotly_graph.js;

@watch:
	dune exec bin/server.exe --watch

@bt:
	dune exec bin/main.exe backtest

@profile:
	@dune clean;
	@dune build --profile=debug;
	@echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid;
	@perf record --call-graph dwarf -i -e cycles:u -- ./main.exe backtest --preload data/start_december_2024.json --target data/christmas_break_live.json;

@flamegraph:
	@sudo perf script -f > out.perf;
	@../FlameGraph/stackcollapse-perf.pl out.perf > out.folded;
	@../FlameGraph/flamegraph.pl out.folded > flamegraph.svg;

@deps:
	@odep dune | dot -Tsvg > dune-odep.svg
