@format:
    dune build @fmt --display=quiet --auto-promote

@server:
	@dune exec bin/server.exe;
	@prettier --write "src/javascript/**/*.js";


@watch:
	dune exec bin/server.exe --watch

@bt:
	dune exec bin/main.exe backtest
