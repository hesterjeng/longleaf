@format:
    dune build @fmt --display=quiet --auto-promote

@server:
	@dune exec bin/server.exe

@watch:
	dune exec bin/server.exe --watch
