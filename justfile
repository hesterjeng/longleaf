@format:
    dune build @fmt --display=quiet --auto-promote

@server:
	@dune exec bin/server.exe

@watch:
	dun exec bin/server.exe --watch
