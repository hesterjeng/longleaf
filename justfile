@format:
    dune build @fmt --display=quiet --auto-promote

@run:
	dune exec bin/server.exe;
	dune exec bin/gui.exe;
	wait
