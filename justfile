@format:
    dune build @fmt --display=quiet --auto-promote

@server:
	@dune exec bin/server.exe

@gui:
	@cd gui;
	@http-server -p 8000
