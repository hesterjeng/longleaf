@format:
    dune build @fmt --display=quiet --auto-promote

@run:
	@tmux new-session -d -s server_session 'dune exec bin/server.exe' &
	tmux new-session -d -s gui_session 'dune exec bin/gui.exe'

@kill:
	@tmux kill-server

@server:
	@tmux attach-session -t server_session

@gui:
	@tmux attach-session -t gui_session

@detach:
	@tmux detach
