.PHONY: all clean build install format deps odep react run run2 shutdown shutdown2 run-prod

all:
	dune build

build:
	dune build

install:
	dune build && dune install && cd react && npm install

format:
	dune build @fmt --display=quiet --auto-promote

odep:
	odep dune | dot -Tsvg > dune-odep.svg

deps:
	@echo "Use 'guix shell -m manifest.scm' for development environment"
	@echo "Or 'guix shell -f longleaf.scm' for full OCaml environment"

react:
	cd react && npm start

run:
	tmux new-session -d -s longleaf
	tmux send-keys -t longleaf:0 "guix shell -m manifest.scm -- bash -c 'cd react && npm start'" Enter
	tmux new-window -t longleaf -n tearsheets
	tmux send-keys -t longleaf:tearsheets "guix shell -m manifest.scm -- python tearsheets/tearsheet_server.py" Enter
	tmux new-window -t longleaf -n longleaf
	tmux send-keys -t longleaf:longleaf "guix shell -m manifest.scm -- dune exec bin/longleaf_server.exe" Enter
	tmux attach-session -t longleaf

run2:
	tmux new-session -d -s longleaf2
	tmux send-keys -t longleaf2:0 "guix shell -m manifest.scm -- bash -c 'cd react && PORT=3001 REACT_APP_API_URL=http://localhost:8081 npm start'" Enter
	tmux new-window -t longleaf2 -n longleaf
	tmux send-keys -t longleaf2:longleaf "guix shell -m manifest.scm -- dune exec bin/longleaf_server.exe -- --port 8081 --cors-origin http://localhost:3001" Enter
	tmux attach-session -t longleaf2

run-prod:
	tmux new-session -d -s longleaf
	tmux send-keys -t longleaf:0 "guix shell -m manifest.scm -- bash -c 'cd react && npm run build && npm run serve'" Enter
	tmux new-window -t longleaf -n tearsheets
	tmux send-keys -t longleaf:tearsheets "guix shell -m manifest.scm -- python tearsheets/tearsheet_server.py" Enter
	tmux new-window -t longleaf -n longleaf
	tmux send-keys -t longleaf:longleaf "guix shell -m manifest.scm -- dune exec bin/longleaf_server.exe" Enter
	tmux attach-session -t longleaf

shutdown:
	tmux kill-session -t longleaf

shutdown2:
	tmux kill-session -t longleaf2
