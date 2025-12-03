.PHONY: all clean build install format deps odep react run dev shutdown

# Port configuration via environment variables (set in .envrc)
LONGLEAF_PORT ?= 8080
REACT_PORT ?= 3000

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
	tmux send-keys -t longleaf:0 "guix shell -m manifest.scm -- bash -c 'cd react && npm run build && node server.js $(REACT_PORT) http://localhost:$(LONGLEAF_PORT)'" Enter
	tmux new-window -t longleaf -n tearsheets
	tmux send-keys -t longleaf:tearsheets "guix shell -m manifest.scm -- python tearsheets/tearsheet_server.py" Enter
	tmux new-window -t longleaf -n longleaf
	tmux send-keys -t longleaf:longleaf "guix shell -m manifest.scm -- dune exec bin/longleaf_server.exe -- --port $(LONGLEAF_PORT)" Enter
	tmux attach-session -t longleaf

dev:
	tmux new-session -d -s longleaf
	tmux send-keys -t longleaf:0 "guix shell -m manifest.scm -- bash -c 'cd react && npm start'" Enter
	tmux new-window -t longleaf -n tearsheets
	tmux send-keys -t longleaf:tearsheets "guix shell -m manifest.scm -- python tearsheets/tearsheet_server.py" Enter
	tmux new-window -t longleaf -n longleaf
	tmux send-keys -t longleaf:longleaf "guix shell -m manifest.scm -- dune exec bin/longleaf_server.exe" Enter
	tmux attach-session -t longleaf

shutdown:
	tmux kill-session -t longleaf
