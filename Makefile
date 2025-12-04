.PHONY: all clean build install format deps odep react run shutdown

# Port configuration (can override via environment or .envrc)
LONGLEAF_PORT ?= 8080

all:
	dune build

build:
	dune build
	cd react && npm run build

clean:
	dune clean
	cd react && npm run clean

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
	cd react && npm run build

run:
	cd react && npm run build
	tmux new-session -d -s longleaf
	tmux send-keys -t longleaf:0 "guix shell -m manifest.scm -- dune exec bin/longleaf_server.exe -- --port $(LONGLEAF_PORT)" Enter
	tmux new-window -t longleaf -n tearsheets
	tmux send-keys -t longleaf:tearsheets "guix shell -m manifest.scm -- python tearsheets/tearsheet_server.py" Enter
	tmux attach-session -t longleaf

shutdown:
	tmux kill-session -t longleaf
