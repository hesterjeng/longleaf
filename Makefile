.PHONY: all clean build install format deps odep react run run-limited shutdown run20 run-prod

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

_tmux-setup:
	tmux new-session -d -s longleaf
	tmux send-keys -t longleaf:0 "cd react && npm start" Enter
	tmux new-window -t longleaf -n tearsheets
	tmux send-keys -t longleaf:tearsheets "python tearsheets/tearsheet_server.py" Enter
	tmux new-window -t longleaf -n longleaf
	tmux send-keys -t longleaf:longleaf "dune exec bin/longleaf_server.exe" Enter
	tmux attach-session -t longleaf

run:
	guix shell --rebuild-cache -m manifest.scm -- $(MAKE) _tmux-setup

run-limited:
	systemd-run --user --scope -p MemoryMax=8G \
		guix shell --rebuild-cache -m manifest.scm -- $(MAKE) _tmux-setup

run20:
	systemd-run --user --scope -p MemoryMax=20G \
		guix shell --rebuild-cache -m manifest.scm -- $(MAKE) _tmux-setup

_tmux-setup-prod:
	tmux new-session -d -s longleaf
	tmux send-keys -t longleaf:0 "cd react && npm run build && npm run serve" Enter
	tmux new-window -t longleaf -n tearsheets
	tmux send-keys -t longleaf:tearsheets "python tearsheets/tearsheet_server.py" Enter
	tmux new-window -t longleaf -n longleaf
	tmux send-keys -t longleaf:longleaf "dune exec bin/longleaf_server.exe" Enter
	tmux attach-session -t longleaf

run-prod:
	systemd-run --user --scope -p MemoryMax=6G \
		guix shell --rebuild-cache -m manifest.scm -- $(MAKE) _tmux-setup-prod

shutdown:
	tmux kill-session -t longleaf
