.PHONY: all clean build install format deps react

all:
	dune build

build:
	dune build

install:
	dune build && dune install && cd react && npm install

format:
	dune build @fmt --display=quiet --auto-promote

deps:
	odep dune | dot -Tsvg > dune-odep.svg

react:
	cd react && npm start
