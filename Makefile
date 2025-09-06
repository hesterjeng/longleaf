.PHONY: all clean

all: build

build: dune build

install: dune bnuild && dune install && cd react && npm install

deps: odep dune | dot -Tsvg > dune-odep.svg

react: cd react && npm start
