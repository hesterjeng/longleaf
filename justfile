@format:
	@dune build @fmt --display=quiet --auto-promote;

@deps:
	@odep dune | dot -Tsvg > dune-odep.svg

@install:
    @dune build;
    @dune install;
    @cd react && npm install;

@dashboard:
    @streamlit run streamlit/new_dashboard.py

@react:
    @cd react && npm start
