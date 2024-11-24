# Summary

This is an algorithmic trading platform written in OCaml.  At the moment, it only supports the Alpaca brokerage and market data API.  Strategies are functors with a backend parameter, which in principle will allow other brokerages and market data sources to be used without changing the strategy itself.  Care is taken so that the same strategy can be used for backtesting as live execution, simple by changing the backend.

# Example usage

* Backtest the configured strategy:
`./main.exe backtest`

* Paper trade the configured strategy:
`./main.exe paper`

# Miscellaneous
In order to compile and run this program, you will need to create a `.envrc` file with some environment variables configured, corresponding to the environment variables in `environment.ml`.
