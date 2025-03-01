![Longleaf](static/screenshot.png)

# Summary

This is an algorithmic trading platform written in OCaml.  At the moment, it only supports the Alpaca brokerage.  Both Alpaca and Tiingo can be used for market data.  Strategies are functors with a backend parameter, which in principle will allow other brokerages and market data sources to be used without changing the strategy itself.  Care is taken so that the same strategy can be used for backtesting as live execution, simple by changing the backend.

# Example usage

* Backtest the configured strategy:
`./main.exe backtest --preload data/tiingo_november.json --target data/tiingo_december.json`

* Paper trade the configured strategy:
`./main.exe paper --preload data/tiingo_november.json --target data/tiingo_december.json`

# View data

Go to `http://localhost:8080/` to see some visualizations, statistics, and indicators about the behavior of your strategy.

# Data downloader

There is a helpful data downloader executable that is also included.  The options for this can be examined by running it with the `--help` flag.

# Emacs usage

In the `elisp` directory, there is an Emacs Lisp file that allows you to select a preload and/or a target file and run backtests quickly.  To use use this, open the file and then load the buffer. In Doom Emacs:

* `SPC l p` - specify the preloaded data file
* `SPC l t` - specify the target data file for the algorithm to run on
* `SPC l r` - run the program as a backtest on these file

# Miscellaneous
In order to compile and run this program, you will need to create a `.envrc` file with some environment variables configured, corresponding to the environment variables in `environment.ml`.

# Technical Details

Strategies are located in the `strategies` directory.  You can write your own by copying one of the small ones, like the listener or lowball, and then adding a hook into it in `strategies/longleaf_strategies.ml`.  This project uses `eio` to handle multiple domains, one for the strategy, and one for an http server that can receive commands like a graceful shutdown and deliver json for graph rendering in your browser.  The overarching flow looks like this:

Parse your options -> Your options are used to create a backend -> The strategy is instantiated with the backend -> The strategy runs

# Help
If you found this and want to try it, make an issue and I will help you out.  This is still a work in progress that's changing rapidly.
