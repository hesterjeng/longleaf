![Longleaf](static/screenshot.png)

# Summary

This is an algorithmic trading platform written in OCaml.  At the moment, it only supports the Alpaca brokerage.  Both Alpaca and Tiingo can be used for market data.  Strategies are functors with a backend parameter, which in principle will allow other brokerages and market data sources to be used without changing the strategy itself.  Care is taken so that the same strategy can be used for backtesting as live execution, simple by changing the backend.

All executable have extra information about options that you can examine using the `--help` argument.

# Example usage

* Set up your `.envrc` in the root directory of the project.  To follow this example, you will need a Tiingo account to download data.  For a minimal example, save this as your `.envrc`.  Of course, you will need to populate your own keys.  I recommend using `direnv` to load this `.envrc` when you enter the `longleaf` directory.

```
export APCA_API_KEY_ID=myapcakeyid
export APCA_API_SECRET_KEY=myapcasecret
export TIINGO_KEY=mytiingokey
```

* Compile and install the program:
`dune clean; dune build; dune install`

* Download market data to populate the indicators for the backtest.  This is important, because otherwise when the backtest begins indicators like simple moving averages and stochastic oscillators would have no information.  This downloads market data for all of the symbols in the SP100 at the time of writing.  The target collection can be modified in `longleaf_downloader.ml`, in the `Cmd.run` function at the moment.  Market data can also be downloaded from Alpaca by replacing `tiingo` with `alpaca`.

`longleaf_downloader tiingo --begin=2023-12-01 --end=2023-12-31 --interval=10 --timeframe=minute data/dec23.json`

* Download some market data to run a backtest on.  This command downloads market data for all of the symbols in the SP100.
`longleaf_downloader tiingo --begin=2024-01-01 --end=2024-12-31 --interval=10 --timeframe=minute data/24.json`

* Backtest the strategy.
`longleaf Backtest ThrowingCrossover --preload data/dec23.json --target data/24.json`

* Paper trade the configured strategy on Alpaca.  The `--preload download` argument will download recent market data to populate the indicators.
`./main.exe Paper --preload download -o papertrading.log`

# Make your own strategy

Take a look at `strategies/template_example.ml`.  The easiest way to write a `longleaf` strategy is to use the strategy templates.  At the time of writing, this is limited to strategies that will only buy and sell stocks, meaning that there is no support for options, futures, etc. yet.  It is possible to directly write strategies that do more complicated things, but there isn't any documentation for this yet.

There are two components to a strategy, a module of type `Buy_trigger.INPUT` that contains functions for determining whether or not to buy a particular symbol and a function for ranking these symbols that pass the filter.  In addition, there is a `num_positions` value that limits the amount of positions the program will hold.  If there are more symbols that pass the filter than positions allowed, the `score` function will be used to select those with the highest value.

The `pass` function in a `Buy_trigger.INPUT` module will return a `Signal.Flag.t`.  If it is `Pass`, the symbol will be bought (if doing so doens't exceed the number of allowed positions), otherwise nothing happens.  Your module of type `Buy_trigger.INPUT` is then passed to the `Template.Buy_trigger.Make` functor to get a `Template.Buy_trigger.S`.

Now that the information for buying is there, we need to set up the information for selling.  We can directly write a `Template.Sell_trigger.S`, which has a single function, `make`.  This function takes in an `'a State.t'` and the buying order, and returns pass if we would like to close the position by selling.  Finally, we need to have a functor of type `Strategy.BUILDER`.  This functor will take in the backend to create an instatiated strategy.  We create it buy partially applying the `Template.Make` functor to our `Buy_trigger.S` and `Sell_trigger.S` functors.

Finally, we need to add a hook to call our strategy when we run the `longleaf` program.  Add a constructor for your strategy to the varaint type `Longleaf_strategies.t`, and add an element to `Longleaf_strategies.strats` like `MyStrategy --> (module My_strategy.Make)` if your strategy is locacted in `strategies/my_strategy.ml`, your constructor is `MyStrategy`, and the partially instantiated `Strategy.BUILDER` is `My_strategy.Make`.

# View data

Go to `http://localhost:8080/` to see some visualizations, statistics, and indicators about the behavior of your strategy when you have run the program.

# Stop the program

Execute `longleaf_shotdown`.  This will cause the program to gracefully exit and save any relevant data if options were specified to do so.

# Emacs usage

This is a small thing that probably won't be improved any further, and I probably won't update it as the program changes.

In the `elisp` directory, there is an Emacs Lisp file that allows you to select a preload and/or a target file and run backtests quickly.  To use use this, open the file and then load the buffer. In Doom Emacs:

* `SPC l p` - specify the preloaded data file
* `SPC l t` - specify the target data file for the algorithm to run on
* `SPC l r` - run the program as a backtest on these file

# Technical Details

Strategies are located in the `strategies` directory.  You can write your own by copying one of the small ones, like the listener or lowball, and then adding a hook into it in `strategies/longleaf_strategies.ml`.  This project uses `eio` to handle multiple domains, one for the strategy, and one for an http server that can receive commands like a graceful shutdown and deliver json for graph rendering in your browser.  The overarching flow looks like this:

Parse your options -> Your options are used to create a backend -> The strategy is instantiated with the backend -> The strategy runs

# Help
If you found this and want to try it, make an issue and I will help you out.  This is still a work in progress that's changing rapidly.
