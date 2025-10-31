# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **Longleaf**, an algorithmic trading platform written in OCaml. It supports live trading, paper trading, and backtesting with multiple brokerages (currently Alpaca) and market data sources (Alpaca, Tiingo). The platform uses a functional, modular architecture with strategies implemented as functors for maximum code reuse and type safety.

## Common Development Commands

### Build and Installation
```bash
# Clean, build, and install
dune clean && dune build && dune install

# Format code
just format
# OR
dune build @fmt --display=quiet --auto-promote
prettier --write static/plotly_graph.js

# Generate dependency graph
just deps
# OR
odep dune | dot -Tsvg > dune-odep.svg
```

### Running the Platform
```bash
# Download market data (example for backtesting)
longleaf_downloader tiingo --begin=2024-01-01 --end=2024-12-31 --interval=10 --timeframe=minute data/2024.json

# Backtest a strategy
# Usage: longleaf Backtest <strategy> <data-file> -i <starting-index>
# The starting index should be high enough to allow indicators to compute (typically 200+)
longleaf Backtest RocketReef data/2024.json -i 200

# Paper trade (requires Alpaca API keys)
# Usage: longleaf Paper <strategy> <download|data-file> [options]
longleaf Paper RocketReef download -o papertrading.log

# Live trade (requires Alpaca API keys)
longleaf Live RocketReef download -o trading.log

# Graceful shutdown
longleaf_shutdown
```

### Testing
```bash
# Run tests
dune runtest

# Run specific test
dune exec test/test.exe
```

## Architecture Overview

### Core Components

1. **Backend System** (`lib/backend/`)
   - **`backend_intf.ml`**: Defines backend interface for different execution environments
   - **`alpaca_backend.ml`**: Live/paper trading implementation
   - **`backtesting_backend.ml`**: Historical simulation backend
   - **`backend.ml`**: Factory for creating backends based on run type

2. **Strategy System** (`strategies/`)
   - **Template Pattern**: `template.ml` provides `Buy_trigger` and `Sell_trigger` functors
   - **Strategy Registration**: `longleaf_strategies.ml` contains strategy registry
   - **Execution Engine**: `strategy_utils.ml` handles strategy execution loop

3. **Data Management** (`lib/bars/`)
   - **`data.ml`**: Core data structures for market data
   - **`bars.ml`**: Time series data management
   - **`latest.ml`**: Latest price data handling

4. **Trading Infrastructure** (`lib/`)
   - **`order.ml`**: Order lifecycle management
   - **`portfolio.ml`**: Position tracking and cash management
   - **`state.ml`**: Central state machine for strategy execution
   - **`signal.ml`**: Composable signal system for trading decisions

5. **Indicators** (`lib/indicators/`)
   - **`indicators.ml`**: Technical indicator computations
   - **`talib_binding.ml`**: TA-Lib integration for advanced indicators
   - **`config.ml`**: Indicator configuration

6. **APIs** (`lib/apis/`, `lib/`)
   - **`trading_api.ml`**: Generic trading API interface
   - **`tiingo_api.ml`**: Tiingo market data integration
   - **`alpaca_backend.ml`**: Alpaca brokerage integration

### Strategy Development Pattern

Strategies are built using OCaml functors following this pattern:

```ocaml
(* 1. Define buy trigger *)
module Buy_inp : Template.Buy_trigger.INPUT = struct
  let pass state symbol = (* entry condition logic *)
  let score state symbol = (* ranking logic *)
  let num_positions = (* maximum positions *)
end

(* 2. Define sell trigger *)
module Sell : Template.Sell_trigger.S = struct
  let make state ~buying_order = (* exit condition logic *)
end

(* 3. Create strategy functor *)
module Make : Strategy.BUILDER = Template.Make (Buy_inp) (Sell)
```

### Key Design Principles

1. **Backend Abstraction**: Same strategy code works for backtesting and live trading
2. **Functional Programming**: Immutable data structures, pure functions where possible
3. **Type Safety**: OCaml's type system prevents many runtime errors
4. **Modularity**: Clear separation of concerns with well-defined interfaces
5. **Composability**: Signals can be combined using monadic operators (`let&&`, `let||`)

### State Management

The platform uses a state machine with these states:
- `Initialize`: Setup indicators and data
- `Listening`: Wait for market events
- `Ordering`: Process buy/sell signals
- `Liquidate`: Close all positions
- `Finished`: Strategy completed

### Important Files to Understand

- **`lib/state.ml`**: Central state definitions and transitions
- **`lib/strategy_utils.ml`**: Main execution loop and state handling
- **`strategies/template.ml`**: Template system for strategy creation
- **`lib/backend/backend_intf.ml`**: Backend interface definitions
- **`lib/signal.ml`**: Signal composition system
- **`strategies/longleaf_strategies.ml`**: Strategy registration and execution

### Development Workflow

1. **Strategy Creation**: Use template system or implement custom strategy
2. **Registration**: Add strategy to `longleaf_strategies.ml` registry
3. **Testing**: Use backtesting mode with historical data
4. **Deployment**: Switch to paper trading, then live trading

### Environment Setup

Required environment variables (save in `.envrc`):
```bash
export APCA_API_KEY_ID=your_alpaca_key
export APCA_API_SECRET_KEY=your_alpaca_secret
export TIINGO_KEY=your_tiingo_key
```

### Web Interface

The platform includes a web server (runs on localhost:8080) that provides:
- Real-time portfolio visualization
- Strategy statistics and performance metrics
- Order history and execution details
- Technical indicator charts

### Current Limitations

- Limited to stock trading (no options, futures, etc.)
- Most strategies must use the template pattern
- Single-threaded strategy execution (uses Eio for concurrency)
- Currently only supports Alpaca brokerage

### Adding New Strategies

1. Create strategy file in `strategies/` directory
2. Implement using Template.Make functor or custom logic
3. Add variant to `Longleaf_strategies.t` type
4. Add handler in `Longleaf_strategies.strats` list
5. Test with backtesting before live deployment