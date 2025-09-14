![Longleaf](static/screenshot.png)

# Longleaf

An algorithmic trading platform written in OCaml that supports live trading, paper trading, and backtesting. The platform uses a functional, modular architecture with strategies implemented as functors for maximum code reuse and type safety.

## Features

- **Multiple execution modes**: Backtesting, paper trading, and live trading
- **Brokerage support**: Alpaca (with extensible backend system)
- **Market data sources**: Alpaca and Tiingo
- **Technical indicators**: Integration with TA-lib via tacaml
- **Web dashboard**: Real-time visualization using React
- **Strategy templates**: Functional approach with reusable components

## Installation

You'll need to install the following dependencies:
- **tacaml**: OCaml bindings for TA-lib technical analysis
- **TA-lib**: Technical analysis library (system dependency)

## Setup

1. **Environment variables**: Create a `.envrc` file in the project root with your API keys:

```bash
export APCA_API_KEY_ID=your_alpaca_key_id
export APCA_API_SECRET_KEY=your_alpaca_secret_key
export TIINGO_KEY=your_tiingo_key
export DASHBOARD_PASSWORD_HASH=your_hashed_password  # Optional: for dashboard auth
```

2. **Build the project**:
```bash
dune clean && dune build && dune install
```

## Example Usage

### Download Market Data

Download historical data to populate indicators (preload data):
```bash
longleaf_downloader tiingo --begin=2023-12-01 --end=2023-12-31 --interval=10 --timeframe=minute data/dec23.json
```

Download target data for backtesting:
```bash
longleaf_downloader tiingo --begin=2024-01-01 --end=2024-12-31 --interval=10 --timeframe=minute data/24.json
```

### Backtesting

Run a backtest with different strategies:
```bash
# Basic backtest starting from index 100
longleaf Backtest AStarExample data/24.json -i 100

# With preloaded indicator data
longleaf Backtest ThrowingCrossover --preload data/dec23.json --target data/24.json

# Other strategy examples
longleaf Backtest RsiMeanReversion data/24.json -i 100
longleaf Backtest MacdBollingerMomentum --preload data/dec23.json --target data/24.json
longleaf Backtest VolumeBreakout data/24.json -i 50
```

### Paper Trading

Run paper trading with live market data:
```bash
longleaf Paper --preload download -o papertrading.log
```

### Live Trading

**⚠️ Use with extreme caution - this trades real money!**
```bash
longleaf Live --preload download -o live_trading.log
```

## Web Dashboard

The platform includes a real-time web dashboard for visualization:

1. **Start your strategy** (backtest, paper, or live):
```bash
longleaf Backtest AStarExample data/24.json -i 100
```

2. **Launch the dashboard**:
```bash
cd react
npm start
```

3. **Access the dashboard**: Open `http://localhost:3000` in your browser

The dashboard provides:
- Real-time portfolio visualization
- Technical indicator charts with 30+ traces
- Strategy statistics and performance metrics
- Interactive symbol selection and data exploration

## Available Strategies

The platform includes numerous built-in strategies:

- **AStarExample**: A* search-based strategy example
- **ThrowingCrossover**: Moving average crossover with momentum
- **RsiMeanReversion**: RSI-based mean reversion strategy
- **MacdBollingerMomentum**: MACD and Bollinger Band momentum strategy
- **VolumeBreakout**: Volume-based breakout detection
- **AdaptiveMomentumRegime**: Regime-aware momentum strategy
- **BuyAndHold**: Simple buy and hold benchmark
- And many more in the `strategies/` directory

## Creating Custom Strategies

Strategies are built using OCaml functors with the template system. See existing strategies in `strategies/` for examples. The basic pattern:

1. Define buy trigger logic (entry conditions)
2. Define sell trigger logic (exit conditions)  
3. Register strategy in `strategies/longleaf_strategies.ml`

All strategies work seamlessly across backtesting, paper trading, and live trading modes.

## Graceful Shutdown

Stop the program safely:
```bash
longleaf_shutdown
```

This ensures proper cleanup and data saving.

## Technical Details

Strategies are located in the `strategies/` directory. The platform uses `eio` for concurrent execution with separate domains for strategy execution and the HTTP server. The architecture follows:

**Parse options → Create backend → Instantiate strategy → Execute strategy**

The backend abstraction allows the same strategy code to work across different execution environments (backtesting vs. live trading) and different brokerages, simply by changing the backend implementation.

## Command Line Options

All executables provide detailed help information:
```bash
longleaf --help
longleaf_downloader --help
longleaf_shutdown --help
```

## Help

This is an active project under development. If you encounter issues or want to contribute, please create an issue on the repository.