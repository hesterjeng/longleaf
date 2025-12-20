# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **Longleaf**, an algorithmic trading platform written in OCaml. It supports live trading, paper trading, backtesting, and battery testing with multiple brokerages (Alpaca) and market data sources (Alpaca, Tiingo, Massive). The platform uses a GADT-based DSL for strategy definition, enabling both type-safe strategy composition and automated parameter optimization via NLopt/ISRES.

## Common Development Commands

### Build and Installation
```bash
# Clean, build, and install
dune clean && dune build && dune install

# Format code
dune build @fmt --display=quiet --auto-promote

# Generate dependency graph
odep dune | dot -Tsvg > dune-odep.svg
```

### Running the Platform
```bash
# Download market data
longleaf_downloader tiingo --begin=2024-01-01 --end=2024-12-31 --interval=10 --timeframe=minute data/2024.json

# Backtest a strategy
longleaf Backtest <strategy> <data-file> -i <starting-index>
longleaf Backtest MRNB_3_A data/q3q4-2025-1min.json -i 400

# Battery test (evaluate strategy across multiple periods)
longleaf Battery <strategy> quarterly
# Available batteries: quarterly, quarterly_2023_2025

# Paper trade (requires Alpaca API keys)
longleaf Paper <strategy> download -o papertrading.log

# Live trade (requires Alpaca API keys)
longleaf Live <strategy> download -o trading.log

# List available strategies
longleaf --help
```

### Testing
```bash
dune runtest
```

## Architecture Overview

### Directory Structure

```
longleaf/
├── bin/                    # CLI executables
│   ├── longleaf.ml         # Main CLI entry point
│   ├── longleaf_downloader.ml  # Data download tool
│   └── longleaf_server.ml  # Web server
├── lib/
│   ├── apis/               # External API integrations
│   │   ├── alpaca_*.ml     # Alpaca brokerage
│   │   ├── tiingo_api.ml   # Tiingo market data
│   │   ├── massive_*.ml    # Massive market data
│   │   └── downloader.ml   # Data download orchestration
│   ├── backend/            # Execution backends
│   │   ├── backend_intf.ml # Backend interface
│   │   ├── alpaca_backend.ml   # Live/paper trading
│   │   └── backtesting_backend.ml  # Historical simulation
│   ├── bars/               # Market data structures
│   │   ├── data.ml         # Core data types
│   │   └── bars.ml         # Time series management
│   ├── battery/            # Battery testing framework
│   │   └── battery.ml      # Multi-period evaluation
│   ├── core/               # Core types and utilities
│   │   ├── options.ml      # CLI options parsing
│   │   ├── runtype.ml      # Run modes (Live/Paper/Backtest/Battery)
│   │   └── target.ml       # Data targets (File/Download/Battery)
│   ├── indicators/         # Technical indicators
│   │   └── indicators.ml   # TA-Lib bindings via Tacaml
│   └── state/              # State management
│       ├── state.ml        # Trading state machine
│       └── stats.ml        # Performance statistics
├── strategies/
│   ├── gadt/               # GADT-based strategies (primary)
│   │   ├── gadt.ml         # Core GADT DSL
│   │   ├── gadt_strategy.ml    # Strategy type and execution
│   │   ├── gadt_fo.ml      # First-order optimization variables
│   │   ├── gadt_atomic.ml  # Atomic optimization runner
│   │   ├── gadt_examples.ml    # All strategy definitions
│   │   ├── mean_reversion.ml   # Mean reversion strategies
│   │   └── volume_momentum.ml  # Volume/momentum strategies
│   ├── template/           # Legacy functor-based strategies
│   │   └── longleaf_template.ml
│   ├── battery_runner.ml   # Battery test execution
│   └── longleaf_strategies.ml  # Strategy registry
├── data/                   # Market data files (JSON)
└── react/                  # Web UI (React)
```

### GADT Strategy System

Strategies are defined using a type-safe GADT DSL that enables:
- Composable buy/sell triggers using boolean operators
- Automatic indicator collection and computation
- Parameter optimization via NLopt/ISRES

```ocaml
(* Strategy structure *)
type t = {
  name : string;
  buy_trigger : bool Gadt.t;      (* Entry condition *)
  sell_trigger : bool Gadt.t;     (* Exit condition *)
  score : float Gadt.t;           (* Ranking function *)
  max_positions : int;
  position_size : float;
}

(* Example: Simple mean reversion strategy *)
let example_strategy =
  let mfi = Gadt.Data (App1 (Fun ("mfi", I.mfi), Const (90, Int))) in
  let bb_lower = ... in
  {
    name = "Example";
    buy_trigger = mfi <. Const (30.0, Float) &&. (last <. bb_lower);
    sell_trigger = mfi >. Const (50.0, Float) ||. force_exit_eod ();
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }
```

### Optimization with ISRES

Strategies can include optimization variables that NLopt tunes:

```ocaml
(* Define a variable with bounds *)
let mfi_period_var = Gadt_fo.var ~lower:50.0 ~upper:200.0 Type.Int

(* Use in indicator - ISRES will optimize this value *)
let mfi = Gadt.Data (App1 (Fun ("mfi", I.mfi), mfi_period_var))
```

Optimization runs via `Gadt_atomic.opt_atomic` during backtesting.

### Battery Testing

Battery testing evaluates strategy performance across multiple time periods:

```ocaml
(* Predefined battery: 6 half-year periods from 2023-2025 *)
let quarterly_2023_2025 = Battery.evaluate [
  Target.File "data/q1q2-2023-1min.json";
  Target.File "data/q3q4-2023-1min.json";
  Target.File "data/q1q2-2024-1min.json";
  Target.File "data/q3q4-2024-1min.json";
  Target.File "data/q1q2-2025-1min.json";
  Target.File "data/q3q4-2025-1min.json";
]
```

Output includes per-period Sharpe, return, drawdown, and aggregate statistics.

### Key GADT DSL Operators

```ocaml
(* Comparison *)
let ( <. ) = Gadt.lt
let ( >. ) = Gadt.gt
let ( =. ) = Gadt.eq

(* Boolean *)
let ( &&. ) = Gadt.and_
let ( ||. ) = Gadt.or_

(* Arithmetic *)
let ( +. ) = Gadt.add
let ( -. ) = Gadt.sub
let ( *. ) = Gadt.mul

(* Data access *)
let last = Gadt.last           (* Current price *)
let lag x n = Gadt.lag x n     (* Lagged value *)
let EntryPrice                 (* Position entry price *)
let TicksHeld                  (* Bars since entry *)

(* Safety *)
let safe_to_enter ()           (* Not near EOD *)
let force_exit_eod ()          (* Force exit at EOD *)
```

### Adding New Strategies

1. Create strategy in `strategies/gadt/` (e.g., in `mean_reversion.ml`)
2. Add to `all_strategies` list in that file
3. Strategies are auto-registered via `Gadt_examples.all_strategies`
4. Test with: `longleaf Backtest <name> <data-file> -i 400`

### Current Mean Reversion Strategies

| Strategy | Description |
|----------|-------------|
| `MR_Basic` | Baseline with textbook defaults (no optimization) |
| `MR0_Opt` | Lean 6-variable optimizable strategy |
| `Nature_Boy_V3_Opt` | Full 14-variable optimizable strategy |
| `MRNB_3_A/B/C` | ISRES-trained variants (locked parameters) |
| `NB_V2` | Earlier trained variant |

### Environment Setup

Required environment variables (save in `.envrc`):
```bash
export APCA_API_KEY_ID=your_alpaca_key
export APCA_API_SECRET_KEY=your_alpaca_secret
export TIINGO_KEY=your_tiingo_key
```

### Data Files

Battery testing expects these files in `data/`:
- `q1q2-2023-1min.json`, `q3q4-2023-1min.json`
- `q1q2-2024-1min.json`, `q3q4-2024-1min.json`
- `q1q2-2025-1min.json`, `q3q4-2025-1min.json`

Download with `longleaf_downloader tiingo --begin=YYYY-MM-DD --end=YYYY-MM-DD --interval=1 --timeframe=minute <output.json>`

### Run Types

| Type | Description |
|------|-------------|
| `Backtest` | Single historical simulation |
| `Battery` | Multi-period evaluation |
| `Paper` | Paper trading (Alpaca) |
| `Live` | Live trading (Alpaca) |

### State Machine

Trading execution uses these states:
- `Initialize`: Setup indicators and data
- `Listening`: Wait for market events
- `Ordering`: Process buy/sell signals
- `Liquidate`: Close all positions
- `Finished`: Strategy completed

### Current Limitations

- Stock trading only (no options, futures, crypto)
- S&P 100 universe for most strategies
- Single strategy execution per run
- Intraday strategies only (positions closed at EOD)
