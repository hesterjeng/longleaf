# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **Longleaf**, an algorithmic trading platform written in OCaml. It supports live trading, paper trading, backtesting, and battery testing with multiple brokerages (Alpaca) and market data sources (Massive preferred, Tiingo deprecated). The platform uses a GADT-based DSL for strategy definition, enabling both type-safe strategy composition and automated parameter optimization via NLopt/ISRES.

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
# Download market data (use Massive, not Tiingo)
longleaf_downloader massive --begin=2024-01-01 --end=2024-12-31 --interval=1 --timeframe=minute data/2024.json

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
│   │   ├── massive_*.ml    # Massive market data (preferred)
│   │   ├── tiingo_api.ml   # Tiingo market data (deprecated)
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

(* Example: Simple mean reversion strategy using gadt_fo helpers *)
let example_strategy =
  let module Real = Gadt_fo.Constant in
  let mfi = Real.mfi 90 in
  let bb_lower = Real.lower_bband 20 2.0 2.0 in
  {
    name = "Example";
    buy_trigger = mfi <. Const (30.0, Float) &&. (last <. bb_lower) &&. safe_to_enter ();
    sell_trigger = mfi >. Const (50.0, Float) ||. max_holding_time 120 ||. force_exit_eod ();
    score = Const (100.0, Float) -. mfi;
    max_positions = 5;
    position_size = 0.20;
  }
```

### Optimization with ISRES

Strategies can include optimization variables that NLopt tunes:

```ocaml
(* Option 1: Create shared variable, use Indicator module *)
let mfi_period_var = Gadt_fo.var ~lower:50.0 ~upper:200.0 Type.Int
let mfi = Gadt_fo.Indicator.mfi mfi_period_var

(* Option 2: Use Variable module for independent optimization vars *)
let mfi = Gadt_fo.Variable.mfi ~lower:50.0 ~upper:200.0 ()
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
let not_ = Gadt.not_           (* Negation *)

(* Arithmetic *)
let ( +. ) = Gadt.add
let ( -. ) = Gadt.sub
let ( *. ) = Gadt.mul

(* Data access *)
let last = Gadt.last           (* Current price *)
let lag x n = Gadt.lag x n     (* Lagged value *)
let EntryPrice                 (* Position entry price *)
let TicksHeld                  (* Bars since entry *)

(* Safety - from Gadt_strategy *)
let safe_to_enter ()           (* Not near EOD *)
let force_exit_eod ()          (* Force exit at EOD *)
```

### Indicator Constructors (gadt_fo.ml) - IMPORTANT

**Always use `Gadt_fo` for technical indicators instead of manually constructing GADT expressions.**

The module provides a 3-tier architecture for indicator construction:

#### 1. Constant Module - Fixed parameter values
Use for strategies with hardcoded indicator parameters:

```ocaml
module Real = Gadt_fo.Constant

(* Single-argument indicators *)
let rsi = Real.rsi 14
let sma = Real.sma 20
let ema = Real.ema 50
let mfi = Real.mfi 90
let adx = Real.adx 14
let atr = Real.atr 14

(* Multi-argument indicators *)
let bb_lower = Real.lower_bband 20 2.0 2.0    (* period, nbdev_up, nbdev_dn *)
let bb_upper = Real.upper_bband 20 2.0 2.0
let macd = Real.macd_macd 12 26 9             (* fast, slow, signal *)
let stoch_k = Real.stoch_slow_k 14 3 3        (* fast_k, slow_k, slow_d *)
```

#### 2. Indicator Module - Expression-based parameters
Use when sharing variables between indicators or passing complex expressions:

```ocaml
module Ind = Gadt_fo.Indicator

(* Shared period variable across multiple indicators *)
let period_var = Gadt_fo.var ~lower:10.0 ~upper:50.0 Type.Int
let rsi = Ind.rsi period_var
let bb_lower = Ind.lower_bband period_var (Const (2.0, Float)) (Const (2.0, Float))
let bb_upper = Ind.upper_bband period_var (Const (2.0, Float)) (Const (2.0, Float))
```

#### 3. Variable Module - Fresh optimization variables
Use for creating independent optimization variables (each call creates a new UUID):

```ocaml
module Opt = Gadt_fo.Variable

(* Each indicator gets its own optimizable period *)
let rsi = Opt.rsi ~lower:5.0 ~upper:30.0 ()
let mfi = Opt.mfi ~lower:50.0 ~upper:200.0 ()
let adx = Opt.adx ~lower:10.0 ~upper:30.0 ()
```

#### DO NOT manually construct indicators like this:
```ocaml
(* BAD - verbose and error-prone *)
let rsi =
  Gadt.Data
    (App1
       ( Fun ("tacaml", fun x -> Data.Type.Tacaml x),
         App1 (Fun ("I.rsi", Tacaml.Indicator.Raw.rsi), Const (14, Int)) ))

(* GOOD - use the helper *)
let rsi = Real.rsi 14
```

### Strategy Helper Functions (gadt_strategy.ml)

Common exit conditions and safety checks:

```ocaml
(* Exit conditions *)
stop_loss 0.02                    (* Exit if price drops 2% below entry *)
profit_target 0.05                (* Exit if price rises 5% above entry *)
max_holding_time 120              (* Exit after 120 bars *)
max_holding_time_expr var         (* Exit after variable bars *)
min_holding_time 10               (* Gate: held at least 10 bars *)
min_holding_time_expr var         (* Gate: held at least variable bars *)

(* Intraday safety *)
safe_to_enter ()                  (* Not within 10 min of close *)
safe_to_enter ~close_buffer:15.0 ()  (* Custom buffer *)
force_exit_eod ()                 (* Within 10 min of close *)

(* Example sell trigger using helpers *)
let sell_trigger =
  stop_loss 0.02
  ||. profit_target 0.05
  ||. max_holding_time 120
  ||. force_exit_eod ()
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
export MASSIVE_KEY=your_massive_key
```

### Data Files

Battery testing expects these files in `data/`:
- `q1q2-2023-1min.json`, `q3q4-2023-1min.json`
- `q1q2-2024-1min.json`, `q3q4-2024-1min.json`
- `q1q2-2025-1min.json`, `q3q4-2025-1min.json`

Download with `longleaf_downloader massive --begin=YYYY-MM-DD --end=YYYY-MM-DD --interval=1 --timeframe=minute <output.json> -u <universe>`

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
