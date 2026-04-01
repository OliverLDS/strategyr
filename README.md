# strategyr

`strategyr` is an execution-oriented R package for modular strategy workflows. It is designed to transform already-available market features, portfolio state, and dynamic quantitative analysis into actionable signals, target positions, portfolio adjustments, and order intents.

The package emphasizes path-dependent backtesting so strategy logic is evaluated under evolving account state, execution assumptions, and market conditions rather than through purely static signal research.

Compared with `investlabr`, which is oriented toward exploratory research and communication, `strategyr` is intended for disciplined strategy logic and timely investment decision support.

## Installation

```r
devtools::install_github("OliverLDS/strategyr")
```

## Usage Cases

### 1. Generate a target-position path and run a path-dependent backtest

```r
library(data.table)
library(strategyr)

DT <- data.table(
  datetime = seq(0, by = 60, length.out = 200),
  open = 100,
  high = 101,
  low = 99,
  close = seq(100, 120, length.out = 200)
)

tgt_pos <- strat_buy_and_hold_tgt_pos(DT)

eq <- backtest_rcpp(
  timestamp = DT$datetime,
  open = DT$open,
  high = DT$high,
  low = DT$low,
  close = DT$close,
  tgt_pos = tgt_pos,
  pos_strat = rep(1L, nrow(DT)),
  tol_pos = rep(0, nrow(DT)),
  strat = 1L,
  asset = 8001L
)
```

### 2. Convert the latest strategy target into an executable action plan

```r
state <- list(
  ctr_size = 1.0,
  ctr_step = 1.0,
  lev = 10.0,
  last_px = 100.0,
  ctr_unit = 0.0,
  avg_price = NaN,
  cash = 10000.0,
  pos_dir = 0L
)

plan <- strat_buy_and_hold_action_plan(DT, state, value = 1.0)
```

### 3. Turn target portfolio weights into rebalance intents

```r
portfolio_state <- data.table(
  asset = c("BTC", "ETH"),
  price = c(100, 50),
  current_units = c(10, 20),
  target_weight = c(0.6, 0.4)
)

adjustment <- plan_portfolio_adjustment(portfolio_state, equity = 3000)
intents <- build_order_intents(adjustment)
```

## Docs

- [Package Philosophy](docs/philosophy.md)
- [Architecture Notes](docs/architecture.md)

## Current Scope

- feature utilities such as EMA, ATR, and ladder-cycle indexing
- strategy-facing target-position logic
- portfolio-adjustment planning from target weights and current holdings
- action-plan generation from current account state
- order-intent tables for rebalancing workflows
- path-dependent backtesting with execution and funding assumptions

## Author

Oliver Zhou <oliver.yxzhou@gmail.com>

## License

MIT License
