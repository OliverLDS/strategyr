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

### 4. Add Donchian channel features for breakout or range logic

```r
DT <- data.table(
  high = c(10, 11, 12, 13, 14, 13),
  low = c(5, 6, 7, 8, 9, 8)
)

calc_DonchianChannels(DT, ns = c(3, 5))

DT[, .(dc_high_3, dc_low_3, dc_mid_3, dc_high_5, dc_low_5)]
```

### 5. Build a bond risk-state snapshot and a duration hedge plan

```r
bond_state <- calc_bond_risk_state(
  par = 100,
  c_rate = 0.06,
  T = 3,
  freq = 2,
  ytm = 0.05,
  accrual_frac = 0.25
)

dv01_plan <- plan_duration_neutral_adjustment(
  current_dv01 = bond_state$dv01 * 1000,
  target_dv01 = 0,
  hedge_dv01 = -125
)
```

### 6. Build an option risk-state snapshot and a delta hedge plan

```r
option_state <- calc_option_risk_state(
  S = 100,
  K = 100,
  T = 30 / 365,
  r = 0.04,
  sigma = 0.25,
  type = "call"
)

delta_plan <- plan_delta_neutral_adjustment(
  current_delta = option_state$delta * 100,
  target_delta = 0,
  hedge_delta = -50
)
```

## Docs

- [Package Philosophy](docs/philosophy.md)
- [Architecture Notes](docs/architecture.md)

## Current Scope

- feature utilities such as SMA, EMA, WMA, DEMA, ZLEMA, HMA, RSI, SMI, CMO,
  ROC, MACD, TRIX, KST, ultimate oscillator, ADX, Aroon, ATR, Bollinger
  bands, Keltner channels, CCI, stochastic oscillator, MFI, OBV, VWAP, VWMA,
  CMF, Williams %R, EMV, Chaikin AD, Chaikin volatility, Donchian channels,
  ladder-cycle indexing, rolling statistical features, breadth indicators,
  relative-value descriptors, execution and microstructure proxies, futures
  curve features, FX carry and basis features, and option-surface summaries
- fixed-income analytics such as bond cash flows, yield, clean and dirty
  pricing, duration, convexity, DV01/PV01, z-spread, carry and roll-down,
  curve shocks, key-rate duration, and coupon/convention helpers
- option-risk analytics such as Black-Scholes Greeks, implied volatility,
  position Greek aggregation, option risk-state snapshots, and delta/vega
  hedge-adjustment helpers
- credit and spread features such as credit spread, excess spread, and
  spread-curve slope and butterfly descriptors
- strategy-facing target-position logic
- portfolio-adjustment planning from target weights and current holdings
- fixed-income risk-state snapshots and hedge-adjustment helpers
- option risk-state snapshots and hedge-adjustment helpers
- action-plan generation from current account state
- order-intent tables for rebalancing workflows
- path-dependent backtesting with execution and funding assumptions

## Author

Oliver Zhou <oliver.yxzhou@gmail.com>

## License

MIT License
