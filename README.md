# strategyr

`strategyr` is an execution-oriented R package for modular strategy workflows. It is designed to transform already-available market features, portfolio state, and dynamic quantitative analysis into actionable signals, target positions, portfolio adjustments, and order intents.

The package emphasizes path-dependent backtesting so strategy logic is evaluated under evolving account state, execution assumptions, and market conditions rather than through purely static signal research.

Compared with `investlabr`, which is oriented toward exploratory research and communication, `strategyr` is intended for disciplined strategy logic and timely investment decision support.

## Installation

```r
devtools::install_github("OliverLDS/strategyr")
```

## Usage Cases

### 1. Single-asset strategy backtest

```r
library(data.table)
library(strategyr)

DT <- data.table(
  datetime = as.POSIXct("2020-01-01", tz = "UTC") + 86400 * 0:249
)
DT[, close := 100 + cumsum(sin(seq_len(.N) / 8) + 0.15)]
DT[, open := shift(close, fill = close[1])]
DT[, high := pmax(open, close) + 1]
DT[, low := pmin(open, close) - 1]

tgt_pos <- strat_donchian_turtle_tgt_pos(
  DT,
  entry_n = 30L,
  exit_n = 10L,
  target_size = 0.95
)

eq <- backtest_rcpp(
  timestamp = as.numeric(DT$datetime),
  open = DT$open,
  high = DT$high,
  low = DT$low,
  close = DT$close,
  tgt_pos = tgt_pos,
  pos_strat = rep(307L, nrow(DT)),
  tol_pos = rep(0.05, nrow(DT)),
  strat = 307L,
  asset = 8001L,
  ctr_step = 0.01,
  lev = 1,
  fee_rt = 0.0005,
  rec = TRUE
)

calc_strategy_performance_summary(
  eq,
  tgt_pos = tgt_pos,
  recorder = attr(eq, "recorder"),
  fee_rt = 0.0005
)
```

### 2. Latest-state action plan

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

plan <- strat_donchian_turtle_action_plan(
  DT,
  state,
  entry_n = 30L,
  exit_n = 10L,
  target_size = 0.95,
  strat_id = 307L
)
```

### 3. Warmup-aware strategy mining

```r
walk_res <- mine_strategy_walk_forward(
  DT,
  strategy_fun = strat_ema_triple_trend_tgt_pos,
  param_grid = data.table::CJ(
    fast = c(10L, 20L),
    mid = c(30L, 50L),
    slow = c(80L, 120L),
    target_size = 0.95
  )[fast < mid & mid < slow],
  train_years = 1,
  test_years = 0.5,
  step_years = 0.5,
  n_best = 2L,
  min_train_rows = 100L,
  min_test_rows = 50L,
  warmup_days = 120L,
  strat_id = 106L,
  ctr_step = 0.01,
  lev = 1,
  fee_rt = 0.0005
)

summarize_walk_forward_results(walk_res, group_cols = c("fast", "mid", "slow"))
filter_walk_forward_results(
  walk_res,
  group_cols = c("fast", "mid", "slow"),
  min_windows = 1L,
  min_positive_return_rate = 0.5,
  max_avg_score_decay = 10
)
```

### 4. Cross-sectional allocator and portfolio backtest

```r
panel <- CJ(
  date = as.Date("2020-01-01") + 0:9,
  asset = c("AAA", "BBB", "CCC")
)
panel[, score := fifelse(asset == "AAA", 0.8, fifelse(asset == "BBB", 0.4, 0.2)) + as.numeric(date - min(date)) / 100]
panel[, open := 100 + match(asset, c("AAA", "BBB", "CCC")) * 5 + as.numeric(date - min(date))]
panel[, close := open * (1 + score / 100)]
panel[, target_weight := strat_cross_sectional_rank_allocator_tgt_pos(
  panel,
  date_col = "date",
  asset_col = "asset",
  signal_col = "score",
  long_n = 2L,
  short_n = 0L,
  gross_exposure = 1.0
)]

portfolio_bt <- backtest_portfolio_weights(
  panel,
  initial_equity = 100000,
  fee_rt = 0.0005
)

portfolio_bt$equity[, .(date, equity, gross_exposure, turnover, fee_paid)]
```

### 5. Fixed-income carry/roll and hedge workflow

```r
bond_dt <- data.table(
  par = rep(100, 5),
  c_rate = rep(0.05, 5),
  T = seq(2, 6),
  freq = rep(2, 5),
  ytm = c(0.045, 0.044, 0.043, 0.042, 0.041)
)

carry_tgt <- strat_bond_carry_roll_tgt_pos(
  bond_dt,
  long_threshold = 0,
  short_threshold = -0.02,
  target_size = 0.5
)

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

### 6. Option and volatility workflow

```r
option_chain <- CJ(
  date = as.Date("2020-01-01") + 0:79,
  T = c(30, 60) / 365,
  type = c("put", "call")
)
option_chain[, option_log_forward_moneyness := fifelse(type == "put", -0.1, 0.1)]
option_chain[, close := 100 + as.numeric(date - min(date)) * 0.1]
option_chain[, iv := 0.2 + fifelse(type == "put", 0.03, -0.01) + sin(seq_len(.N) / 20) / 100]

iv_tgt <- strat_iv_directional_overlay_tgt_pos(
  option_chain,
  trend_n = 20L,
  skew_long_threshold = 0.02,
  skew_short_threshold = -0.02,
  overlay_mode = "confirm"
)

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
- [Strategy Design Notes](docs/strategy_design.md)
- [Strategy Catalog](docs/strategy_catalog.md)
- [API Consistency Audit](docs/api_consistency.md)

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
- public strategy families covering buy-and-hold, EMA/MACD crossover and MACD
  contrarian, Bollinger reversion, classic and log-return RSI reversion,
  Donchian and ATR breakout, volatility
  targeting, trend pullback, pair-spread and ratio reversion, relative
  strength, FX carry, bond carry-and-roll, curve steepener, roll yield, IV
  skew, IV term structure, and options structure-proxy workflows
- portfolio-adjustment planning from target weights and current holdings
- portfolio target-weight backtesting with open-price rebalancing and
  close-price mark-to-market, backed by a native accounting core for the
  standard no-position-path case
- fixed-income risk-state snapshots and hedge-adjustment helpers
- option risk-state snapshots and hedge-adjustment helpers
- action-plan generation from current account state
- order-intent tables for rebalancing workflows
- path-dependent backtesting with execution and funding assumptions
- strategy-mining helpers for Sortino-ranked parameter sweeps,
  fixed-parameter asset sweeps, asset-year mining with buy-and-hold filtering,
  walk-forward train/test mining, reusable parameter selection, and
  walk-forward diagnostics and anti-overfit filtering
- native signal kernels for selected high-use path-dependent strategies while
  preserving R wrappers as the public API

## Author

Oliver Zhou <oliver.yxzhou@gmail.com>

## License

MIT License
