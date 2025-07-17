# strategyr

**strategyr** is an R package containing modular and reusable trading strategies designed for quantitative research, signal generation, and real-time decision support. Each strategy is implemented with clarity, parameterization, and plug-and-play components, supporting both backtesting and live execution workflows.

---

## Features

* **Modular Strategy Design:** Each strategy is implemented as a self-contained function using clearly defined inputs and outputs.
* **Layered Breakout Logic:** Fine-grained breakout signals with configurable confidence levels and position sizing.
* **Zone-Based Reversal Logic:** Detect entries near support/resistance using quantile, pivot, and momentum-aware zones.
* **SL/TP Rules:** Take-profit and stop-loss management based on PnL ratio or volatility-based thresholds.
* **Technical Indicator Utilities:** Calculates standard TA signals and dynamic market structure metrics.
* **ARIMAX Forecast Fan (Optional):** Probabilistic short-term return prediction using volume as exogenous input.

---

## Strategies

### `breakout_v1()`

Implements a 4-layer breakout strategy:

* Layer 1: ATR rising + EMA crossover + price breakout
* Layer 2: EMA crossover + price breakout
* Layer 3: ATR rising + EMA crossover
* Layer 4: EMA crossover only

Each layer has configurable position size multipliers.

### `zone_sniper_v1()`

Fade price near support/resistance zones:

* Uses quantile-based and pivot-based zones
* Filters entries with RSI oversold/overbought
* ATR-scaled tolerance ensures volatility-awareness

### `generate_tp_sl_orders()`

Manages exits based on:

* Unrealized PnL ratio
* Price movement vs ATR-scaled threshold
* Distinct reasons labeled as "TP" or "SL"

---

## Utilities

### `.calc_zones_quantile_fixed()` / `.calc_zones_quantile_dyn()`

* Quantile-based zone calculation using fixed or dynamic windows

### `.calc_zones_pivots()`

* Pivot-based zone detection with clustering and scoring

### `.add_arimax_fan()`

* ARIMAX prediction with volume as exogenous input, outputs forecast mean and quantiles

### `calculate_technical_indicators()`

* Computes a wide range of indicators, including:

  * EMA/SMA, RSI, ATR, Bollinger Bands
  * Trend flags, squeeze flags, volatility ratios
  * Dynamic SL/TP bands
  * (Optional) zone levels and ARIMA forecast

---

## Installation

```r
# Coming soon: install via GitHub
# devtools::install_github("OliverLDS/strategyr")
```

---

## Usage

```r
# Load market data frame
market_data <- read.csv("eth_4h.csv")

# Calculate indicators and zones
market_data <- calculate_technical_indicators(market_data, if_calculate_pivot_zone = TRUE)

# Run strategy on latest row
signal <- breakout_v1(trade_state, market_data[nrow(market_data), ], trade_pars)
```

---

## License

MIT License. Feel free to modify, extend, and adapt.

---

## Author

Created and maintained by Oliver Lee.
