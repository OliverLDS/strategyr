# strategyr

`strategyr` is an execution-oriented R package for modular strategy workflows. It is designed to transform already-available market features, portfolio state, and dynamic quantitative analysis into actionable signals, target positions, portfolio adjustments, and order intents.

The package emphasizes path-dependent backtesting so strategy logic is evaluated under evolving account state, execution assumptions, and market conditions rather than through purely static signal research.

Compared with `investlabr`, which is oriented toward exploratory research and communication, `strategyr` is intended for disciplined strategy logic and timely investment decision support.


## Installation

```r
devtools::install_github("OliverLDS/strategyr")
```

## Current Scope

- feature utilities such as EMA, ATR, and ladder-cycle indexing
- strategy-facing target-position logic
- action-plan generation from current account state
- path-dependent backtesting with execution and funding assumptions

## Author

Oliver Zhou <oliver.yxzhou@gmail.com>

## License

MIT License
