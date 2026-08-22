# Extension Boundaries

`strategyr` is the deterministic CRAN core. It accepts already-available
market features and state, then produces target positions, target weights, and
action plans through a shared backtest and execution architecture.

## Future LSTM Companion

A future `strategyr.ml` companion package may provide LSTM forecasting after
the `torch` workflow has stable cross-platform checks. It should depend on
`strategyr`, own all model-training dependencies, and return forecast columns
or standard `numeric(nrow(DT))` target-position vectors. It must not duplicate
the backtest or action-plan engines.

## Future PPO GitHub Extension

A future GitHub-only `strategyr-ppo` extension may provide Python PPO support.
It should own `reticulate`, Python environment discovery, and
`stable_baselines3` integration. Its policy adapter must convert decision-time
observations into a numeric target-position vector or a target-weight panel
compatible with `strategyr`.

The extension environment must not expose future data. Execution, fees,
funding, portfolio accounting, and order conversion remain in `strategyr`.

## Compatibility Contract

Extensions may create forecasts, actions, or scores, but their public strategy
outputs must use one of these core contracts:

- a numeric target-position vector with one value per market row;
- a panel of target weights compatible with portfolio helpers; or
- an action plan produced through the shared planner helpers.

This preserves the same target-generation rule for backtesting and live order
planning.
