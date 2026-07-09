# Optional ML/RL Workflows

`strategyr` keeps machine-learning and reinforcement-learning support
experimental and optional. The core package remains a deterministic strategy
engine: market features and model outputs must still become target positions,
target weights, or action plans through the same public interfaces used by
rule-based strategies.

## Dependency Boundary

- `torch` is listed in `Suggests` for experimental LSTM forecasting.
- `reticulate` is listed in `Suggests` for Python PPO adapters.
- Python RL libraries, such as `stable-baselines3` and `gymnasium`, are not R
  package dependencies. Users own the Python environment.
- Package tests avoid requiring `torch` or a working Python PPO environment.

## LSTM Forecast Path

The LSTM path is intentionally narrow:

```text
numeric market/features table
  -> calc_lstm_forecast()
  -> forecast column
  -> strat_lstm_forecast_tgt_pos()
  -> backtest_rcpp() or strat_lstm_forecast_action_plan()
```

`calc_lstm_forecast()` trains a small `torch` LSTM regressor and returns a
forecast vector aligned with the input rows. `strat_lstm_forecast_tgt_pos()`
converts forecast return into long, short, or flat target exposure.

For production research, prefer precomputing and storing model forecasts outside
the live action path, then calling `strat_lstm_forecast_tgt_pos()` with
`compute_features = FALSE`. This keeps live execution deterministic and avoids
retraining during order generation.

## PPO Adapter Path

The PPO path is an adapter, not a native RL framework:

```text
Python gym/gymnasium environment
  -> train_ppo_policy_py()
  -> Python PPO model
  -> strat_ppo_policy_tgt_pos()
  -> backtest_rcpp() or strat_ppo_policy_action_plan()
```

`train_ppo_policy_py()` imports Python `stable_baselines3` through
`reticulate`. `strat_ppo_policy_tgt_pos()` accepts either a Python object with a
`predict()` method or an R function that returns discrete actions.

The default action map is zero-based:

```text
0 -> short
1 -> flat
2 -> long
```

This can be changed through `action_map`, but the output must remain a numeric
target-position vector with one value per input row.

See `docs/ppo_environment.md` for the expected Python environment interface
before adding new PPO training helpers.

## Architecture Rule

ML/RL code should not bypass the package execution architecture. Models may
create forecasts, classifications, actions, or scores, but public strategy
functions must still return:

- `numeric(nrow(DT))` for single-instrument target positions.
- A panel of target weights for portfolio-level allocators.
- An action plan only through shared planner wrappers such as
  `.action_plan_from_tgt_pos()` or `plan_portfolio_adjustment()`.

This preserves the package principle that backtesting and executable order
planning use the same strategy output.
