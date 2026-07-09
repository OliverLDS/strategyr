# PPO Environment Interface

`strategyr` does not provide a native reinforcement-learning environment. PPO
support is an adapter around an external Python environment so model training
can remain optional and separate from the deterministic strategy/action-plan
core.

## Required Python Contract

The environment passed to `train_ppo_policy_py()` should follow the
`gymnasium`/`gym` style interface expected by `stable_baselines3.PPO`:

- `reset()` returns an initial observation, or `(observation, info)` for
  `gymnasium`.
- `step(action)` advances one time step and returns the standard Python tuple.
- `observation_space` describes the numeric feature vector or tensor.
- `action_space` describes the discrete action ids consumed by the policy.
- observations must contain only information available at the decision time.

The default `strategyr` action map is zero-based:

```text
0 -> short
1 -> flat
2 -> long
```

If a custom environment uses a different action encoding, pass an explicit
`action_map` to `strat_ppo_policy_tgt_pos()`.

## Recommended Observation Fields

A single-instrument environment should usually include normalized, lagged, or
already-computed features rather than raw future-dependent quantities. Common
inputs include:

- recent returns or log returns
- trend and momentum features
- volatility and drawdown state
- current position or exposure
- cash/equity state if the policy controls sizing
- execution-cost or spread state when available

The environment should not expose future bars, future target returns, or
post-trade realized PnL before the action is chosen.

## Reward Boundary

Reward design is owned by the Python environment. A practical reward can start
from net equity change after fees and funding, but should be consistent with
the execution assumptions used by `backtest_rcpp()` or
`backtest_portfolio_weights()`.

Do not move order execution or account accounting into `strat_ppo_policy_*()`.
The strategy wrapper only maps policy actions into target positions. Execution
conversion remains centralized in the existing action-plan and backtest layers.

## Backtest Compatibility

After a PPO model is trained, `strat_ppo_policy_tgt_pos()` must return one
numeric target position per market row:

```text
market/features table
  -> trained policy predict()
  -> discrete action ids
  -> numeric target positions
  -> backtest_rcpp() or strat_ppo_policy_action_plan()
```

For portfolio-level PPO, use the same principle but return target weights in a
panel compatible with portfolio allocation and backtesting helpers. Do not add
portfolio PPO wrappers until the target-weight contract is explicit for the
specific environment.
