# strategyr API Consistency Audit

This note records the current public API conventions for strategy functions and
the known inconsistencies that should be handled carefully in future releases.
It is a documentation audit, not a breaking-change plan.

## Public Strategy Contract

The public strategy layer is currently consistent on the core shape:

- strategy files live under `R/strat_<name>.R`
- single-instrument strategy families expose `strat_<name>_tgt_pos()`
- single-instrument strategy families expose `strat_<name>_action_plan()`
- `*_tgt_pos()` returns a numeric vector by default
- `*_action_plan()` converts the latest target into executable planning output
- most action-plan functions accept `strat_id`, `tol_pos`, and `debug`

Portfolio-level strategy families intentionally differ:

- `strat_cross_sectional_rank_allocator_*`
- `strat_roll_yield_cross_sectional_*`
- `strat_fx_carry_basket_rank_*`

Those functions work with target weights and use `portfolio_state`, `equity`,
`plan_portfolio_adjustment()`, and `build_order_intents()`.

## Naming Conventions That Are Stable

| Concept | Current convention | Notes |
|---|---|---|
| Market input | `DT` | Always a `data.table`-style market or panel input |
| Single-instrument execution state | `state` | Passed to `.action_plan_from_tgt_pos()` |
| Portfolio execution state | `portfolio_state`, `equity` | Used for portfolio adjustment workflows |
| Absolute target exposure | `target_size` | Dominant convention for one-instrument target rules |
| Portfolio exposure | `gross_exposure` | Used by rank allocators |
| Feature generation flag | `compute_features` | Dominant convention for `calc_*` preparation |
| Debug output flag | `debug` | Present across public strategy families |
| Column arguments | `*_col` | Used for flexible input schema |
| Rolling windows | `*_n`, `n`, or named windows | See inconsistencies below |
| Z-score thresholds | `long_z`, `short_z`, `entry_z`, `exit_z` | Family-specific but internally coherent |
| Threshold filters | `long_threshold`, `short_threshold` | Dominant convention for threshold signals |

## Intentional Exceptions

| Exception | Reason |
|---|---|
| `strat_buy_and_hold_*` has no `compute_features` | It does not need features |
| Ladder strategies use `compute_ladder` | They depend on ladder-cycle construction, not generic indicator features |
| Portfolio rank strategies omit `state` and `tol_pos` | They produce portfolio adjustment tables, not one-instrument market-order plans |
| Summarized panel strategies may return `data` in debug mode | Futures curves and option chains are summarized before target generation |
| `strat_ema_cross_*` includes ATR freshness and TP/SL parameters | It predates the later minimal trend-wrapper pattern and encodes more execution-oriented logic |
| `strat_vol_target_*` uses `vol_target` and `max_leverage` instead of `target_size` | Position size is the strategy output, not a fixed target exposure |

## Inconsistencies To Watch

### Window names

The package uses both generic and semantic window names:

- `n` for compact single-indicator rules
- `rsi_n`, `adx_n`, `rv_n`, `z_n`, `trend_n`, `mom_n` for multi-feature rules
- `fast`, `mid`, `slow`, `signal` for EMA/MACD-style rules
- `entry_n`, `exit_n` for Turtle-style channel rules

Recommendation: keep existing names for backward compatibility. For new
strategies, prefer semantic names when more than one window is present.

### Feature-computation flags

Most strategies use `compute_features`. Ladder strategies use `compute_ladder`.

Recommendation: do not rename `compute_ladder`; document it as a domain-specific
variant. New non-ladder strategies should use `compute_features`.

### Threshold semantics

Threshold arguments are family-specific:

- `long_threshold` and `short_threshold` usually mean direct threshold signals
- `entry_z` and `exit_z` usually mean stateful reversion bands
- `long_z` and `short_z` usually mean signed z-score activation levels
- `oversold`, `overbought`, and `exit_level` are RSI-family conventions

Recommendation: avoid forcing all thresholds into one naming style. The current
names are more readable when interpreted within their signal family.

### Debug return shapes

Current debug modes commonly return one of these shapes:

- `list(tgt_pos, feature_cols)` for ordinary single-instrument strategies
- `list(tgt_pos, data, feature_col)` for summarized panel strategies
- action-plan debug output with `plan` and `latest_tgt_pos`
- portfolio action-plan debug output with target weights and order tables

Recommendation: preserve the family-specific shapes, but document them in each
Rd page and in `docs/strategy_catalog.md`.

### Target units

Most single-instrument strategies emit target position units via `target_size`.
Portfolio strategies emit target weights via `gross_exposure`. Vol-target
strategies emit volatility-scaled exposure.

Recommendation: keep the distinction explicit. Do not overload
`target_size` for portfolio weights.

## Suggested Non-Breaking Cleanup

1. Add a machine-readable strategy registry.

   A registry table could live in `R/strategy_registry.R` or
   `inst/extdata/strategy_registry.csv` and include family name, category,
   level, `strat_id`, target function, action-plan function, and default input
   type.

2. Add API consistency tests.

   Tests can verify that every public `strat_*_tgt_pos()` has a matching
   `strat_*_action_plan()` except documented aliases such as
   `strat_macd_contrarian_*` living in `R/strat_macd_cross.R`.

3. Add standard debug-shape tests by level.

   Single-instrument, summarized-panel, and portfolio-level strategy families
   can each have a small shared expectation helper.

4. Keep old argument names stable.

   If a clearer alias is useful, add it as an additive argument with explicit
   precedence rules rather than renaming existing parameters.

5. Promote high-use R-only signal loops to native kernels.

   Good candidates are stateful or path-dependent rules such as Donchian
   Turtle, ATR trailing stop, RSI reversion, pair-spread reversion, and RSI
   divergence.

## Backward-Compatibility Rule

Before a `0.2.0` release, prefer additive API improvements over renames.
Document exceptions, add aliases if necessary, and reserve breaking cleanup for
a clearly announced version boundary.
