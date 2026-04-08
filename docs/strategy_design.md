# strategyr Strategy Design

This note defines the implementation standard for public `strat_*` functions in
`strategyr`.

## Purpose

`strat_*` is the strategy-decision layer of the package.

Its job is to:

- transform prepared market features and relevant account or portfolio state
  into target exposure
- keep backtest and live execution driven by the same core rule
- remain modular, testable, and execution-oriented

It is not for:

- raw indicator construction
- generic portfolio accounting
- exchange plumbing
- proprietary orchestration scripts

## Core Principle

A mature public strategy in `strategyr` should converge to:

- a native `Rcpp` strategy kernel for the core decision rule
- a thin R wrapper for the package API
- shared native execution and backtest engines downstream

This is important for two reasons:

- path-dependent backtesting needs native performance
- live order placement must follow the same rule as backtesting

The package should avoid any backtest versus live logic drift.

## Required Structure

For a first-class strategy family named `<name>`, use:

1. `R/strat_<name>.R`
2. `src/strat_<name>.h`
3. `src/rcpp_strategies.cpp` or another focused native bridge file
4. `tests/testthat/test-strat_<name>.R`

The R file owns the public contract.
The native kernel owns the reusable core rule.

## Public Function Contract

Every public strategy should expose exactly two top-level entry points:

- `strat_<name>_tgt_pos(...)`
- `strat_<name>_action_plan(DT, state, ...)`

### `strat_<name>_tgt_pos(...)`

This function should:

- return the full historical `tgt_pos`
- be the main entry point for backtesting
- compute missing features internally or require them explicitly
- be vectorized over the full input history

### `strat_<name>_action_plan(DT, state, ...)`

This function should:

- apply the same strategy rule to the latest relevant observation
- return executable action-planning output
- call the shared planner rather than reimplement order logic

## Shared Execution Rule

`strat_<name>_action_plan()` should:

1. obtain strategy target from the same core rule used by
   `strat_<name>_tgt_pos()`
2. extract the latest valid target
3. pass it into `gen_action_plan_rcpp()`

This is mandatory for public `strat_*`.

## Naming Rules

### Files

- `R/strat_<name>.R`
- `src/strat_<name>.h`
- `tests/testthat/test-strat_<name>.R`

### Public R Functions

- `strat_<name>_tgt_pos`
- `strat_<name>_action_plan`

### Internal Helpers

- `.strat_<name>_*`
- shared helpers may live in `R/strat_utils.R`

### Native Wrappers

Use `_rcpp` only for low-level bridge functions.
Do not make `_rcpp` functions the main public package contract unless there is
strong reason to do so.

### Runtime Identifiers

`strat_id` should remain a runtime integer identifier.
It should not become the primary public naming mechanism.

## Separation Of Responsibilities

### `calc_*`

Computes indicators, descriptors, and risk state.

### `strat_*`

Converts those feature and state inputs into target-position logic.

### `plan_*`

Converts exposure gaps or risk gaps into adjustment quantities.

### `gen_action_plan_rcpp()`

Converts target position into executable action planning output.

### `backtest_rcpp()`

Replays target-position history under path-dependent execution assumptions.

This separation should not be blurred.

## When R-Only Is Acceptable

A new strategy may start as R-only if:

- it is experimental
- the rule is still unstable
- it is mostly orchestration over existing features
- historical performance cost is still negligible

Once a strategy is:

- public
- intended for recurring backtests
- intended for live or executable use

it should be promoted toward a native kernel.

## Native Kernel Standard

The native kernel should contain:

- the rule that maps prepared inputs to target-position outputs
- no package-facing R validation
- no README or example logic
- no exchange or account orchestration

The native kernel should not own:

- user-facing defaults documentation
- `data.table` mutation semantics
- external configuration handling

Those responsibilities stay in R wrappers.

## R Wrapper Standard

The R wrapper should own:

- input validation
- feature preparation via `calc_*`
- naming and output shape
- debug mode options
- conversion from latest target to action plan
- package-facing docs and examples

The wrapper is the contract.
The kernel is the engine.

## Testing Standard

Each `strat_*` family should have tests for:

1. target-path behavior
2. action-plan consistency
3. backtest and live alignment
4. edge cases such as insufficient history, missing features, flat regimes, and
   threshold flips

If a native kernel exists, include parity tests between wrapper expectations and
native output.

## Input Design Standard

Market data should be passed as `DT`, a `data.table`.

Execution state should be passed as `state`, a named list or small structured
object.

Required `state` fields should stay consistent with the current planner:

- `ctr_size`
- `ctr_step`
- `lev`
- `last_px`
- `ctr_unit`
- `avg_price`
- `cash`
- `pos_dir`

Optional fields may include:

- `strat_id`
- `tol_pos`

Do not hide required execution inputs inside global variables or file reads.

## Output Design Standard

### `*_tgt_pos()`

- default return: numeric vector of target positions
- debug mode may return richer inspection output

### `*_action_plan()`

- return the planner output from `gen_action_plan_rcpp()`
- extra debug fields are acceptable, but the planner structure should remain
  recognizable

## Promotion Rule For New Strategies

A new strategy should move through this maturity path:

1. idea appears in `calc_*` or a small R prototype
2. provisional implementation lands in `R/strat_<name>.R`
3. if it becomes a stable public strategy, move the core rule into
   `src/strat_<name>.h`
4. keep the R wrapper as the official API

This gives speed without losing package clarity.

## Current Examples In The Repo

Current public examples already follow the paired-wrapper pattern:

- `strat_buy_and_hold_*`
- `strat_ema_cross_*`
- `strat_ladder_bounce_*`
- `strat_ladder_breakout_*`

The buy-and-hold family already includes a native kernel.
That should be the model mature public strategies move toward.

## Repo Rule

For future public strategies:

- prototype in R
- publish with paired `*_tgt_pos` and `*_action_plan`
- once the rule is stable and used in both backtesting and execution, move the
  rule core to `Rcpp`
- keep the R wrapper as the official API
