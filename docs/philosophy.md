# strategyr Philosophy

`strategyr` is not intended to be a generic indicator library or a notebook-first
research package.

Its purpose is to turn already-available quantitative outputs and portfolio
state into decisions that can survive contact with execution constraints. In
practice, that means three things:

1. Strategy logic matters more than isolated indicators.
2. Path dependency matters because account state, fills, funding, and existing
   positions affect what the next valid action actually is.
3. The same strategy rule should remain relevant across backtesting and live
   execution.

This is the main difference from `investlabr`. `investlabr` is better suited to
exploration, communication, and broad research workflows. `strategyr` is for
disciplined rule definition and timely investment decision support.

## Design Principles

### 1. Separate analysis from execution, but connect them tightly

`strategyr` assumes that useful market features or allocation outputs may come
from richer external analysis. The package does not try to own all of that
research. Instead, it focuses on the bridge from:

- market features
- account and portfolio state
- target allocations or exposures

to:

- target positions
- portfolio adjustments
- order intents

### 2. Keep strategy rules reusable

A strategy should not exist only as a backtest artifact. Public strategy wrappers
such as buy-and-hold, EMA-cross, ladder bounce, and ladder breakout are exposed
so the same rule can be used to:

- generate a historical target-position path
- translate the latest state into an execution plan

### 3. Treat execution assumptions as first-class

Backtesting is only relevant if it respects how decisions are turned into
actions. The package therefore keeps explicit structures for:

- target positions
- action planning
- fills at the next bar open
- fees and funding
- recorder output for auditability

### 4. Prefer minimal but composable public building blocks

The GitHub version of `strategyr` should expose generic and educational strategy
components, while keeping proprietary strategy composition outside the package.
That is why ladder/cycle primitives are public, but the public ladder strategies
remain intentionally naive.
