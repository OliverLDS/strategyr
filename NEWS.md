# strategyr 0.1.2.2

- Added native rolling sum and linearly weighted moving-average kernels.
- Added `calc_WMA()`, `calc_ADX()`, `calc_MFI()`, `calc_OBV()`,
  `calc_VWAP()`, and `calc_WPR()`.
- Added TTR parity coverage for the newly added conventional indicators.

# strategyr 0.1.2.1

- Added native rolling max/min kernels in the rolling layer.
- Added `calc_DonchianChannels()` with `dc_high_*`, `dc_low_*`, and
  `dc_mid_*` outputs.
- Added Donchian channel tests and README usage examples.

# strategyr 0.1.2

- Added minimal portfolio-adjustment planning via
  `plan_portfolio_adjustment()`.
- Added minimal rebalance order-intent generation via
  `build_order_intents()`.
- Expanded the README with package usage cases.
- Added package-level philosophy and architecture notes under `docs/`.

# strategyr 0.1.1

- Reframed the package as an execution-oriented strategy workflow engine.
- Added public strategy wrappers for buy-and-hold, EMA-cross, ladder bounce,
  and ladder breakout workflows.
- Exported and documented `calc_ATR()`, `gen_action_plan_rcpp()`, and
  `backtest_rcpp()`.
- Fixed the `calc_ladder_index()` helper to use its explicit datetime input.
- Added generated help pages and portable example scripts.
- Added release hygiene files and removed compiled build artifacts from source
  control.
- Renamed active native source files from `old_*.cpp` to `rcpp_*.cpp`.
