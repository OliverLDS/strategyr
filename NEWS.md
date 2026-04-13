# strategyr 0.1.3.5

- Updated asset-year strategy mining to compute strategy signals on a warmup
  window before each evaluation year, then backtest only the trade-year slice.
- Added warmup metadata to asset-year mining results so signal and evaluation
  windows are reproducible.
- Updated real-data mining examples to show warmup-aware asset-year results.

# strategyr 0.1.3.4

- Added an asset-year strategy-mining helper that seeds candidate parameter
  rows from selected assets, evaluates valid asset-year pairs, filters for
  strategy total return above buy-and-hold, and ranks survivors by Sortino.
- Updated log-return RSI reversion defaults and the real-data mining example
  away from classic `30/70` RSI thresholds.

# strategyr 0.1.3.3

- Added strategy-mining helpers for Sortino-ranked parameter grids and
  fixed-parameter asset sweeps.
- Added `strat_macd_contrarian_tgt_pos()` and
  `strat_macd_contrarian_action_plan()` for the inverse MACD-cross rule.
- Added `strat_rsi_logr_revert_tgt_pos()` and
  `strat_rsi_logr_revert_action_plan()` for reversion rules based on
  strategyr's log-return RSI feature.
- Fixed `calc_ADX()` smoothing so ADX directional indicators match `TTR::ADX()`
  on the shared non-NA region.

# strategyr 0.1.3.1

- Fixed native backtest recorder handling so `rec = FALSE` no longer forms a
  reference from a null recorder pointer.

# strategyr 0.1.3

- Added a broader public `strat_*` layer covering Bollinger, RSI, Donchian,
  ATR, MACD, volatility-targeting, and trend-pullback strategy families.
- Added cross-asset public strategy families for pair-spread reversion, ratio
  reversion, and relative-strength workflows.
- Added asset-class-specific public strategy families for FX carry, bond
  carry-and-roll, curve steepener, and futures roll-yield workflows.
- Added option strategy-proxy families for IV skew, IV term structure,
  straddle, strangle, and vertical-spread workflows.
- Added contributor documentation for `strat_*` implementation standards and
  updated package-facing docs to reflect the expanded strategy surface.

# strategyr 0.1.2.6

- Added statistical, breadth, and cross-asset relative-value feature layers.
- Added execution and microstructure feature descriptors.
- Added futures curve, FX carry/basis, option-surface, and credit-spread
  feature layers.
- Expanded README and architecture notes to reflect the broader `calc_*`
  feature surface.

# strategyr 0.1.2.5

- Added an option-risk analytics layer with Black-Scholes Greeks and implied
  volatility helpers.
- Added option portfolio helpers for position Greek aggregation, option
  risk-state snapshots, and delta/vega hedge-adjustment planning.
- Expanded README and architecture notes to include the option-risk workflow.

# strategyr 0.1.2.4

- Added a fixed-income analytics layer covering bond cash flows, pricing,
  yield, duration, convexity, DV01/PV01, z-spread, carry/roll, curve shocks,
  and key-rate risk.
- Added fixed-income convention helpers for day-count fractions, coupon
  schedules, and previous/next coupon dates.
- Added fixed-income strategy-facing helpers for bond risk-state snapshots and
  duration/curve hedge adjustment planning.
- Expanded package docs and README usage examples to reflect the fixed-income
  workflow surface.

# strategyr 0.1.2.3

- Added `calc_DEMA()`, `calc_ZLEMA()`, `calc_HMA()`, `calc_CMO()`,
  `calc_TRIX()`, `calc_KST()`, and `calc_ultimateOscillator()`.
- Added `calc_SMI()`, `calc_chaikinAD()`, `calc_chaikinVolatility()`, and
  `calc_EMV()`.
- Added `calc_CMF()`, `calc_VWMA()`, `calc_aroon()`, and `calc_SAR()`.
- Expanded TTR parity coverage for the newly added indicator families.

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
