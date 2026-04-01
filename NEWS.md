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
