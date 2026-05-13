library(data.table)
library(strategyr)

if (!requireNamespace("investdatar", quietly = TRUE)) {
  stop("Package `investdatar` is required for this real-data example.")
}

ticker <- "SPY"
from_date <- as.Date("2000-01-01")
to_date <- Sys.Date()

DT <- data.table::as.data.table(
  investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo")
)
DT <- DT[
  datetime >= as.POSIXct(from_date) &
    datetime < as.POSIXct(to_date + 1)
][order(datetime)]
DT <- DT[
  is.finite(open) & is.finite(high) & is.finite(low) & is.finite(close) &
    open > 0 & high > 0 & low > 0 & close > 0
]

param_grid <- data.table::CJ(
  fast = c(10L, 20L, 30L),
  mid = c(40L, 50L, 75L),
  slow = c(100L, 150L, 200L),
  target_size = 0.95
)[fast < mid & mid < slow]

walk_res <- strategyr::mine_strategy_walk_forward(
  DT,
  strategy_fun = strategyr::strat_ema_triple_trend_tgt_pos,
  param_grid = param_grid,
  train_years = 5,
  test_years = 1,
  step_years = 1,
  n_best = 3L,
  from = from_date,
  to = to_date,
  min_train_rows = 1000L,
  min_test_rows = 200L,
  warmup_days = 365L,
  strat_id = 106L,
  asset_id = 8001L,
  ctr_size = 0.01,
  ctr_step = 0.01,
  lev = 1.0,
  fee_rt = 0.0007,
  tol_pos = 0.1,
  keep_paths = FALSE,
  annualization = 252
)

print(walk_res$windows)
print(walk_res$train_results[, .(
  window_id, train_rank, fast, mid, slow, sortino, total_return, max_drawdown
)])
print(walk_res$test_results[seq_len(min(.N, 20L)), .(
  rank, window_id, train_rank, fast, mid, slow, sortino, sharpe,
  total_return, max_drawdown, exposure, turnover, trade_count,
  warmup_n_obs, warmup_insufficient
)])
