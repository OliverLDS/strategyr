library(data.table)
library(strategyr)

if (!requireNamespace("investdatar", quietly = TRUE)) {
  stop("Package `investdatar` is required for this real-data example.")
}

from_date <- as.Date("2000-01-01")
to_date <- Sys.Date()
ticker <- "SPY"

underlying_dt <- data.table::as.data.table(investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo"))
underlying_dt <- underlying_dt[
  datetime >= as.POSIXct(from_date) &
    datetime < as.POSIXct(to_date + 1) &
    is.finite(close) & close > 0
][order(datetime)]
if (nrow(underlying_dt) < 1000L) {
  stop("Need enough locally cached Yahoo OHLC rows for the selected ticker.")
}

# Yahoo OHLC does not provide option-chain IV history through this local path.
# Build a deterministic proxy option surface from realized underlying movement
# so the example exercises the option strategy and mining workflow.
underlying_dt[, date := as.Date(datetime)]
underlying_dt[, log_ret := c(NA_real_, diff(log(close)))]
underlying_dt[, rv_20 := sqrt(252) * rolling_sd(log_ret, 20L, sample = FALSE)]
underlying_dt[is.na(rv_20), rv_20 := median(rv_20, na.rm = TRUE)]

surface_dt <- rbindlist(list(
  underlying_dt[, .(
    date,
    T = 30 / 365,
    type = "put",
    option_log_forward_moneyness = -0.1,
    iv = pmax(0.05, rv_20 + 0.03 + 0.01 * sin(seq_len(.N) / 20)),
    close
  )],
  underlying_dt[, .(
    date,
    T = 30 / 365,
    type = "call",
    option_log_forward_moneyness = 0.1,
    iv = pmax(0.05, rv_20 - 0.01 + 0.01 * cos(seq_len(.N) / 20)),
    close
  )]
), use.names = TRUE)

param_grid <- data.table::CJ(
  trend_n = c(30L, 50L, 100L),
  skew_long_threshold = c(0.02, 0.03, 0.04),
  skew_short_threshold = c(-0.02, -0.03, -0.04),
  overlay_mode = c("confirm", "flip"),
  target_size = 0.95
)

market_dt <- underlying_dt[, .(
  datetime,
  open,
  high,
  low,
  close
)]

mining_res <- strategyr::mine_strategy_walk_forward(
  DT = market_dt,
  strategy_fun = function(DT, trend_n, skew_long_threshold, skew_short_threshold, overlay_mode, target_size) {
    dates <- as.Date(DT$datetime)
    signal_surface <- surface_dt[date %in% dates]
    dbg <- strategyr::strat_iv_directional_overlay_tgt_pos(
      signal_surface,
      trend_n = trend_n,
      skew_long_threshold = skew_long_threshold,
      skew_short_threshold = skew_short_threshold,
      overlay_mode = overlay_mode,
      target_size = target_size,
      debug = TRUE
    )
    summary_dt <- data.table::copy(dbg$data)
    summary_dt[, tgt_pos := dbg$tgt_pos]
    out <- summary_dt[match(dates, date), tgt_pos]
    data.table::fifelse(is.na(out), 0, out)
  },
  param_grid = param_grid,
  train_years = 5,
  test_years = 1,
  step_years = 1,
  n_best = 2L,
  from = from_date,
  to = to_date,
  min_train_rows = 1000L,
  min_test_rows = 200L,
  warmup_years = 1,
  strat_id = 710L,
  asset_id = 8301L,
  ctr_size = 0.01,
  ctr_step = 0.01,
  lev = 1.0,
  fee_rt = 0.0007,
  tol_pos = 0.1
)

print(strategyr::summarize_walk_forward_results(mining_res, group_cols = c(
  "trend_n", "skew_long_threshold", "skew_short_threshold", "overlay_mode"
)))
print(strategyr::select_strategy_params(
  mining_res,
  n = 5L,
  param_cols = c("trend_n", "skew_long_threshold", "skew_short_threshold", "overlay_mode", "target_size"),
  include_metrics = TRUE
))
