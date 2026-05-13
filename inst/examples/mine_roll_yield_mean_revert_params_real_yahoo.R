library(data.table)
library(strategyr)

if (!requireNamespace("investdatar", quietly = TRUE)) {
  stop("Package `investdatar` is required for this real-data example.")
}

from_date <- as.Date("2000-01-01")
to_date <- Sys.Date()

# Yahoo OHLC does not expose a full contract curve through the local quantmod
# cache. This example uses two liquid commodity futures proxies as a minimal
# front/deferred curve-shaped panel so the mining workflow remains executable.
front_ticker <- "CL=F"
deferred_ticker <- "BZ=F"

load_local_yahoo <- function(symbol, min_rows = 260L) {
  out <- tryCatch(
    data.table::as.data.table(investdatar::get_local_quantmod_OHLC(symbol, src = "yahoo")),
    error = function(e) NULL
  )
  if (is.null(out) || nrow(out) == 0L || !"datetime" %in% names(out)) {
    return(NULL)
  }
  out <- out[datetime >= as.POSIXct(from_date) & datetime < as.POSIXct(to_date + 1)][order(datetime)]
  out <- out[is.finite(close) & close > 0]
  if (nrow(out) < min_rows) {
    return(NULL)
  }
  out[, .(date = as.Date(datetime), close)]
}

front_dt <- load_local_yahoo(front_ticker)
deferred_dt <- load_local_yahoo(deferred_ticker)
if (is.null(front_dt) || is.null(deferred_dt)) {
  stop("Need locally cached Yahoo data for both futures proxy tickers.")
}

curve_dt <- rbindlist(list(
  front_dt[, .(date, contract_rank = 1L, close, time_to_expiry = 30 / 365)],
  deferred_dt[, .(date, contract_rank = 2L, close, time_to_expiry = 60 / 365)]
), use.names = TRUE)
curve_dt <- curve_dt[date %in% Reduce(intersect, list(front_dt$date, deferred_dt$date))]
setorder(curve_dt, date, contract_rank)

param_grid <- data.table::CJ(
  z_n = c(40L, 60L, 120L),
  long_z = c(-0.75, -1, -1.5),
  short_z = c(0.75, 1, 1.5),
  target_size = 0.95
)

mining_res <- strategyr::mine_strategy_walk_forward(
  DT = curve_dt[contract_rank == 1L, .(
    datetime = as.POSIXct(date),
    open = close,
    high = close,
    low = close,
    close = close
  )],
  strategy_fun = function(DT, z_n, long_z, short_z, target_size) {
    dates <- as.Date(DT$datetime)
    signal_curve <- curve_dt[date %in% dates]
    dbg <- strategyr::strat_roll_yield_mean_revert_tgt_pos(
      signal_curve,
      z_n = z_n,
      long_z = long_z,
      short_z = short_z,
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
  strat_id = 614L,
  asset_id = 8201L,
  ctr_size = 0.01,
  ctr_step = 0.01,
  lev = 1.0,
  fee_rt = 0.0007,
  tol_pos = 0.1
)

print(strategyr::summarize_walk_forward_results(mining_res, group_cols = c("z_n", "long_z", "short_z")))
print(strategyr::select_strategy_params(
  mining_res,
  n = 5L,
  param_cols = c("z_n", "long_z", "short_z", "target_size"),
  include_metrics = TRUE
))
