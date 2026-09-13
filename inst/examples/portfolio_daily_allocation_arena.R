library(data.table)
library(strategyr)

if (!requireNamespace("investdatar", quietly = TRUE)) {
  stop("Package investdatar is required for this real-data example.")
}

# Daily Arena candidate instruments. The example retains only locally available
# Yahoo histories and their common completed trading dates.
arena_symbols <- c("SPY", "AGG", "IAU", "IBIT", "USO", "UUP", "QQQ", "TLT")
from_date <- as.Date("2020-01-01")
to_date <- Sys.Date()

load_local_daily_ohlc <- function(symbol) {
  out <- tryCatch(
    data.table::as.data.table(investdatar::get_local_quantmod_OHLC(symbol, src = "yahoo")),
    error = function(e) NULL
  )
  if (is.null(out) || !all(c("datetime", "open", "high", "low", "close") %in% names(out))) {
    return(NULL)
  }

  out[, date := as.Date(datetime)]
  out <- out[date >= from_date & date <= to_date]
  out <- out[
    is.finite(open) & is.finite(high) & is.finite(low) & is.finite(close) &
      open > 0 & high > 0 & low > 0 & close > 0
  ]
  if (!nrow(out)) {
    return(NULL)
  }
  out[, asset := symbol]
  out[, .(date, asset, open, high, low, close)]
}

asset_data <- lapply(arena_symbols, load_local_daily_ohlc)
asset_data <- Filter(Negate(is.null), asset_data)
if (length(asset_data) < 2L) {
  stop("Need at least two locally available Arena instruments.")
}

common_dates <- Reduce(intersect, lapply(asset_data, function(x) x$date))
panel <- data.table::rbindlist(asset_data)[date %in% common_dates]
data.table::setkey(panel, date, asset)

# Targets on row t use only data completed on t - 1 and execute at row t open.
targets <- strat_cross_asset_trend_allocation_target_weights(
  panel,
  trend_n = 126L,
  vol_n = 20L,
  min_obs = 126L,
  rebalance_n = 21L,
  gross_exposure = 1.0,
  weight_cap = 0.4
)

result <- strat_portfolio_daily_backtest(
  panel,
  targets,
  initial_cash = 100000,
  fee_rt = 0.0005
)

print(targets[date == max(date), .(asset, target_weight, eligible, rebalance_due, signal_date)])
print(result$equity)
print(result$weights[date == max(date)])
