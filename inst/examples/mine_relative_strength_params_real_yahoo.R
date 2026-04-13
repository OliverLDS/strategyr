library(data.table)
library(strategyr)

if (!requireNamespace("investdatar", quietly = TRUE)) {
  stop("Package `investdatar` is required for this real-data example.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package `jsonlite` is required to read the local Yahoo ticker registry.")
}

from_date <- as.Date("2000-01-01")
to_date <- Sys.Date()
registry_path <- "/Users/oliver/Documents/2025/_2025-08-05_investdatar/YahooFinance_ticker_registry.json"
benchmark_ticker <- "IVV"
seed_assets <- c("SPY", "AGG", "IAU", "IBIT", "USO", "UUP")

load_local_yahoo <- function(symbol, min_rows = 260L) {
  out <- tryCatch(
    data.table::as.data.table(
      investdatar::get_local_quantmod_OHLC(symbol, src = "yahoo")
    ),
    error = function(e) NULL
  )

  if (is.null(out) || nrow(out) == 0L || !"datetime" %in% names(out)) {
    return(NULL)
  }

  out <- out[
    datetime >= as.POSIXct(from_date) &
      datetime < as.POSIXct(to_date + 1)
  ][order(datetime)]

  bad_ohlc <- !is.finite(out$open) |
    !is.finite(out$high) |
    !is.finite(out$low) |
    !is.finite(out$close) |
    out$open <= 0 |
    out$high <= 0 |
    out$low <= 0 |
    out$close <= 0
  if (any(bad_ohlc)) {
    out <- out[!bad_ohlc]
  }

  if (nrow(out) < min_rows) {
    return(NULL)
  }

  out
}

build_relative_strength_market <- function(symbol, benchmark_dt, min_rows = 260L) {
  out <- load_local_yahoo(symbol, min_rows = min_rows)
  if (is.null(out)) {
    return(NULL)
  }

  out[, date := as.Date(datetime)]
  benchmark_close <- benchmark_dt[, .(date = as.Date(datetime), benchmark_close = close)]
  out <- merge(out, benchmark_close, by = "date", all = FALSE)[order(datetime)]
  out[, date := NULL]

  if (nrow(out) < min_rows) {
    return(NULL)
  }

  out
}

# Keep this public example grid modest. Increase these ranges when running a
# deeper offline mining job. The gallery settings are n = 20 and 1.02/0.98.
param_grid <- data.table::CJ(
  n = c(10L, 20L, 60L, 120L),
  long_threshold = c(1.01, 1.02, 1.05),
  short_threshold = c(0.95, 0.98, 0.99),
  target_size = 0.95,
  use_log = TRUE
)

registry_dt <- data.table::as.data.table(jsonlite::fromJSON(registry_path))

# Relative strength compares each traded asset with one fixed benchmark. For
# this example, skip yield indices and volatility indices, then keep only
# symbols with usable local Yahoo OHLC cache.
candidate_registry <- registry_dt[
  main_asset_type %in% c("Equity", "Credit", "Commodity", "Cryptocurrency", "FX")
]

benchmark_dt <- load_local_yahoo(benchmark_ticker)
if (is.null(benchmark_dt)) {
  stop("Need locally cached Yahoo data for benchmark ticker: ", benchmark_ticker)
}

candidate_symbols <- setdiff(unique(candidate_registry$yahoo_finance_ticker), benchmark_ticker)
asset_market_data <- list()
for (symbol in candidate_symbols) {
  symbol_dt <- build_relative_strength_market(symbol, benchmark_dt)
  if (!is.null(symbol_dt)) {
    asset_market_data[[symbol]] <- symbol_dt
  }
}

if (length(asset_market_data) < 2L) {
  stop("Need at least two locally cached Yahoo assets for relative-strength mining.")
}

relative_strength_mining_res <- strategyr::mine_strategy_asset_years(
  market_data_list = asset_market_data,
  strategy_fun = strategyr::strat_relative_strength_tgt_pos,
  param_grid = param_grid,
  seed_assets = seed_assets,
  from = from_date,
  to = to_date,
  min_year_rows = 200L,
  strat_id = 503L,
  asset_id = 8001L,
  ctr_size = 0.01,
  ctr_step = 0.01,
  lev = 1.0,
  fee_rt = 0.0007,
  fund_rt = 0,
  tol_pos = 0.1,
  rec = FALSE,
  keep_paths = FALSE,
  annualization = 252
)

print(relative_strength_mining_res$seed_params[, .(
  seed_asset,
  rank,
  n,
  long_threshold,
  short_threshold,
  target_size,
  use_log,
  sortino,
  total_return,
  annual_return,
  max_drawdown
)])

print(relative_strength_mining_res$candidate_params)

asset_year_res <- relative_strength_mining_res$asset_year_results
print(asset_year_res[seq_len(min(.N, 20L)), .(
  rank,
  asset,
  year,
  param_id,
  n,
  long_threshold,
  short_threshold,
  target_size,
  use_log,
  sortino,
  total_return,
  buy_hold_total_return,
  excess_total_return,
  signal_start,
  trade_start,
  trade_end,
  warmup_n_obs,
  warmup_insufficient,
  n_obs
)])
