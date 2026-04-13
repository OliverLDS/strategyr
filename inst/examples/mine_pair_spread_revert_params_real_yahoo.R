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
benchmark_ticker <- "LQD"
seed_assets <- c("HYG", "AGG", "SHY", "IEF", "TLT", "BND")

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

build_benchmark_market <- function(symbol, benchmark_dt, min_rows = 260L) {
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
# deeper offline mining job. The gallery settings are z = 20 and 2/0.5.
param_grid <- data.table::CJ(
  z_n = c(20L, 40L, 60L),
  entry_z = c(1.5, 2.0, 2.5),
  exit_z = c(0.25, 0.5, 1.0),
  target_size = 0.95,
  sample = TRUE
)[exit_z < entry_z]

registry_dt <- data.table::as.data.table(jsonlite::fromJSON(registry_path))
candidate_registry <- registry_dt[
  main_asset_type %in% c("Equity", "Credit")
]

benchmark_dt <- load_local_yahoo(benchmark_ticker)
if (is.null(benchmark_dt)) {
  stop("Need locally cached Yahoo data for benchmark ticker: ", benchmark_ticker)
}

candidate_symbols <- setdiff(unique(candidate_registry$yahoo_finance_ticker), benchmark_ticker)
asset_market_data <- list()
for (symbol in candidate_symbols) {
  symbol_dt <- build_benchmark_market(symbol, benchmark_dt)
  if (!is.null(symbol_dt)) {
    asset_market_data[[symbol]] <- symbol_dt
  }
}

if (length(asset_market_data) < 2L) {
  stop("Need at least two locally cached Yahoo assets for pair-spread mining.")
}

pair_spread_mining_res <- strategyr::mine_strategy_asset_years(
  market_data_list = asset_market_data,
  strategy_fun = strategyr::strat_pair_spread_revert_tgt_pos,
  param_grid = param_grid,
  seed_assets = seed_assets,
  from = from_date,
  to = to_date,
  min_year_rows = 200L,
  strat_id = 501L,
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

print(pair_spread_mining_res$seed_params[, .(
  seed_asset,
  rank,
  z_n,
  entry_z,
  exit_z,
  target_size,
  sample,
  sortino,
  total_return,
  annual_return,
  max_drawdown
)])

print(pair_spread_mining_res$candidate_params)

asset_year_res <- pair_spread_mining_res$asset_year_results
print(asset_year_res[seq_len(min(.N, 20L)), .(
  rank,
  asset,
  year,
  param_id,
  z_n,
  entry_z,
  exit_z,
  target_size,
  sample,
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
