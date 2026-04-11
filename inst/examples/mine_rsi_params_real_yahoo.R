library(data.table)
library(strategyr)

if (!requireNamespace("investdatar", quietly = TRUE)) {
  stop("Package `investdatar` is required for this real-data example.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package `jsonlite` is required to read the local Yahoo ticker registry.")
}

ticker <- "^GSPC"
from_date <- as.Date("2000-01-01")
to_date <- Sys.Date()
registry_path <- "/Users/oliver/Documents/2025/_2025-08-05_investdatar/YahooFinance_ticker_registry.json"

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
    !is.finite(out$close)
  if (any(bad_ohlc)) {
    out <- out[!bad_ohlc]
  }

  if (nrow(out) < min_rows) {
    return(NULL)
  }

  out
}

market_dt <- load_local_yahoo(ticker)

if (is.null(market_dt)) {
  stop("Need at least 260 rows of local OHLC data for RSI parameter mining: ", ticker)
}

# Keep the first public example grid modest. Increase these ranges when running
# a deeper offline mining job.
param_grid <- data.table::CJ(
  n = c(7L, 10L, 14L, 21L),
  oversold = c(25, 30, 35, 40),
  overbought = c(60, 65, 70, 75),
  exit_level = c(45, 50, 55),
  target_size = 0.95
)[oversold < exit_level & exit_level < overbought]

rsi_mining_res <- strategyr::mine_strategy_params(
  DT = market_dt,
  strategy_fun = strategyr::strat_rsi_revert_tgt_pos,
  param_grid = param_grid,
  strat_id = 303L,
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

best_rsi_params <- rsi_mining_res[1L]

print(best_rsi_params)
print(rsi_mining_res[seq_len(min(.N, 10L)), .(
  rank,
  n,
  oversold,
  overbought,
  exit_level,
  target_size,
  sortino,
  total_return,
  annual_return,
  max_drawdown
)])

registry_dt <- data.table::as.data.table(jsonlite::fromJSON(registry_path))

# RSI reversion is a price-path rule. For this example, skip yield indices and
# volatility indices, then keep only symbols with usable local Yahoo OHLC cache.
candidate_registry <- registry_dt[
  main_asset_type %in% c("Equity", "Credit", "Commodity", "Cryptocurrency", "FX")
]

candidate_symbols <- unique(candidate_registry$yahoo_finance_ticker)
asset_market_data <- list()
for (symbol in candidate_symbols) {
  symbol_dt <- load_local_yahoo(symbol)
  if (!is.null(symbol_dt)) {
    asset_market_data[[symbol]] <- symbol_dt
  }
}

if (length(asset_market_data) < 2L) {
  stop("Need at least two locally cached Yahoo assets for asset mining.")
}

best_strategy_params <- as.list(
  best_rsi_params[, .(n, oversold, overbought, exit_level, target_size)]
)

rsi_asset_res <- strategyr::mine_strategy_assets(
  market_data_list = asset_market_data,
  strategy_fun = strategyr::strat_rsi_revert_tgt_pos,
  strategy_params = best_strategy_params,
  from = from_date,
  to = to_date,
  strat_id = 303L,
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

print(best_strategy_params)
print(rsi_asset_res[seq_len(min(.N, 20L)), .(
  rank,
  asset,
  sortino,
  total_return,
  annual_return,
  max_drawdown,
  n_obs
)])
