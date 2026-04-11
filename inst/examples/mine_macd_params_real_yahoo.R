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

market_dt <- data.table::as.data.table(
  investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo")
)

if (nrow(market_dt) == 0L || !"datetime" %in% names(market_dt)) {
  stop("Local Yahoo OHLC cache is empty or malformed for ticker: ", ticker)
}

market_dt <- market_dt[
  datetime >= as.POSIXct(from_date) &
    datetime < as.POSIXct(to_date + 1)
][order(datetime)]

bad_ohlc <- !is.finite(market_dt$open) |
  !is.finite(market_dt$high) |
  !is.finite(market_dt$low) |
  !is.finite(market_dt$close)
if (any(bad_ohlc)) {
  warning(
    sprintf("Dropped %s %s rows with incomplete OHLC values before mining.", sum(bad_ohlc), ticker)
  )
  market_dt <- market_dt[!bad_ohlc]
}

if (nrow(market_dt) < 260L) {
  stop("Need at least 260 rows of local OHLC data for MACD parameter mining.")
}

# Keep the first public example grid modest. Increase these ranges when running
# a deeper offline mining job.
param_grid <- data.table::CJ(
  fast = c(8L, 10L, 12L, 14L),
  slow = c(21L, 26L, 30L, 35L),
  signal = c(7L, 9L, 12L),
  target_size = 0.95
)[fast < slow]

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

mine_macd_strategy <- function(strategy_fun, strat_id) {
  strategyr::mine_strategy_params(
    DT = market_dt,
    strategy_fun = strategy_fun,
    param_grid = param_grid,
    strat_id = strat_id,
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
}

top_rows <- function(res, label, n = 10L) {
  out <- res[seq_len(min(.N, n)), .(
    strategy = label,
    rank,
    fast,
    slow,
    signal,
    target_size,
    sortino,
    total_return,
    annual_return,
    max_drawdown
  )]
  out
}

macd_mining_res <- mine_macd_strategy(
  strategy_fun = strategyr::strat_macd_cross_tgt_pos,
  strat_id = 304L
)
macd_contrarian_mining_res <- mine_macd_strategy(
  strategy_fun = strategyr::strat_macd_contrarian_tgt_pos,
  strat_id = 305L
)

best_macd_params <- macd_mining_res[1L]
best_macd_contrarian_params <- macd_contrarian_mining_res[1L]

print(best_macd_params)
print(best_macd_contrarian_params)

top_macd_res <- top_rows(macd_mining_res, "macd_cross")
top_macd_contrarian_res <- top_rows(macd_contrarian_mining_res, "macd_contrarian")

print(top_macd_res)
print(top_macd_contrarian_res)

best_by_strategy <- data.table::rbindlist(list(
  top_rows(macd_mining_res, "macd_cross", n = 1L),
  top_rows(macd_contrarian_mining_res, "macd_contrarian", n = 1L)
))
data.table::setorder(best_by_strategy, -sortino)

print(best_by_strategy)

registry_dt <- data.table::as.data.table(jsonlite::fromJSON(registry_path))

# MACD is a price-path rule. For this example, skip yield indices and volatility
# indices, then keep only symbols with usable local Yahoo OHLC cache.
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

best_contrarian_strategy_params <- as.list(
  best_macd_contrarian_params[, .(fast, slow, signal, target_size)]
)

macd_contrarian_asset_res <- strategyr::mine_strategy_assets(
  market_data_list = asset_market_data,
  strategy_fun = strategyr::strat_macd_contrarian_tgt_pos,
  strategy_params = best_contrarian_strategy_params,
  from = from_date,
  to = to_date,
  strat_id = 305L,
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

print(best_contrarian_strategy_params)
print(macd_contrarian_asset_res[seq_len(min(.N, 20L)), .(
  rank,
  asset,
  sortino,
  total_return,
  annual_return,
  max_drawdown,
  n_obs
)])
