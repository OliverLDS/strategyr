library(data.table)
library(strategyr)

if (!requireNamespace("investdatar", quietly = TRUE)) {
  stop("Package `investdatar` is required for this real-data example.")
}

from_date <- as.Date("2000-01-01")
to_date <- Sys.Date()

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

build_ratio_ohlc <- function(short_symbol, long_symbol, min_rows = 260L) {
  short_dt <- load_local_yahoo(short_symbol, min_rows = min_rows)
  long_dt <- load_local_yahoo(long_symbol, min_rows = min_rows)
  if (is.null(short_dt) || is.null(long_dt)) {
    return(NULL)
  }

  short_px <- short_dt[, .(
    date = as.Date(datetime),
    datetime,
    open_short = open,
    high_short = high,
    low_short = low,
    close_short = close
  )]
  long_px <- long_dt[, .(
    date = as.Date(datetime),
    open_long = open,
    high_long = high,
    low_long = low,
    close_long = close
  )]

  out <- merge(short_px, long_px, by = "date", all = FALSE)[order(datetime)]
  out <- out[, .(
    date,
    datetime,
    open = open_short / open_long,
    high = high_short / low_long,
    low = low_short / high_long,
    close = close_short / close_long
  )]

  if (nrow(out) < min_rows) {
    return(NULL)
  }

  out
}

load_curve_features <- function() {
  treasury_dt <- data.table::as.data.table(
    investdatar::get_local_treasury_rates("par_yield_curve")
  )
  if (nrow(treasury_dt) == 0L) {
    stop("Local Treasury data not found for dataset: par_yield_curve")
  }

  curve_dt <- data.table::dcast(
    treasury_dt[
      measure == "yield" &
        tenor %in% c("2YEAR", "10YEAR") &
        as.Date(date) >= from_date &
        as.Date(date) <= to_date,
      .(date = as.Date(date), tenor, value = value / 100)
    ],
    date ~ tenor,
    value.var = "value"
  )
  data.table::setnames(curve_dt, c("2YEAR", "10YEAR"), c("short_rate", "long_rate"))
  curve_dt
}

build_curve_ratio_market <- function(short_symbol, long_symbol, curve_dt, min_rows = 260L) {
  ratio_dt <- build_ratio_ohlc(short_symbol, long_symbol, min_rows = min_rows)
  if (is.null(ratio_dt)) {
    return(NULL)
  }

  out <- merge(ratio_dt, curve_dt, by = "date", all = FALSE)[order(datetime)]
  out[, `:=`(
    short_rate_signal = data.table::shift(short_rate, 1L),
    long_rate_signal = data.table::shift(long_rate, 1L)
  )]
  out <- out[!is.na(short_rate_signal) & !is.na(long_rate_signal)]
  out[, date := NULL]

  if (nrow(out) < min_rows) {
    return(NULL)
  }

  out
}

strat_curve_steepener_lagged_tgt_pos <- function(
  DT,
  short_rate_col = "short_rate_signal",
  long_rate_col = "long_rate_signal",
  long_threshold = 0.006,
  short_threshold = 0.004,
  target_size = 0.95
) {
  strategyr::strat_curve_steepener_tgt_pos(
    DT,
    short_rate_col = short_rate_col,
    long_rate_col = long_rate_col,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size,
    compute_features = TRUE
  )
}

strat_curve_steepener_contrarian_tgt_pos <- function(
  DT,
  short_rate_col = "short_rate_signal",
  long_rate_col = "long_rate_signal",
  long_threshold = 0.006,
  short_threshold = 0.004,
  target_size = 0.95
) {
  -strat_curve_steepener_lagged_tgt_pos(
    DT,
    short_rate_col = short_rate_col,
    long_rate_col = long_rate_col,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )
}

curve_pairs <- data.table::data.table(
  asset = c("SHY/TLT", "IEI/TLT", "IEF/TLT", "SHY/IEF"),
  short_symbol = c("SHY", "IEI", "IEF", "SHY"),
  long_symbol = c("TLT", "TLT", "TLT", "IEF")
)
seed_assets <- c("SHY/TLT", "IEI/TLT", "IEF/TLT", "SHY/IEF")

# Keep this public example grid modest. Increase these ranges when running a
# deeper offline mining job. The gallery settings are 60bp/40bp.
param_grid <- data.table::CJ(
  long_threshold = c(0.004, 0.006, 0.008),
  short_threshold = c(0.002, 0.004, 0.006),
  target_size = 0.95
)[short_threshold < long_threshold]

curve_dt <- load_curve_features()
asset_market_data <- list()
for (i in seq_len(nrow(curve_pairs))) {
  pair_dt <- build_curve_ratio_market(
    short_symbol = curve_pairs$short_symbol[[i]],
    long_symbol = curve_pairs$long_symbol[[i]],
    curve_dt = curve_dt
  )
  if (!is.null(pair_dt)) {
    asset_market_data[[curve_pairs$asset[[i]]]] <- pair_dt
  }
}

if (length(asset_market_data) < 1L) {
  stop("Need at least one locally cached Treasury ETF ratio for curve mining.")
}

mine_curve_asset_years <- function(strategy_fun, strat_id) {
  strategyr::mine_strategy_asset_years(
    market_data_list = asset_market_data,
    strategy_fun = strategy_fun,
    param_grid = param_grid,
    seed_assets = intersect(seed_assets, names(asset_market_data)),
    from = from_date,
    to = to_date,
    min_year_rows = 200L,
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

print_curve_mining_result <- function(res, label) {
  cat("\n", label, " seed-ratio best parameter rows:\n", sep = "")
  print(res$seed_params[, .(
    seed_asset,
    rank,
    long_threshold,
    short_threshold,
    target_size,
    sortino,
    total_return,
    annual_return,
    max_drawdown
  )])

  cat("\n", label, " unique candidate parameter rows:\n", sep = "")
  print(res$candidate_params)

  asset_year_res <- res$asset_year_results
  cat("\n", label, " asset-year winners above buy-and-hold:\n", sep = "")
  print(asset_year_res[seq_len(min(.N, 20L)), .(
    rank,
    asset,
    year,
    param_id,
    long_threshold,
    short_threshold,
    target_size,
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
}

curve_steepener_mining_res <- mine_curve_asset_years(
  strategy_fun = strat_curve_steepener_lagged_tgt_pos,
  strat_id = 603L
)

curve_steepener_contrarian_mining_res <- mine_curve_asset_years(
  strategy_fun = strat_curve_steepener_contrarian_tgt_pos,
  strat_id = 613L
)

print_curve_mining_result(curve_steepener_mining_res, "Curve steepener")
print_curve_mining_result(curve_steepener_contrarian_mining_res, "Curve steepener contrarian")
