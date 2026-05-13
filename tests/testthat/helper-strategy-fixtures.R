make_test_ohlc <- function(n = 120L) {
  close <- 100 + cumsum(rep(0.1, n))
  data.table::data.table(
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + seq_len(n) * 86400,
    open = close,
    high = close + 1,
    low = close - 1,
    close = close,
    volume = 1000
  )
}

make_test_regime_ohlc <- function(n = 180L) {
  idx <- seq_len(n)
  drift <- c(rep(1.25, n %/% 3L), rep(-1.45, n %/% 3L), rep(0.95, n - 2L * (n %/% 3L)))
  cycle <- sin(idx / 5) * 0.8
  close <- 100 + cumsum(drift + cycle)
  data.table::data.table(
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + idx * 86400,
    open = data.table::shift(close, fill = close[[1L]]),
    high = pmax(close, data.table::shift(close, fill = close[[1L]])) + 0.4,
    low = pmin(close, data.table::shift(close, fill = close[[1L]])) - 0.4,
    close = close,
    volume = round(1000 + abs(cycle) * 250 + idx)
  )
}

make_test_pair_market <- function(n = 120L) {
  DT <- make_test_ohlc(n)
  DT[, benchmark_close := seq(100, 100 + n - 1, length.out = n)]
  DT
}

make_test_spread_reversion_market <- function(n = 160L) {
  DT <- make_test_ohlc(n)
  spread <- sin(seq_len(n) / 4) * 4
  DT[, benchmark_close := close - spread]
  DT
}

make_test_cross_sectional_market <- function(n_dates = 4L, assets = c("AAA", "BBB", "CCC", "DDD")) {
  dates <- as.Date("2020-01-01") + seq_len(n_dates) - 1L
  out <- data.table::CJ(date = dates, asset = assets)
  out[, score := c(
    4, 3, 2, 1,
    1, 4, 3, 2,
    2, 1, 4, 3,
    3, 2, 1, 4
  )[seq_len(.N)]]
  out[, close := 100 + seq_len(.N)]
  out
}

make_test_portfolio_weight_panel <- function(n_dates = 8L, assets = c("AAA", "BBB", "CCC")) {
  dates <- as.Date("2020-01-01") + seq_len(n_dates) - 1L
  out <- data.table::CJ(date = dates, asset = assets)
  out[, asset_idx := match(asset, assets)]
  out[, open := 100 + asset_idx * 5 + as.integer(date - min(date)) * asset_idx]
  out[, close := open * (1 + 0.002 * asset_idx)]
  out[, target_weight := 0]
  out[asset == "AAA", target_weight := 0.6]
  out[asset == "BBB", target_weight := 0.4]
  out[date >= dates[[n_dates %/% 2L]], `:=`(
    target_weight = data.table::fifelse(asset == "CCC", 0.5, data.table::fifelse(asset == "AAA", 0.5, 0))
  )]
  out[, asset_idx := NULL]
  out[]
}

make_test_futures_curve <- function(n_dates = 100L) {
  dates <- as.Date("2020-01-01") + seq_len(n_dates) - 1L
  out <- data.table::CJ(date = dates, contract_rank = 1:2)
  out[, t := as.integer(date - min(date))]
  out[, close := 50 + 0.05 * t + data.table::fifelse(contract_rank == 1L, sin(t / 8), cos(t / 8) * 1.5)]
  out[, time_to_expiry := data.table::fifelse(contract_rank == 1L, 30 / 365, 60 / 365)]
  out[, t := NULL]
  out[]
}

make_test_option_surface <- function(n_dates = 80L) {
  dates <- as.Date("2020-01-01") + seq_len(n_dates) - 1L
  out <- data.table::CJ(date = dates, T = c(30, 60) / 365, type = c("put", "call"))
  out[, day_idx := as.integer(date - min(date))]
  out[, option_log_forward_moneyness := data.table::fifelse(type == "put", -0.1, 0.1)]
  out[, close := 100 + day_idx * 0.2 + sin(day_idx / 6)]
  out[, iv := 0.2 + 0.02 * sin(day_idx / 5) + data.table::fifelse(type == "put", 0.03, -0.01)]
  out[, day_idx := NULL]
  out[]
}


make_test_portfolio_state <- function(assets = c("AAA", "BBB", "CCC", "DDD")) {
  data.table::data.table(
    asset = assets,
    price = rep(100, length(assets)),
    current_units = rep(0, length(assets)),
    contract_size = rep(1, length(assets)),
    lot_step = rep(1, length(assets))
  )
}

make_test_fx_market <- function(n = 120L) {
  DT <- data.table::data.table(
    spot = 100 + cumsum(rep(0.1, n)),
    r_domestic = rep(0.05, n),
    r_foreign = rep(0.02, n)
  )
  DT
}

make_test_bond_market <- function(n = 5L) {
  data.table::data.table(
    par = rep(100, n),
    c_rate = rep(0.05, n),
    T = seq(2, 2 + n - 1),
    freq = rep(2, n),
    ytm = rep(0.04, n)
  )
}

make_test_state <- function(last_px = 100) {
  list(
    ctr_size = 1,
    ctr_step = 1,
    lev = 10,
    last_px = last_px,
    ctr_unit = 0,
    avg_price = 0,
    cash = 10000,
    pos_dir = 0L
  )
}
