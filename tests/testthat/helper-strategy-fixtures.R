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

make_test_pair_market <- function(n = 120L) {
  DT <- make_test_ohlc(n)
  DT[, benchmark_close := seq(100, 100 + n - 1, length.out = n)]
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

make_test_portfolio_state <- function(assets = c("AAA", "BBB", "CCC", "DDD")) {
  data.table::data.table(
    asset = assets,
    price = rep(100, length(assets)),
    current_units = rep(0, length(assets)),
    contract_size = rep(1, length(assets)),
    lot_step = rep(1, length(assets))
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
