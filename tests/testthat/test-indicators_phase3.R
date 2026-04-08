library(testthat)
library(data.table)

test_that("quote-based microstructure features are added in place", {
  DT <- data.table(
    bid = c(99, 100),
    ask = c(101, 102),
    bid_size = c(200, 300),
    ask_size = c(100, 300)
  )

  calc_bid_ask_spread(DT)
  calc_mid_price(DT)
  calc_microprice(DT)
  calc_order_imbalance(DT)

  expect_equal(DT$bid_ask_spread, c(2, 2), tolerance = 1e-12)
  expect_equal(DT$bid_ask_spread_rel[1], 2 / 100, tolerance = 1e-12)
  expect_equal(DT$mid_price, c(100, 101), tolerance = 1e-12)
  expect_equal(DT$microprice[1], (101 * 200 + 99 * 100) / 300, tolerance = 1e-12)
  expect_equal(DT$order_imbalance, c((200 - 100) / 300, 0), tolerance = 1e-12)
})

test_that("turnover features use traded value and optional float ratio", {
  DT <- data.table(
    close = c(10, 11),
    volume = c(100, 200),
    shares_out = c(1000, 1000)
  )

  calc_turnover(DT, float_col = "shares_out")

  expect_equal(DT$turnover_value, c(1000, 2200), tolerance = 1e-12)
  expect_equal(DT$turnover_ratio, c(0.1, 0.2), tolerance = 1e-12)
})

test_that("slippage and price-impact proxies match direct definitions", {
  DT <- data.table(
    high = c(101, 103, 104),
    low = c(99, 100, 101),
    close = c(100, 102, 103),
    volume = c(1000, 2000, 1500)
  )

  calc_slippage_proxy(DT)
  calc_price_impact_proxy(DT)

  expect_equal(DT$slippage_proxy, c(0.02, 3 / 102, 3 / 103), tolerance = 1e-12)
  ref_impact <- c(
    NA_real_,
    abs(log(102 / 100)) / (102 * 2000),
    abs(log(103 / 102)) / (103 * 1500)
  )
  expect_equal(DT$price_impact_proxy, ref_impact, tolerance = 1e-12)
})
