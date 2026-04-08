library(testthat)
library(data.table)

test_that("statistical feature builders add the expected columns", {
  DT <- data.table(close = 1:10)

  calc_normalize(DT, cols = "close", ns = 5)
  calc_zscore(DT, cols = "close", ns = 5)
  calc_percent_rank(DT, cols = "close", ns = 5)
  calc_rolling_quantile(DT, cols = "close", ns = 5, probs = c(0.25, 0.5, 0.75))
  calc_skewness(DT, cols = "close", ns = 5)
  calc_kurtosis(DT, cols = "close", ns = 5)

  expect_true(all(c(
    "normalize_close_5", "zscore_close_5", "percent_rank_close_5",
    "quantile_close_5_0p25", "quantile_close_5_0p5", "quantile_close_5_0p75",
    "skew_close_5", "kurt_close_5"
  ) %in% names(DT)))
  expect_equal(DT$normalize_close_5[10], 1, tolerance = 1e-12)
  expect_equal(DT$percent_rank_close_5[10], 1, tolerance = 1e-12)
  expect_equal(DT$quantile_close_5_0p5[10], stats::quantile(6:10, 0.5, names = FALSE), tolerance = 1e-12)
  expect_equal(DT$zscore_close_5[10], (10 - mean(6:10)) / stats::sd(6:10), tolerance = 1e-12)
})

test_that("realized volatility is zero under constant log returns", {
  DT <- data.table(close = exp(seq(0, by = 0.01, length.out = 30)))
  calc_realized_vol(DT, ns = 10)

  expect_equal(DT$rv_10[30], 0, tolerance = 1e-8)
})

test_that("rolling skewness and kurtosis match direct window formulas", {
  x <- c(1, 2, 3, 4, 10, 11)
  DT <- data.table(close = x)
  calc_skewness(DT, cols = "close", ns = 4)
  calc_kurtosis(DT, cols = "close", ns = 4)

  window <- x[3:6]
  mu <- mean(window)
  s <- stats::sd(window)
  ref_skew <- mean((window - mu)^3) / s^3
  ref_kurt <- mean((window - mu)^4) / s^4 - 3

  expect_equal(DT$skew_close_4[6], ref_skew, tolerance = 1e-12)
  expect_equal(DT$kurt_close_4[6], ref_kurt, tolerance = 1e-12)
})

test_that("advance-decline breadth aggregates panel state correctly", {
  panel <- data.table(
    date = rep(1:3, each = 2),
    asset = rep(c("A", "B"), times = 3),
    close = c(10, 20, 11, 19, 12, 20),
    volume = c(100, 200, 150, 250, 120, 300)
  )

  ad <- calc_breadth_ad(panel)
  adl <- calc_breadth_adl(panel)
  ratio <- calc_breadth_ratio(panel)
  trin <- calc_breadth_trin(panel)

  expect_equal(ad$breadth_adv, c(0, 1, 2))
  expect_equal(ad$breadth_dec, c(0, 1, 0))
  expect_equal(ad$breadth_ad, c(0, 0, 2))
  expect_equal(adl$breadth_adl, c(0, 0, 2))
  expect_equal(ratio$breadth_ratio[2], 1, tolerance = 1e-12)
  expect_true(is.na(trin$breadth_trin[1]))
  expect_equal(trin$breadth_trin[2], 1 / (150 / 250), tolerance = 1e-12)
})

test_that("new-high new-low breadth counts prior-window extremes", {
  panel <- data.table(
    date = rep(1:4, each = 2),
    asset = rep(c("A", "B"), times = 4),
    high = c(10, 20, 11, 19, 12, 18, 13, 17),
    low = c(9, 19, 9.5, 18, 10, 17, 11, 16)
  )

  out <- calc_breadth_high_low(panel, n = 2)

  expect_equal(out$breadth_high[4], 1)
  expect_equal(out$breadth_low[4], 1)
  expect_equal(out$breadth_high_low[4], 0)
})
