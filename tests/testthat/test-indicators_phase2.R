library(testthat)
library(data.table)

test_that("spread, ratio, and log-spread features are added in place", {
  DT <- data.table(
    close = c(10, 11, 12),
    benchmark_close = c(5, 5.5, 6)
  )

  calc_spread(DT)
  calc_ratio(DT)
  calc_log_spread(DT)

  expect_equal(DT$spread_close_benchmark_close, c(5, 5.5, 6), tolerance = 1e-12)
  expect_equal(DT$ratio_close_benchmark_close, c(2, 2, 2), tolerance = 1e-12)
  expect_equal(DT$log_spread_close_benchmark_close, log(DT$close) - log(DT$benchmark_close), tolerance = 1e-12)
})

test_that("rolling beta and correlation match reference window calculations", {
  DT <- data.table(
    close = c(100, 102, 101, 105, 107, 110),
    benchmark_close = c(100, 101, 103, 104, 106, 108)
  )

  calc_rolling_beta(DT, ns = 4)
  calc_rolling_corr(DT, ns = 4)

  x_ret <- diff(log(DT$close))
  y_ret <- diff(log(DT$benchmark_close))
  xw <- x_ret[2:5]
  yw <- y_ret[2:5]
  ref_beta <- stats::cov(xw, yw) / stats::var(yw)
  ref_corr <- stats::cor(xw, yw)

  expect_equal(DT$beta_4[6], ref_beta, tolerance = 1e-12)
  expect_equal(DT$corr_4[6], ref_corr, tolerance = 1e-12)
})

test_that("tracking error is zero when asset and benchmark returns match", {
  DT <- data.table(
    close = exp(seq(0, by = 0.01, length.out = 30)),
    benchmark_close = exp(seq(0, by = 0.01, length.out = 30))
  )

  calc_tracking_error(DT, ns = 10)

  expect_equal(DT$tracking_error_10[30], 0, tolerance = 1e-8)
})

test_that("relative strength equals one when the asset matches the benchmark", {
  DT <- data.table(
    close = c(100, 101, 102, 103, 104, 105),
    benchmark_close = c(100, 101, 102, 103, 104, 105)
  )

  calc_relative_strength(DT, ns = 3)

  expect_equal(DT$relative_strength_3[6], 1, tolerance = 1e-12)
})
