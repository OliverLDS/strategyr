library(testthat)
library(data.table)

test_that("credit and excess spread features are added in place", {
  DT <- data.table(
    ytm = c(0.06, 0.07),
    benchmark_ytm = c(0.04, 0.045),
    benchmark_spread = c(0.01, 0.015)
  )

  calc_credit_spread(DT)
  calc_excess_spread(DT)

  expect_equal(DT$credit_spread, c(0.02, 0.025), tolerance = 1e-12)
  expect_equal(DT$excess_spread, c(0.01, 0.01), tolerance = 1e-12)
})

test_that("spread curve slope captures upward and downward spread term structure", {
  panel <- data.table(
    date = rep(1:2, each = 3),
    tenor = rep(c(2, 5, 10), times = 2),
    credit_spread = c(0.01, 0.015, 0.02, 0.02, 0.015, 0.01)
  )

  out <- calc_spread_curve_slope(panel)

  expect_gt(out$spread_curve_slope[1], 0)
  expect_lt(out$spread_curve_slope[2], 0)
})

test_that("spread curve butterfly reflects curvature around the belly tenor", {
  panel <- data.table(
    date = rep(1:2, each = 3),
    tenor = rep(c(2, 5, 10), times = 2),
    credit_spread = c(0.01, 0.015, 0.02, 0.01, 0.02, 0.015)
  )

  out <- calc_spread_curve_butterfly(panel)

  expect_equal(out$spread_curve_butterfly[1], 0, tolerance = 1e-12)
  expect_equal(out$spread_curve_butterfly[2], 0.015, tolerance = 1e-12)
})
