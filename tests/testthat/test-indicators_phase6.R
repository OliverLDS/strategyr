library(testthat)
library(data.table)

test_that("option moneyness features are added in place", {
  DT <- data.table(
    S = c(100, 100),
    K = c(95, 105),
    T = c(30 / 365, 30 / 365),
    r = c(0.05, 0.05),
    q = c(0.01, 0.01)
  )

  calc_option_moneyness(DT)
  calc_option_forward_moneyness(DT)

  expect_equal(DT$option_moneyness, c(100 / 95, 100 / 105), tolerance = 1e-12)
  expect_true(all(c("option_forward", "option_forward_moneyness", "option_log_forward_moneyness") %in% names(DT)))
})

test_that("option IV skew and put-call spread reflect chain asymmetry", {
  DT <- data.table(
    date = rep(1, 4),
    T = rep(30 / 365, 4),
    type = c("put", "call", "put", "call"),
    option_log_forward_moneyness = c(-0.1, 0.1, -0.01, 0.01),
    iv = c(0.30, 0.22, 0.26, 0.24)
  )

  skew <- calc_option_iv_skew(DT, target_abs_moneyness = 0.1)
  spread <- calc_option_put_call_iv_spread(DT)

  expect_equal(skew$iv_skew, 0.08, tolerance = 1e-12)
  expect_equal(spread$iv_put_call_spread, 0.02, tolerance = 1e-12)
})

test_that("option IV term structure and smile slope summarize chain state", {
  DT <- data.table(
    date = rep(1, 6),
    T = c(rep(30 / 365, 3), rep(60 / 365, 3)),
    option_log_forward_moneyness = c(-0.1, 0, 0.1, -0.1, 0, 0.1),
    iv = c(0.28, 0.25, 0.27, 0.30, 0.27, 0.29)
  )

  ts <- calc_option_iv_term_structure(DT)
  smile <- calc_option_smile_slope(DT)

  expect_equal(ts$iv_atm_front, 0.25, tolerance = 1e-12)
  expect_equal(ts$iv_atm_back, 0.27, tolerance = 1e-12)
  expect_gt(ts$iv_term_structure, 0)
  expect_gt(smile$iv_smile_slope[1], 0)
  expect_gt(smile$iv_smile_slope[2], 0)
})
