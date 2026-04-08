library(testthat)
library(data.table)

test_that("option Greeks return a consistent call snapshot", {
  out <- calc_option_greeks(
    S = 100,
    K = 100,
    T = 1,
    r = 0.05,
    sigma = 0.2,
    type = "call"
  )

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 1)
  expect_length(setdiff(c("price", "delta", "gamma", "vega", "theta", "rho"), names(out)), 0)
  expect_equal(out$price, 10.45058, tolerance = 1e-5)
  expect_equal(out$delta, 0.63683, tolerance = 1e-5)
  expect_equal(out$gamma, 0.01876, tolerance = 1e-5)
  expect_equal(out$vega, 37.52403, tolerance = 1e-5)
  expect_equal(out$rho, 53.23248, tolerance = 1e-5)
})

test_that("call and put delta/rho signs follow Black-Scholes conventions", {
  call_delta <- calc_option_delta(100, 100, 1, 0.05, 0.2, type = "call")
  put_delta <- calc_option_delta(100, 100, 1, 0.05, 0.2, type = "put")
  call_rho <- calc_option_rho(100, 100, 1, 0.05, 0.2, type = "call")
  put_rho <- calc_option_rho(100, 100, 1, 0.05, 0.2, type = "put")

  expect_gt(call_delta, 0)
  expect_lt(put_delta, 0)
  expect_gt(call_rho, 0)
  expect_lt(put_rho, 0)
})

test_that("gamma and vega are positive for a standard option", {
  expect_gt(calc_option_gamma(100, 100, 1, 0.05, 0.2), 0)
  expect_gt(calc_option_vega(100, 100, 1, 0.05, 0.2), 0)
})

test_that("Greeks are consistent with finite-difference price bumps", {
  S <- 100
  K <- 100
  T <- 1
  r <- 0.05
  sigma <- 0.2
  h_S <- 1e-3
  h_sigma <- 1e-4
  h_r <- 1e-4

  price_0 <- strategyr:::.calc_option_price_bs(S, K, T, r, sigma, type = "call")
  price_S_up <- strategyr:::.calc_option_price_bs(S + h_S, K, T, r, sigma, type = "call")
  price_S_dn <- strategyr:::.calc_option_price_bs(S - h_S, K, T, r, sigma, type = "call")
  price_sigma_up <- strategyr:::.calc_option_price_bs(S, K, T, r, sigma + h_sigma, type = "call")
  price_sigma_dn <- strategyr:::.calc_option_price_bs(S, K, T, r, sigma - h_sigma, type = "call")
  price_r_up <- strategyr:::.calc_option_price_bs(S, K, T, r + h_r, sigma, type = "call")
  price_r_dn <- strategyr:::.calc_option_price_bs(S, K, T, r - h_r, sigma, type = "call")

  delta_fd <- (price_S_up - price_S_dn) / (2 * h_S)
  gamma_fd <- (price_S_up - 2 * price_0 + price_S_dn) / h_S^2
  vega_fd <- (price_sigma_up - price_sigma_dn) / (2 * h_sigma)
  rho_fd <- (price_r_up - price_r_dn) / (2 * h_r)

  expect_equal(calc_option_delta(S, K, T, r, sigma, type = "call"), delta_fd, tolerance = 1e-5)
  expect_equal(calc_option_gamma(S, K, T, r, sigma), gamma_fd, tolerance = 1e-5)
  expect_equal(calc_option_vega(S, K, T, r, sigma), vega_fd, tolerance = 1e-4)
  expect_equal(calc_option_rho(S, K, T, r, sigma, type = "call"), rho_fd, tolerance = 1e-3)
})

test_that("daily theta scales annual theta by 365", {
  theta_annual <- calc_option_theta(100, 100, 1, 0.05, 0.2, type = "call", scale = "annual")
  theta_daily <- calc_option_theta(100, 100, 1, 0.05, 0.2, type = "call", scale = "daily")

  expect_equal(theta_daily, theta_annual / 365, tolerance = 1e-12)
})

test_that("implied volatility recovers the Black-Scholes input for a call", {
  price <- strategyr:::.calc_option_price_bs(100, 100, 1, 0.05, 0.2, type = "call")

  expect_equal(
    calc_option_iv(price = price, S = 100, K = 100, T = 1, r = 0.05, type = "call"),
    0.2,
    tolerance = 1e-8
  )
})

test_that("implied volatility recovers the Black-Scholes input for a put", {
  price <- strategyr:::.calc_option_price_bs(100, 100, 1, 0.05, 0.2, type = "put")

  expect_equal(
    calc_option_iv(price = price, S = 100, K = 100, T = 1, r = 0.05, type = "put"),
    0.2,
    tolerance = 1e-8
  )
})
