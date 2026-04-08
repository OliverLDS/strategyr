library(testthat)
library(data.table)

test_that("calc_position_greeks scales Greeks by units and contract size", {
  option_state <- data.table(
    asset = c("SPY_C", "SPY_P"),
    S = c(100, 100),
    K = c(100, 100),
    T = c(1, 1),
    r = c(0.05, 0.05),
    sigma = c(0.2, 0.2),
    type = c("call", "put"),
    units = c(2, -3),
    contract_size = c(100, 100)
  )

  out <- calc_position_greeks(option_state)

  expect_s3_class(out, "data.table")
  expect_length(
    setdiff(c("price", "delta", "gamma", "vega", "theta", "rho", "delta_pos", "gamma_pos", "vega_pos", "theta_pos", "rho_pos"), names(out)),
    0
  )
  expect_equal(out$delta_pos[1], out$units[1] * out$contract_size[1] * out$delta[1], tolerance = 1e-12)
  expect_equal(out$delta_pos[2], out$units[2] * out$contract_size[2] * out$delta[2], tolerance = 1e-12)
  expect_gt(out$delta_pos[2], 0)
})

test_that("calc_option_risk_state returns a compact option snapshot", {
  out <- calc_option_risk_state(
    S = 100,
    K = 100,
    T = 1,
    r = 0.05,
    sigma = 0.2,
    type = "call"
  )

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 1)
  expect_length(
    setdiff(c("price", "market_price", "model_price", "iv", "iv_source", "type", "delta", "gamma", "vega", "theta", "rho"), names(out)),
    0
  )
  expect_equal(out$iv, 0.2, tolerance = 1e-12)
  expect_equal(out$iv_source, "input_sigma")
  expect_equal(out$price, out$model_price, tolerance = 1e-12)
  expect_equal(out$market_price, out$model_price, tolerance = 1e-12)
  expect_equal(out$type, "call")
})

test_that("calc_option_risk_state can infer implied volatility from price", {
  price <- strategyr:::.calc_option_price_bs(100, 100, 1, 0.05, 0.2, type = "put")
  out <- calc_option_risk_state(
    S = 100,
    K = 100,
    T = 1,
    r = 0.05,
    sigma = NULL,
    type = "put",
    price = price
  )

  expect_equal(out$iv, 0.2, tolerance = 1e-8)
  expect_equal(out$price, price, tolerance = 1e-12)
  expect_equal(out$market_price, price, tolerance = 1e-12)
  expect_equal(out$iv_source, "inferred_from_price")
})

test_that("plan_delta_neutral_adjustment solves the required hedge units", {
  out <- plan_delta_neutral_adjustment(
    current_delta = 250,
    target_delta = 0,
    hedge_delta = -50
  )

  expect_s3_class(out, "data.table")
  expect_equal(out$delta_gap, -250)
  expect_equal(out$hedge_units, 5)
  expect_equal(out$hedge_units_abs, 5)
  expect_equal(out$hedge_action, "buy")
})

test_that("plan_delta_neutral_adjustment returns flat when already aligned", {
  out <- plan_delta_neutral_adjustment(
    current_delta = 0,
    target_delta = 0,
    hedge_delta = -50
  )

  expect_equal(out$delta_gap, 0)
  expect_equal(out$hedge_units, 0)
  expect_equal(out$hedge_units_abs, 0)
  expect_equal(out$hedge_action, "flat")
})

test_that("plan_vega_target_adjustment solves the required hedge units", {
  out <- plan_vega_target_adjustment(
    current_vega = 1200,
    target_vega = 0,
    hedge_vega = -300
  )

  expect_s3_class(out, "data.table")
  expect_equal(out$vega_gap, -1200)
  expect_equal(out$hedge_units, 4)
  expect_equal(out$hedge_units_abs, 4)
  expect_equal(out$hedge_action, "buy")
})

test_that("plan_vega_target_adjustment handles positive hedge exposure", {
  out <- plan_vega_target_adjustment(
    current_vega = 1200,
    target_vega = 0,
    hedge_vega = 300
  )

  expect_equal(out$vega_gap, -1200)
  expect_equal(out$hedge_units, -4)
  expect_equal(out$hedge_units_abs, 4)
  expect_equal(out$hedge_action, "sell")
})
