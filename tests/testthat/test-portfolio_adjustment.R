library(testthat)
library(data.table)

test_that("plan_portfolio_adjustment computes target and delta units", {
  portfolio_state <- data.table(
    asset = c("BTC", "ETH"),
    price = c(100, 50),
    current_units = c(10, 20),
    target_weight = c(0.6, 0.4)
  )

  res <- plan_portfolio_adjustment(portfolio_state, equity = 3000)

  expect_true(data.table::is.data.table(res))
  expect_true(all(c("current_notional", "target_notional", "delta_notional", "target_units", "delta_units") %in% names(res)))
  expect_equal(res$target_notional, c(1800, 1200))
  expect_equal(res$target_units, c(18, 24))
  expect_equal(res$delta_units, c(8, 4))
})

test_that("build_order_intents converts deltas into buy and sell intents", {
  adjustment_plan <- data.table(
    asset = c("BTC", "ETH", "SOL"),
    price = c(100, 50, 25),
    delta_units = c(8, -4, 0)
  )

  intents <- build_order_intents(adjustment_plan)

  expect_true(data.table::is.data.table(intents))
  expect_equal(intents$asset, c("BTC", "ETH"))
  expect_equal(intents$side, c("buy", "sell"))
  expect_equal(intents$units, c(8, 4))
  expect_true(all(intents$intent_type == "rebalance"))
})
