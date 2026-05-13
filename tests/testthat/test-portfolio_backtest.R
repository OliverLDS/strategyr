library(testthat)
library(data.table)

test_that("portfolio weight backtest produces equity and position paths", {
  DT <- make_test_portfolio_weight_panel()

  core_res <- backtest_portfolio_weights(
    DT,
    initial_equity = 10000,
    fee_rt = 0.0005
  )
  res <- backtest_portfolio_weights(
    DT,
    initial_equity = 10000,
    fee_rt = 0.0005,
    keep_positions = TRUE
  )

  expect_true(is.null(core_res$positions))
  expect_equal(core_res$equity$equity, res$equity$equity, tolerance = 1e-8)
  expect_equal(core_res$equity$fee_paid, res$equity$fee_paid, tolerance = 1e-8)
  expect_true(all(c("equity", "cash", "gross_exposure", "turnover", "fee_paid") %in% names(res$equity)))
  expect_equal(nrow(res$equity), data.table::uniqueN(DT$date))
  expect_true(all(is.finite(res$equity$equity)))
  expect_true(all(res$equity$equity > 0))
  expect_gt(sum(res$equity$fee_paid), 0)
  expect_true(all(c("date", "asset", "units", "target_weight", "close_weight") %in% names(res$positions)))
})

test_that("portfolio weight backtest enforces long-only mode", {
  DT <- make_test_portfolio_weight_panel()
  DT[1, target_weight := -0.2]

  expect_error(
    backtest_portfolio_weights(DT, allow_short = FALSE),
    "Negative target weights"
  )
})
