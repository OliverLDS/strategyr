library(testthat)
library(data.table)

test_that("strat_rsi_revert_tgt_pos enters on RSI extremes and exits at neutral", {
  DT <- data.table(rsi_14 = c(25, 55, 75, 45))

  tgt_pos <- strat_rsi_revert_tgt_pos(
    DT,
    n = 14L,
    oversold = 30,
    overbought = 70,
    exit_level = 50,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(1, 0, -1, 0))
})

test_that("strat_rsi_revert_action_plan uses latest target", {
  DT <- data.table(rsi_14 = c(50, 75))
  state <- list(
    ctr_size = 1.0,
    ctr_step = 1.0,
    lev = 10.0,
    last_px = 100.0,
    ctr_unit = 0.0,
    avg_price = NaN,
    cash = 10000.0,
    pos_dir = 0L
  )

  plan <- strat_rsi_revert_action_plan(
    DT,
    state,
    n = 14L,
    oversold = 30,
    overbought = 70,
    exit_level = 50,
    compute_features = FALSE,
    strat_id = 303L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 303L)
})

test_that("strat_rsi_logr_revert_tgt_pos enters on log-return RSI extremes", {
  DT <- data.table(rsi_logr_12 = c(25, 55, 75, 45))

  tgt_pos <- strat_rsi_logr_revert_tgt_pos(
    DT,
    h = 12,
    oversold = 30,
    overbought = 70,
    exit_level = 50,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(1, 0, -1, 0))
})

test_that("strat_rsi_logr_revert_tgt_pos can compute log-return RSI features", {
  DT <- data.table(close = exp(seq(log(100), log(120), length.out = 40)))

  res <- strat_rsi_logr_revert_tgt_pos(DT, h = 12, debug = TRUE)

  expect_true("rsi_logr_12" %in% names(DT))
  expect_equal(res$feature_cols, "rsi_logr_12")
  expect_length(res$tgt_pos, nrow(DT))
})

test_that("strat_rsi_logr_revert_action_plan uses latest target", {
  DT <- data.table(rsi_logr_12 = c(50, 75))
  state <- list(
    ctr_size = 1.0,
    ctr_step = 1.0,
    lev = 10.0,
    last_px = 100.0,
    ctr_unit = 0.0,
    avg_price = NaN,
    cash = 10000.0,
    pos_dir = 0L
  )

  plan <- strat_rsi_logr_revert_action_plan(
    DT,
    state,
    h = 12,
    oversold = 30,
    overbought = 70,
    exit_level = 50,
    compute_features = FALSE,
    strat_id = 306L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 306L)
})
