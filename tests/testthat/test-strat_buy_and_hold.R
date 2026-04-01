library(testthat)
library(data.table)

test_that("strat_buy_and_hold_tgt_pos returns full-length constant target", {
  DT <- data.table(datetime = c(0, 60, 120, 180))

  tgt_pos <- strat_buy_and_hold_tgt_pos(DT)

  expect_length(tgt_pos, nrow(DT))
  expect_true(is.numeric(tgt_pos))
  expect_equal(tgt_pos, rep(1.0, nrow(DT)))
})

test_that("strat_buy_and_hold_tgt_pos supports custom target size", {
  DT <- data.table(datetime = c(0, 60, 120))

  tgt_pos <- strat_buy_and_hold_tgt_pos(DT, value = 0.25)

  expect_equal(tgt_pos, rep(0.25, nrow(DT)))
})

test_that("strat_buy_and_hold_action_plan opens from flat state", {
  DT <- data.table(datetime = c(0, 60, 120))
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

  plan <- strat_buy_and_hold_action_plan(DT, state, value = 1.0, strat_id = 1L, tol_pos = 0)

  expect_equal(plan$n, 1L)
  expect_equal(length(plan$actions), 1L)
  expect_equal(plan$actions[[1]]$action, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
})

test_that("strat_buy_and_hold_action_plan respects tolerance", {
  DT <- data.table(datetime = c(0, 60, 120))
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

  plan <- strat_buy_and_hold_action_plan(DT, state, value = 0.1, strat_id = 1L, tol_pos = 0.5)

  expect_equal(plan$n, 0L)
  expect_equal(length(plan$actions), 0L)
})
