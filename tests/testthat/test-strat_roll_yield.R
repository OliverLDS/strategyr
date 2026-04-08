library(testthat)
library(data.table)

test_that("strat_roll_yield_tgt_pos follows roll-yield sign on summarized input", {
  DT <- data.table(roll_yield_1_2 = c(-0.02, 0, 0.03))

  tgt_pos <- strat_roll_yield_tgt_pos(
    DT,
    rank_front = 1L,
    rank_deferred = 2L,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(-1, 0, 1))
})

test_that("strat_roll_yield_action_plan uses latest target on summarized input", {
  DT <- data.table(roll_yield_1_2 = c(0, 0.03))
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

  plan <- strat_roll_yield_action_plan(
    DT,
    state,
    rank_front = 1L,
    rank_deferred = 2L,
    compute_features = FALSE,
    strat_id = 604L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 604L)
})
