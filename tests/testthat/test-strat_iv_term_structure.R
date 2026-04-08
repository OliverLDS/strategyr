library(testthat)
library(data.table)

test_that("strat_iv_term_structure_tgt_pos thresholds summarized term structure", {
  DT <- data.table(iv_term_structure = c(-0.02, 0, 0.03))

  tgt_pos <- strat_iv_term_structure_tgt_pos(
    DT,
    long_threshold = 0.01,
    short_threshold = -0.01,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(-1, 0, 1))
})

test_that("strat_iv_term_structure_action_plan uses latest target", {
  DT <- data.table(iv_term_structure = c(0, -0.02))
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

  plan <- strat_iv_term_structure_action_plan(
    DT,
    state,
    compute_features = FALSE,
    strat_id = 702L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 702L)
})
