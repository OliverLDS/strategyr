library(testthat)
library(data.table)

test_that("strat_ema_cross_tgt_pos works with precomputed features", {
  DT <- data.table(
    close = c(100, 101, 102, 103),
    ema_20 = c(100, 101, 102, 103),
    ema_50 = c(101, 100, 99, 98),
    low_atr_5 = c(TRUE, TRUE, TRUE, TRUE)
  )

  tgt_pos <- strat_ema_cross_tgt_pos(
    DT,
    fast = 20L,
    slow = 50L,
    low_atr_threshold = 5L,
    compute_features = FALSE
  )

  expect_length(tgt_pos, nrow(DT))
  expect_true(is.numeric(tgt_pos))
})

test_that("strat_ema_cross_tgt_pos computes missing features", {
  DT <- data.table(
    datetime = seq(0, by = 60, length.out = 400),
    high = seq(100, 499),
    low = seq(99, 498),
    close = seq(99.5, 498.5)
  )

  tgt_pos <- strat_ema_cross_tgt_pos(
    DT,
    fast = 20L,
    slow = 50L,
    low_atr_threshold = 5L,
    compute_features = TRUE,
    atr_h = 12L,
    atr_window = 300L
  )

  expect_length(tgt_pos, nrow(DT))
  expect_true(all(c("ema_20", "ema_50", "atr_logr_12", "atr_q_5_12_300", "low_atr_5", "tgt_pos_20_50_5") %in% names(DT)))
})

test_that("strat_ema_cross_action_plan uses latest target", {
  DT <- data.table(
    close = c(100, 101, 102, 103),
    ema_20 = c(100, 101, 102, 103),
    ema_50 = c(101, 100, 99, 98),
    low_atr_5 = c(TRUE, TRUE, TRUE, TRUE)
  )
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

  plan <- strat_ema_cross_action_plan(
    DT,
    state,
    fast = 20L,
    slow = 50L,
    low_atr_threshold = 5L,
    compute_features = FALSE,
    strat_id = 101L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_true(plan$n %in% c(0L, 1L, 2L))
})
