library(testthat)
library(data.table)

test_that("strat_fx_carry_basket_rank_tgt_pos ranks carry cross-sectionally", {
  DT <- data.table(
    date = rep(as.Date("2020-01-01") + 0:1, each = 4),
    asset = rep(c("AAA", "BBB", "CCC", "DDD"), times = 2),
    fx_carry = c(4, 3, 2, 1, 1, 2, 3, 4)
  )

  tgt_pos <- strat_fx_carry_basket_rank_tgt_pos(DT, long_n = 1L, short_n = 1L, gross_exposure = 1.0)
  dbg <- strat_fx_carry_basket_rank_tgt_pos(DT, long_n = 1L, short_n = 1L, gross_exposure = 1.0, debug = TRUE)

  expect_equal(sum(tgt_pos[DT$date == DT$date[1]]), 0)
  expect_equal(sum(tgt_pos[DT$date == DT$date[2]]), 0)
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_fx_carry_basket_rank_action_plan returns portfolio outputs", {
  DT <- data.table(
    date = rep(as.Date("2020-01-01") + 0:1, each = 4),
    asset = rep(c("AAA", "BBB", "CCC", "DDD"), times = 2),
    fx_carry = c(4, 3, 2, 1, 1, 2, 3, 4)
  )

  out <- strat_fx_carry_basket_rank_action_plan(DT, make_test_portfolio_state(), equity = 10000, long_n = 1L, short_n = 1L, strat_id = 511L)
  expect_true(is.list(out))
  expect_true(data.table::is.data.table(out$adjustment_plan))
  expect_true(all(out$order_intents$strat_id == 511L))
})
