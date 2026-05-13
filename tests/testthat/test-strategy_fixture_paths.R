library(testthat)
library(data.table)

test_that("regime OHLC fixture drives non-trivial path-dependent signals", {
  DT <- make_test_regime_ohlc(180L)

  turtle <- strat_donchian_turtle_tgt_pos(
    data.table::copy(DT),
    entry_n = 20L,
    exit_n = 8L,
    target_size = 0.75
  )
  atr_stop <- strat_atr_breakout_trailing_stop_tgt_pos(
    data.table::copy(DT),
    n = 14L,
    atr_mult = 0.8,
    trail_mult = 1.5,
    target_size = 0.75
  )

  expect_equal(length(turtle), nrow(DT))
  expect_equal(length(atr_stop), nrow(DT))
  expect_true(any(turtle != 0))
  expect_true(any(atr_stop != 0))
  expect_true(length(unique(turtle)) > 1L)
  expect_true(length(unique(atr_stop)) > 1L)
})

test_that("spread reversion fixture produces entry and exit path changes", {
  DT <- make_test_spread_reversion_market(160L)
  tgt <- strat_pair_spread_revert_tgt_pos(
    DT,
    z_n = 20L,
    entry_z = 1.2,
    exit_z = 0.2,
    target_size = 0.5
  )

  expect_equal(length(tgt), nrow(DT))
  expect_true(any(tgt > 0))
  expect_true(any(tgt < 0))
  expect_true(any(abs(diff(tgt)) > 0))
})

test_that("panel fixtures support portfolio and option strategy paths", {
  portfolio_dt <- make_test_portfolio_weight_panel()
  curve_dt <- make_test_futures_curve()
  option_dt <- make_test_option_surface()

  expect_true(all(c("date", "asset", "open", "close", "target_weight") %in% names(portfolio_dt)))
  expect_true(all(c("date", "contract_rank", "close", "time_to_expiry") %in% names(curve_dt)))
  expect_true(all(c("date", "T", "type", "option_log_forward_moneyness", "iv", "close") %in% names(option_dt)))

  roll_tgt <- strat_roll_yield_mean_revert_tgt_pos(curve_dt, z_n = 10L)
  iv_tgt <- strat_iv_directional_overlay_tgt_pos(option_dt, trend_n = 10L)

  expect_true(is.numeric(roll_tgt))
  expect_true(is.numeric(iv_tgt))
  expect_gt(length(roll_tgt), 0L)
  expect_gt(length(iv_tgt), 0L)
})
