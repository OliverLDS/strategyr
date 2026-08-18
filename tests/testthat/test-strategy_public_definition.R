public_definition_ids <- c("buy_hold", "ema_cross", "rsi_revert", "vol_target")

param_value <- function(def, name) {
  idx <- vapply(def$parameters, function(x) identical(x$name, name), logical(1L))
  def$parameters[[which(idx)[[1L]]]]$value
}

test_that("public definitions support the four Phase 1 ids", {
  defs <- lapply(public_definition_ids, strategy_public_definition)
  expect_equal(vapply(defs, `[[`, character(1L), "id"), public_definition_ids)
})

test_that("public definitions use the stable schema", {
  required_names <- c(
    "schema_version",
    "id",
    "display_name",
    "target_function",
    "summary",
    "signal_rule",
    "position_semantics",
    "data_requirements",
    "rebalance_rule",
    "parameters"
  )

  for (id in public_definition_ids) {
    def <- strategy_public_definition(id)
    expect_named(def, required_names)
    expect_equal(def$schema_version, "1.0")
    expect_type(def$id, "character")
    expect_type(def$display_name, "character")
    expect_type(def$target_function, "character")
    expect_type(def$summary, "character")
    expect_type(def$signal_rule, "character")
    expect_type(def$position_semantics, "character")
    expect_type(def$data_requirements, "character")
    expect_type(def$rebalance_rule, "character")
    expect_type(def$parameters, "list")
    expect_gt(length(def$parameters), 0L)

    for (param in def$parameters) {
      expect_named(param, c("name", "value", "unit", "description"))
      expect_type(param$name, "character")
      expect_type(param$unit, "character")
      expect_type(param$description, "character")
    }
  }
})

test_that("public definitions map to exact target functions", {
  expected <- c(
    buy_hold = "strat_buy_and_hold_tgt_pos",
    ema_cross = "strat_ema_cross_tgt_pos",
    rsi_revert = "strat_rsi_revert_tgt_pos",
    vol_target = "strat_vol_target_tgt_pos"
  )

  actual <- vapply(public_definition_ids, function(id) {
    strategy_public_definition(id)$target_function
  }, character(1L))

  expect_equal(actual, expected)
})

test_that("public definitions expose key default parameter values", {
  buy_hold <- strategy_public_definition("buy_hold")
  ema_cross <- strategy_public_definition("ema_cross")
  rsi_revert <- strategy_public_definition("rsi_revert")
  vol_target <- strategy_public_definition("vol_target")

  expect_equal(param_value(buy_hold, "value"), 1.0)

  expect_equal(param_value(ema_cross, "fast"), 20L)
  expect_equal(param_value(ema_cross, "slow"), 50L)
  expect_equal(param_value(ema_cross, "low_atr_threshold"), 5L)
  expect_equal(param_value(ema_cross, "freshness_floor"), 18L)
  expect_equal(param_value(ema_cross, "tp_ratio"), 0.05)
  expect_equal(param_value(ema_cross, "sl_ratio"), 0.02)
  expect_equal(param_value(ema_cross, "atr_h"), 12L)
  expect_equal(param_value(ema_cross, "atr_window"), 300L)

  expect_equal(param_value(rsi_revert, "n"), 14L)
  expect_equal(param_value(rsi_revert, "oversold"), 30)
  expect_equal(param_value(rsi_revert, "overbought"), 70)
  expect_equal(param_value(rsi_revert, "exit_level"), 50)
  expect_equal(param_value(rsi_revert, "target_size"), 1.0)

  expect_equal(param_value(vol_target, "trend_n"), 20L)
  expect_equal(param_value(vol_target, "rv_n"), 20L)
  expect_equal(param_value(vol_target, "vol_target"), 0.2)
  expect_equal(param_value(vol_target, "max_leverage"), 1.0)
  expect_equal(param_value(vol_target, "annualization"), 252)
})

test_that("public definitions reject unsupported ids clearly", {
  expect_error(
    strategy_public_definition("macd_cross"),
    "Unsupported public strategy id"
  )
})
