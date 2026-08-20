public_definition_ids <- c(
  "buy_hold",
  "ema_cross",
  "ema_cross_adx",
  "ema_cross_slope_confirm",
  "donchian_turtle",
  "bollinger_revert",
  "rsi_revert",
  "vol_target",
  "regime_switch"
)

monitor_definition_ids <- c(
  "buy_hold",
  "ema_cross_adx",
  "ema_cross_slope_confirm",
  "rsi_revert",
  "vol_target",
  "donchian_turtle",
  "bollinger_revert",
  "regime_switch"
)

param_value <- function(def, name) {
  idx <- vapply(def$parameters, function(x) identical(x$name, name), logical(1L))
  def$parameters[[which(idx)[[1L]]]]$value
}

param_names <- function(def) {
  vapply(def$parameters, `[[`, character(1L), "name")
}

expect_public_defaults_match_formals <- function(id, fun, excluded = c("DT", "compute_features", "debug")) {
  def <- strategy_public_definition(id)
  fmls <- formals(fun)
  expected_names <- setdiff(names(fmls), excluded)
  expect_equal(param_names(def), expected_names)
  for (nm in expected_names) {
    expected_value <- eval(fmls[[nm]])
    if (is.numeric(expected_value) && length(expected_value) == 1L && !is.finite(expected_value)) {
      expect_null(param_value(def, nm))
    } else {
      expect_equal(param_value(def, nm), expected_value)
    }
  }
}

test_that("public definitions support the expected ids", {
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
      expect_true(is.null(param$value) || (is.numeric(param$value) && length(param$value) == 1L && is.finite(param$value)))
      expect_type(param$unit, "character")
      expect_type(param$description, "character")
    }
  }
})

test_that("public definitions map to exact target functions", {
  expected <- c(
    buy_hold = "strat_buy_and_hold_tgt_pos",
    ema_cross = "strat_ema_cross_tgt_pos",
    ema_cross_adx = "strat_ema_cross_adx_tgt_pos",
    ema_cross_slope_confirm = "strat_ema_cross_slope_confirm_tgt_pos",
    donchian_turtle = "strat_donchian_turtle_tgt_pos",
    bollinger_revert = "strat_bollinger_revert_tgt_pos",
    rsi_revert = "strat_rsi_revert_tgt_pos",
    vol_target = "strat_vol_target_tgt_pos",
    regime_switch = "strat_regime_switch_tgt_pos"
  )

  actual <- vapply(public_definition_ids, function(id) {
    strategy_public_definition(id)$target_function
  }, character(1L))

  expect_equal(actual, expected)
})

test_that("public definitions precisely match executable target defaults", {
  expect_public_defaults_match_formals("buy_hold", strat_buy_and_hold_tgt_pos)
  expect_public_defaults_match_formals("ema_cross", strat_ema_cross_tgt_pos)
  expect_public_defaults_match_formals("ema_cross_adx", strat_ema_cross_adx_tgt_pos)
  expect_public_defaults_match_formals("ema_cross_slope_confirm", strat_ema_cross_slope_confirm_tgt_pos)
  expect_public_defaults_match_formals("donchian_turtle", strat_donchian_turtle_tgt_pos)
  expect_public_defaults_match_formals("bollinger_revert", strat_bollinger_revert_tgt_pos)
  expect_public_defaults_match_formals("rsi_revert", strat_rsi_revert_tgt_pos)
  expect_public_defaults_match_formals("vol_target", strat_vol_target_tgt_pos)
  expect_public_defaults_match_formals("regime_switch", strat_regime_switch_tgt_pos, excluded = c("DT", "breadth_col", "compute_features", "debug"))
})

test_that("unbounded public parameters are explicitly nullable", {
  def <- strategy_public_definition("regime_switch")

  expect_null(param_value(def, "breadth_long_threshold"))
  expect_null(param_value(def, "breadth_short_threshold"))
  expect_match(
    def$parameters[[which(param_names(def) == "breadth_long_threshold")[[1L]]]]$description,
    "Disabled by default"
  )
  expect_match(
    def$parameters[[which(param_names(def) == "breadth_short_threshold")[[1L]]]]$description,
    "Disabled by default"
  )
})

test_that("public definitions serialize to JSON without non-finite values", {
  for (id in public_definition_ids) {
    json <- jsonlite::toJSON(strategy_public_definition(id), auto_unbox = TRUE, null = "null")
    expect_false(grepl("Inf|NaN", json, fixed = FALSE))
  }
})

test_that("public definitions reject unsupported ids clearly", {
  expect_error(
    strategy_public_definition("macd_cross"),
    "Unsupported public strategy id"
  )
})

test_that("monitor definitions cover all intended Vox strategies", {
  defs <- lapply(monitor_definition_ids, strategy_monitor_definition)
  expect_equal(vapply(defs, `[[`, character(1L), "strategy_id"), monitor_definition_ids)
})

test_that("monitor definitions use valid public enums and array regimes", {
  valid_families <- c("baseline", "trend", "mean_reversion", "risk_control", "adaptive")
  valid_regimes <- c(
    "trending",
    "range_bound",
    "high_volatility",
    "normal_volatility",
    "adaptive",
    "regime_agnostic"
  )
  required_names <- c(
    "schema_version",
    "strategy_id",
    "strategy_family",
    "expected_regimes",
    "regime_interpretation"
  )

  for (id in monitor_definition_ids) {
    def <- strategy_monitor_definition(id)
    expect_named(def, required_names)
    expect_equal(def$schema_version, "1.0")
    expect_equal(def$strategy_id, id)
    expect_true(def$strategy_family %in% valid_families)
    expect_type(def$expected_regimes, "character")
    expect_gt(length(def$expected_regimes), 0L)
    expect_true(all(def$expected_regimes %in% valid_regimes))
    expect_type(def$regime_interpretation, "character")
  }
})

test_that("monitor definitions do not expose execution or internal implementation data", {
  private_fields <- c(
    "target_function",
    "parameters",
    "signal_rule",
    "data_requirements",
    "rebalance_rule",
    "compute_features",
    "debug",
    "feature_cols"
  )

  for (id in monitor_definition_ids) {
    def <- strategy_monitor_definition(id)
    expect_false(any(private_fields %in% names(def)))
  }
})

test_that("monitor definitions reject unsupported ids clearly", {
  expect_error(
    strategy_monitor_definition("ema_cross"),
    "Unsupported monitor strategy id"
  )
})
