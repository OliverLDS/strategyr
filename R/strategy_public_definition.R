.strategy_public_parameter <- function(name, value, unit, description) {
  list(
    name = name,
    value = value,
    unit = unit,
    description = description
  )
}

.strategy_public_definitions <- function() {
  list(
    buy_hold = list(
      schema_version = "1.0",
      id = "buy_hold",
      display_name = "Buy and Hold",
      target_function = "strat_buy_and_hold_tgt_pos",
      summary = "Maintains a constant target exposure through the full input history.",
      signal_rule = "Set the target position to the configured exposure on every completed bar.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC datetime"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("value", 1.0, "target exposure", "Constant target exposure.")
      )
    ),
    ema_cross = list(
      schema_version = "1.0",
      id = "ema_cross",
      display_name = "EMA Cross",
      target_function = "strat_ema_cross_tgt_pos",
      summary = "Uses fast and slow exponential moving averages with a low-volatility gate to set directional exposure.",
      signal_rule = "Trade the active EMA-cross direction only when the low-volatility gate and freshness rule allow it.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC high", "daily OHLC low", "daily OHLC close"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("fast", 20L, "bars", "Fast EMA window."),
        .strategy_public_parameter("slow", 50L, "bars", "Slow EMA window."),
        .strategy_public_parameter("low_atr_threshold", 5L, "percentile", "ATR percentile threshold for the low-volatility gate."),
        .strategy_public_parameter("freshness_floor", 18L, "bars", "Maximum signal age allowed for an active target."),
        .strategy_public_parameter("tp_ratio", 0.05, "return", "Take-profit guard ratio."),
        .strategy_public_parameter("sl_ratio", 0.02, "return", "Stop-loss guard ratio."),
        .strategy_public_parameter("atr_h", 12L, "half-life", "ATR half-life used by the volatility gate."),
        .strategy_public_parameter("atr_window", 300L, "bars", "Rolling window used by the ATR quantile.")
      )
    ),
    ema_cross_adx = list(
      schema_version = "1.0",
      id = "ema_cross_adx",
      display_name = "EMA Cross with ADX Filter",
      target_function = "strat_ema_cross_adx_tgt_pos",
      summary = "Uses EMA direction only when ADX indicates sufficient trend strength.",
      signal_rule = "Go long when the fast EMA is above the slow EMA and ADX is at or above the threshold, go short for the reverse, and stay flat otherwise.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC high", "daily OHLC low", "daily OHLC close"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("fast", 20L, "bars", "Fast EMA window."),
        .strategy_public_parameter("slow", 50L, "bars", "Slow EMA window."),
        .strategy_public_parameter("adx_n", 14L, "bars", "ADX window."),
        .strategy_public_parameter("adx_threshold", 20, "ADX level", "Minimum ADX level required for an active target."),
        .strategy_public_parameter("target_size", 1.0, "target exposure", "Absolute target exposure when a signal is active.")
      )
    ),
    ema_cross_slope_confirm = list(
      schema_version = "1.0",
      id = "ema_cross_slope_confirm",
      display_name = "EMA Cross with Slope Confirmation",
      target_function = "strat_ema_cross_slope_confirm_tgt_pos",
      summary = "Uses EMA alignment only when both EMA slopes confirm the same direction.",
      signal_rule = "Go long when the fast EMA is above the slow EMA and both slopes are positive, go short when the reverse is true, and stay flat otherwise.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC close"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("fast", 20L, "bars", "Fast EMA window."),
        .strategy_public_parameter("slow", 50L, "bars", "Slow EMA window."),
        .strategy_public_parameter("slope_lag", 1L, "bars", "Lag used to estimate EMA slope."),
        .strategy_public_parameter("target_size", 1.0, "target exposure", "Absolute target exposure when a signal is active.")
      )
    ),
    donchian_turtle = list(
      schema_version = "1.0",
      id = "donchian_turtle",
      display_name = "Donchian Turtle",
      target_function = "strat_donchian_turtle_tgt_pos",
      summary = "Uses a long Donchian breakout for entry and a shorter Donchian channel for exit.",
      signal_rule = "Enter long above the prior entry channel high, enter short below the prior entry channel low, and exit on the opposite shorter exit channel.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC high", "daily OHLC low", "daily OHLC close"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("entry_n", 55L, "bars", "Donchian entry window."),
        .strategy_public_parameter("exit_n", 20L, "bars", "Donchian exit window."),
        .strategy_public_parameter("target_size", 1.0, "target exposure", "Absolute target exposure when a signal is active.")
      )
    ),
    bollinger_revert = list(
      schema_version = "1.0",
      id = "bollinger_revert",
      display_name = "Bollinger Reversion",
      target_function = "strat_bollinger_revert_tgt_pos",
      summary = "Uses Bollinger band touches to open mean-reversion targets and closes near the middle band.",
      signal_rule = "Open long at or below the lower band, open short at or above the upper band, and close when price returns to the middle band.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC close"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("n", 20L, "bars", "Bollinger window."),
        .strategy_public_parameter("k", 2, "standard deviations", "Bollinger width multiplier."),
        .strategy_public_parameter("target_size", 1.0, "target exposure", "Absolute target exposure when a signal is active.")
      )
    ),
    rsi_revert = list(
      schema_version = "1.0",
      id = "rsi_revert",
      display_name = "RSI Reversion",
      target_function = "strat_rsi_revert_tgt_pos",
      summary = "Uses classic RSI levels to open mean-reversion targets and close them near neutral.",
      signal_rule = "Open long when RSI is oversold, open short when RSI is overbought, and close when RSI reaches the exit level.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC close"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("n", 14L, "bars", "RSI window."),
        .strategy_public_parameter("oversold", 30, "RSI level", "Oversold threshold for long entry."),
        .strategy_public_parameter("overbought", 70, "RSI level", "Overbought threshold for short entry."),
        .strategy_public_parameter("exit_level", 50, "RSI level", "Neutral level used to close open targets."),
        .strategy_public_parameter("target_size", 1.0, "target exposure", "Absolute target exposure when a signal is active.")
      )
    ),
    vol_target = list(
      schema_version = "1.0",
      id = "vol_target",
      display_name = "Vol Target",
      target_function = "strat_vol_target_tgt_pos",
      summary = "Sets trend direction from price versus EMA and scales exposure by realized volatility.",
      signal_rule = "Use price above the trend EMA for long direction, price below it for short direction, and cap size by target volatility versus realized volatility.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC close"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("trend_n", 20L, "bars", "EMA window used for the directional trend filter."),
        .strategy_public_parameter("rv_n", 20L, "bars", "Realized-volatility window."),
        .strategy_public_parameter("vol_target", 0.2, "annualized volatility", "Annualized target volatility."),
        .strategy_public_parameter("max_leverage", 1.0, "target exposure", "Maximum absolute target exposure."),
        .strategy_public_parameter("annualization", 252, "bars per year", "Annualization factor for realized volatility.")
      )
    ),
    regime_switch = list(
      schema_version = "1.0",
      id = "regime_switch",
      display_name = "Regime Switch",
      target_function = "strat_regime_switch_tgt_pos",
      summary = "Switches among trend, mean-reversion, and flat exposure using ADX and realized volatility state.",
      signal_rule = "Use EMA trend targets in strong non-high-volatility trend regimes, Bollinger reversion targets in weak non-high-volatility regimes, and flat exposure otherwise.",
      position_semantics = "Positive values are long exposure, negative values are short exposure, and zero is flat.",
      data_requirements = c("daily OHLC high", "daily OHLC low", "daily OHLC close"),
      rebalance_rule = "A changed target is eligible after a completed source bar.",
      parameters = list(
        .strategy_public_parameter("fast", 20L, "bars", "Fast EMA window for the trend regime."),
        .strategy_public_parameter("slow", 50L, "bars", "Slow EMA window for the trend regime."),
        .strategy_public_parameter("adx_n", 14L, "bars", "ADX window."),
        .strategy_public_parameter("rv_n", 20L, "bars", "Realized-volatility window."),
        .strategy_public_parameter("bb_n", 20L, "bars", "Bollinger window for the reversion regime."),
        .strategy_public_parameter("bb_k", 2, "standard deviations", "Bollinger width multiplier for the reversion regime."),
        .strategy_public_parameter("trend_adx_threshold", 25, "ADX level", "ADX level at or above which the trend regime is allowed."),
        .strategy_public_parameter("revert_adx_threshold", 18, "ADX level", "ADX level at or below which the reversion regime is allowed."),
        .strategy_public_parameter("high_vol_threshold", 0.4, "annualized volatility", "Maximum realized volatility allowed for active regimes."),
        .strategy_public_parameter("breadth_long_threshold", -Inf, "breadth value", "Minimum breadth value required for long trend states."),
        .strategy_public_parameter("breadth_short_threshold", Inf, "breadth value", "Maximum breadth value required for short trend states."),
        .strategy_public_parameter("annualization", 252, "bars per year", "Annualization factor for realized volatility."),
        .strategy_public_parameter("target_size", 1.0, "target exposure", "Absolute target exposure when a signal is active.")
      )
    )
  )
}

.strategy_monitor_record <- function(strategy_id, strategy_family, expected_regimes, regime_interpretation) {
  list(
    schema_version = "1.0",
    strategy_id = strategy_id,
    strategy_family = strategy_family,
    expected_regimes = as.character(expected_regimes),
    regime_interpretation = regime_interpretation
  )
}

.strategy_monitor_definitions <- function() {
  list(
    buy_hold = .strategy_monitor_record(
      strategy_id = "buy_hold",
      strategy_family = "baseline",
      expected_regimes = "regime_agnostic",
      regime_interpretation = "Maintains configured exposure without selecting a market regime."
    ),
    ema_cross_adx = .strategy_monitor_record(
      strategy_id = "ema_cross_adx",
      strategy_family = "trend",
      expected_regimes = "trending",
      regime_interpretation = "Designed to be active when EMA direction is supported by sufficient ADX trend strength."
    ),
    ema_cross_slope_confirm = .strategy_monitor_record(
      strategy_id = "ema_cross_slope_confirm",
      strategy_family = "trend",
      expected_regimes = "trending",
      regime_interpretation = "Designed to be active when EMA alignment and EMA slopes confirm the same direction."
    ),
    rsi_revert = .strategy_monitor_record(
      strategy_id = "rsi_revert",
      strategy_family = "mean_reversion",
      expected_regimes = "range_bound",
      regime_interpretation = "Designed to act on RSI exhaustion and mean reversion rather than persistent trend."
    ),
    vol_target = .strategy_monitor_record(
      strategy_id = "vol_target",
      strategy_family = "risk_control",
      expected_regimes = c("normal_volatility", "high_volatility"),
      regime_interpretation = "Scales directional exposure by realized volatility and caps target exposure."
    ),
    donchian_turtle = .strategy_monitor_record(
      strategy_id = "donchian_turtle",
      strategy_family = "trend",
      expected_regimes = "trending",
      regime_interpretation = "Designed to enter channel breakouts and exit through a shorter channel reversal."
    ),
    bollinger_revert = .strategy_monitor_record(
      strategy_id = "bollinger_revert",
      strategy_family = "mean_reversion",
      expected_regimes = "range_bound",
      regime_interpretation = "Designed to fade Bollinger band touches and close near the middle band."
    ),
    regime_switch = .strategy_monitor_record(
      strategy_id = "regime_switch",
      strategy_family = "adaptive",
      expected_regimes = c("adaptive", "trending", "range_bound", "normal_volatility", "high_volatility"),
      regime_interpretation = "Selects trend, mean-reversion, or flat behavior from ADX and volatility state."
    )
  )
}

#' Public Strategy Definition
#'
#' Returns the canonical public-safe deterministic strategy definition used by
#' Vox for a supported strategy id.
#'
#' @param id Character scalar strategy id. Supported ids are `"buy_hold"`,
#'   `"ema_cross"`, `"ema_cross_adx"`, `"ema_cross_slope_confirm"`,
#'   `"donchian_turtle"`, `"bollinger_revert"`, `"rsi_revert"`,
#'   `"vol_target"`, and `"regime_switch"`.
#'
#' @return A named list with schema version, public description, target
#'   function name, data requirements, rebalance rule, and effective default
#'   strategy parameters.
#' @export
strategy_public_definition <- function(id) {
  stopifnot(is.character(id), length(id) == 1L, !is.na(id))
  definitions <- .strategy_public_definitions()
  if (!id %in% names(definitions)) {
    stop(
      "Unsupported public strategy id: `", id, "`. Supported ids are: ",
      paste(names(definitions), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  definitions[[id]]
}

#' Strategy Monitor Definition
#'
#' Returns public-safe monitor metadata for a supported Vox strategy id. This
#' metadata is separate from execution and should be used by monitoring
#' consumers instead of inferring family or regime labels from names.
#'
#' @param strategy_id Character scalar strategy id. Supported ids are
#'   `"buy_hold"`, `"ema_cross_adx"`, `"ema_cross_slope_confirm"`,
#'   `"rsi_revert"`, `"vol_target"`, `"donchian_turtle"`,
#'   `"bollinger_revert"`, and `"regime_switch"`.
#'
#' @return A named list with schema version, strategy id, strategy family,
#'   expected regimes, and public regime interpretation.
#' @export
strategy_monitor_definition <- function(strategy_id) {
  stopifnot(is.character(strategy_id), length(strategy_id) == 1L, !is.na(strategy_id))
  definitions <- .strategy_monitor_definitions()
  if (!strategy_id %in% names(definitions)) {
    stop(
      "Unsupported monitor strategy id: `", strategy_id, "`. Supported ids are: ",
      paste(names(definitions), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  definitions[[strategy_id]]
}
