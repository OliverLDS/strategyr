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
    )
  )
}

#' Public Strategy Definition
#'
#' Returns the canonical public-safe deterministic strategy definition used by
#' Vox for a supported strategy id.
#'
#' @param id Character scalar strategy id. Supported ids are `"buy_hold"`,
#'   `"ema_cross"`, `"rsi_revert"`, and `"vol_target"`.
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
