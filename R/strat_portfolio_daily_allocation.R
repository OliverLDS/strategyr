.validate_portfolio_daily_ohlc <- function(DT, date_col, asset_col, open_col, high_col, low_col, close_col) {
  stopifnot(data.table::is.data.table(DT))
  required <- c(date_col, asset_col, open_col, high_col, low_col, close_col)
  .validate_market_dt(DT, required)

  if (anyNA(DT[[date_col]]) || anyNA(DT[[asset_col]])) {
    stop("Portfolio input cannot contain missing dates or asset identifiers.", call. = FALSE)
  }
  if (anyDuplicated(DT[, .(get(date_col), get(asset_col))])) {
    stop("Portfolio input must contain at most one row per date and asset.", call. = FALSE)
  }

  for (col in c(open_col, high_col, low_col, close_col)) {
    x <- DT[[col]]
    if (!is.numeric(x) || any(!is.finite(x) | x <= 0)) {
      stop("Daily OHLC columns must contain finite positive values.", call. = FALSE)
    }
  }
  if (any(DT[[low_col]] > pmin(DT[[open_col]], DT[[close_col]])) ||
      any(DT[[high_col]] < pmax(DT[[open_col]], DT[[close_col]]))) {
    stop("Daily OHLC values must satisfy low <= open/close <= high.", call. = FALSE)
  }

  invisible(required)
}

.validate_portfolio_allocation_parameters <- function(rebalance_n, min_obs, gross_exposure, weight_cap) {
  stopifnot(length(rebalance_n) == 1L, is.finite(rebalance_n), rebalance_n >= 1, rebalance_n == as.integer(rebalance_n))
  stopifnot(length(min_obs) == 1L, is.finite(min_obs), min_obs >= 1, min_obs == as.integer(min_obs))
  stopifnot(length(gross_exposure) == 1L, is.finite(gross_exposure), gross_exposure >= 0, gross_exposure <= 1)
  stopifnot(length(weight_cap) == 1L, is.finite(weight_cap), weight_cap > 0, weight_cap <= 1)
}

.capped_long_weights <- function(score, gross_exposure, weight_cap) {
  out <- rep(0.0, length(score))
  valid <- which(is.finite(score) & score > 0)
  if (!length(valid) || gross_exposure <= 0) {
    return(out)
  }

  target_gross <- min(gross_exposure, length(valid) * weight_cap)
  remaining <- valid
  remaining_gross <- target_gross
  while (length(remaining) && remaining_gross > 0) {
    score_sum <- sum(score[remaining])
    proposed <- if (score_sum > 0) score[remaining] / score_sum * remaining_gross else rep(remaining_gross / length(remaining), length(remaining))
    capped <- proposed >= weight_cap
    if (!any(capped)) {
      out[remaining] <- proposed
      break
    }
    capped_idx <- remaining[capped]
    out[capped_idx] <- weight_cap
    remaining_gross <- remaining_gross - sum(out[capped_idx])
    remaining <- remaining[!capped]
  }
  out[!is.finite(out)] <- 0.0
  out
}

.portfolio_asset_history <- function(DT, date_col, asset_col, close_col) {
  assets <- sort(unique(as.character(DT[[asset_col]])))
  histories <- stats::setNames(vector("list", length(assets)), assets)
  for (asset in assets) {
    rows <- which(as.character(DT[[asset_col]]) == asset)
    rows <- rows[order(DT[[date_col]][rows])]
    asset_dates <- DT[[date_col]][rows]
    histories[[asset]] <- list(
      date = asset_dates,
      date_num = as.numeric(asset_dates),
      close = DT[[close_col]][rows]
    )
  }
  histories
}

.portfolio_history_index <- function(history, date_value) {
  findInterval(as.numeric(date_value), history$date_num)
}

.portfolio_volatility <- function(close, idx, vol_n, min_obs, annualization) {
  if (idx <= 1L) {
    return(NA_real_)
  }
  start <- max(1L, idx - vol_n)
  returns <- close[(start + 1L):idx] / close[start:(idx - 1L)] - 1
  returns <- returns[is.finite(returns)]
  if (length(returns) < min_obs) {
    return(NA_real_)
  }
  value <- stats::sd(returns) * sqrt(annualization)
  if (is.finite(value) && value > 0) value else NA_real_
}

.portfolio_allocation_path <- function(DT, date_col, asset_col, signal_fun, rebalance_n) {
  work_dt <- data.table::copy(DT)
  data.table::setkeyv(work_dt, c(date_col, asset_col))
  dates <- sort(unique(work_dt[[date_col]]))
  assets <- sort(unique(as.character(work_dt[[asset_col]])))
  date_run <- rle(match(work_dt[[date_col]], dates))
  row_end <- cumsum(date_run$lengths)
  row_start <- c(1L, head(row_end, -1L) + 1L)
  target_map <- stats::setNames(rep(0.0, length(assets)), assets)
  pending_rebalance <- FALSE
  pending_signal_idx <- NA_integer_

  work_dt[, target_weight := 0.0]
  work_dt[, eligible := FALSE]
  work_dt[, rebalance_due := FALSE]
  signal_idx <- rep(NA_integer_, nrow(work_dt))

  for (date_idx in seq_along(dates)) {
    date_value <- dates[[date_idx]]
    day_rows <- row_start[[date_idx]]:row_end[[date_idx]]
    day_assets <- as.character(work_dt[[asset_col]][day_rows])

    # Targets set from yesterday's completed bar execute at today's open.
    data.table::set(work_dt, i = day_rows, j = "target_weight", value = unname(target_map[day_assets]))
    data.table::set(work_dt, i = day_rows, j = "rebalance_due", value = pending_rebalance)
    signal_idx[day_rows] <- pending_signal_idx

    # Missing rows represent unavailable assets; do not revive stale targets.
    target_map[setdiff(names(target_map), day_assets)] <- 0.0

    due_today <- (date_idx - 1L) %% as.integer(rebalance_n) == 0L
    pending_rebalance <- FALSE
    if (!due_today) {
      next
    }

    signal <- signal_fun(day_assets, date_value)
    target_map[] <- 0.0
    target_map[day_assets] <- signal$weight
    data.table::set(work_dt, i = day_rows, j = "eligible", value = signal$eligible)
    pending_rebalance <- TRUE
    pending_signal_idx <- date_idx
  }

  data.table::set(work_dt, j = "signal_date", value = dates[signal_idx])
  work_dt[, target_weight := as.numeric(target_weight)]
  if (any(!is.finite(work_dt$target_weight))) {
    stop("Portfolio strategy generated non-finite target weights.", call. = FALSE)
  }
  if (any(work_dt$target_weight < 0) || any(work_dt$target_weight > 1)) {
    stop("Portfolio strategy generated invalid long-only target weights.", call. = FALSE)
  }
  gross_by_date <- work_dt[, sum(abs(target_weight)), by = date_col][["V1"]]
  if (any(gross_by_date > 1 + 1e-12)) {
    stop("Portfolio strategy exceeded unit gross exposure.", call. = FALSE)
  }
  work_dt[]
}

#' Equal-Weight-Rebalance Target Weights
#'
#' Generates a long-only daily portfolio target-weight path that allocates
#' equally across assets with enough observed history. Signals are formed from
#' completed bars and shifted one eligible open forward for execution.
#'
#' @param DT Long daily OHLC `data.table` with one row per date and asset.
#' @param date_col Date column name.
#' @param asset_col Asset identifier column name.
#' @param open_col Daily open-price column name.
#' @param high_col Daily high-price column name.
#' @param low_col Daily low-price column name.
#' @param close_col Daily close-price column name.
#' @param rebalance_n Number of completed daily bars between rebalance signals.
#' @param min_obs Minimum observed closes required for eligibility.
#' @param gross_exposure Maximum long gross exposure. Residual equity remains cash.
#' @param weight_cap Maximum weight assigned to one asset.
#'
#' @return A copy of `DT` with `target_weight`, `eligible`, `rebalance_due`,
#'   and `signal_date`. Each row's target is executable at that row's open.
#' @export
strat_equal_weight_rebalance_target_weights <- function(DT, date_col = "date", asset_col = "asset", open_col = "open", high_col = "high", low_col = "low", close_col = "close", rebalance_n = 21L, min_obs = 1L, gross_exposure = 1.0, weight_cap = 1.0) {
  .validate_portfolio_daily_ohlc(DT, date_col, asset_col, open_col, high_col, low_col, close_col)
  .validate_portfolio_allocation_parameters(rebalance_n, min_obs, gross_exposure, weight_cap)
  history <- .portfolio_asset_history(DT, date_col, asset_col, close_col)

  .portfolio_allocation_path(DT, date_col, asset_col, function(day_assets, date_value) {
    eligible <- vapply(day_assets, function(asset) .portfolio_history_index(history[[asset]], date_value) >= min_obs, logical(1L))
    list(weight = .capped_long_weights(as.numeric(eligible), gross_exposure, weight_cap), eligible = eligible)
  }, rebalance_n)
}

#' Inverse-Volatility-Allocation Target Weights
#'
#' Generates long-only daily portfolio targets proportional to inverse realized
#' volatility. Signals are formed from completed bars and shifted one eligible
#' open forward for execution.
#'
#' @inheritParams strat_equal_weight_rebalance_target_weights
#' @param vol_n Realized-volatility lookback in daily returns.
#' @param annualization Number of daily bars per year.
#'
#' @return A copy of `DT` with portfolio target-weight contract columns.
#' @export
strat_inverse_volatility_allocation_target_weights <- function(DT, date_col = "date", asset_col = "asset", open_col = "open", high_col = "high", low_col = "low", close_col = "close", vol_n = 20L, min_obs = 20L, annualization = 252, rebalance_n = 21L, gross_exposure = 1.0, weight_cap = 0.4) {
  .validate_portfolio_daily_ohlc(DT, date_col, asset_col, open_col, high_col, low_col, close_col)
  .validate_portfolio_allocation_parameters(rebalance_n, min_obs, gross_exposure, weight_cap)
  stopifnot(length(vol_n) == 1L, is.finite(vol_n), vol_n >= 1, vol_n == as.integer(vol_n))
  stopifnot(length(annualization) == 1L, is.finite(annualization), annualization > 0)
  history <- .portfolio_asset_history(DT, date_col, asset_col, close_col)

  .portfolio_allocation_path(DT, date_col, asset_col, function(day_assets, date_value) {
    score <- vapply(day_assets, function(asset) {
      h <- history[[asset]]
      idx <- .portfolio_history_index(h, date_value)
      if (idx < min_obs) return(NA_real_)
      vol <- .portfolio_volatility(h$close, idx, as.integer(vol_n), max(1L, as.integer(min_obs) - 1L), annualization)
      if (is.finite(vol) && vol > 0) 1 / vol else NA_real_
    }, numeric(1L))
    list(weight = .capped_long_weights(score, gross_exposure, weight_cap), eligible = is.finite(score))
  }, rebalance_n)
}

#' Cross-Asset-Trend-Allocation Target Weights
#'
#' Allocates across assets with positive medium-term momentum. Optional inverse
#' volatility scaling is applied only after the positive-trend filter. Signals
#' are formed from completed bars and shifted one eligible open forward.
#'
#' @inheritParams strat_inverse_volatility_allocation_target_weights
#' @param trend_n Medium-term momentum lookback in daily bars.
#' @param volatility_scale Logical; when `TRUE`, positive-trend weights are
#'   proportional to inverse realized volatility.
#'
#' @return A copy of `DT` with portfolio target-weight contract columns.
#' @export
strat_cross_asset_trend_allocation_target_weights <- function(DT, date_col = "date", asset_col = "asset", open_col = "open", high_col = "high", low_col = "low", close_col = "close", trend_n = 126L, vol_n = 20L, min_obs = 126L, annualization = 252, volatility_scale = TRUE, rebalance_n = 21L, gross_exposure = 1.0, weight_cap = 0.4) {
  .validate_portfolio_daily_ohlc(DT, date_col, asset_col, open_col, high_col, low_col, close_col)
  .validate_portfolio_allocation_parameters(rebalance_n, min_obs, gross_exposure, weight_cap)
  stopifnot(length(trend_n) == 1L, is.finite(trend_n), trend_n >= 1, trend_n == as.integer(trend_n))
  stopifnot(length(vol_n) == 1L, is.finite(vol_n), vol_n >= 1, vol_n == as.integer(vol_n))
  stopifnot(length(annualization) == 1L, is.finite(annualization), annualization > 0)
  stopifnot(is.logical(volatility_scale), length(volatility_scale) == 1L, !is.na(volatility_scale))
  history <- .portfolio_asset_history(DT, date_col, asset_col, close_col)
  required_obs <- max(as.integer(min_obs), as.integer(trend_n) + 1L)

  .portfolio_allocation_path(DT, date_col, asset_col, function(day_assets, date_value) {
    score <- vapply(day_assets, function(asset) {
      h <- history[[asset]]
      idx <- .portfolio_history_index(h, date_value)
      if (idx < required_obs) return(NA_real_)
      momentum <- h$close[[idx]] / h$close[[idx - as.integer(trend_n)]] - 1
      if (!is.finite(momentum) || momentum <= 0) return(NA_real_)
      if (!volatility_scale) return(1.0)
      vol <- .portfolio_volatility(h$close, idx, as.integer(vol_n), min(as.integer(vol_n), idx - 1L), annualization)
      if (is.finite(vol) && vol > 0) 1 / vol else NA_real_
    }, numeric(1L))
    list(weight = .capped_long_weights(score, gross_exposure, weight_cap), eligible = is.finite(score))
  }, rebalance_n)
}
