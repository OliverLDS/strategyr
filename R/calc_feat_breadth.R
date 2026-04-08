.validate_breadth_panel <- function(DT, required_cols) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(required_cols %in% names(DT)))
}

.breadth_adv_dec_panel <- function(DT, date_col, asset_col, close_col) {
  panel <- data.table::copy(DT)
  data.table::setorderv(panel, c(asset_col, date_col))
  panel[, prev_close := data.table::shift(get(close_col)), by = asset_col]
  panel[, is_adv := !is.na(prev_close) & get(close_col) > prev_close]
  panel[, is_dec := !is.na(prev_close) & get(close_col) < prev_close]
  panel[, is_unch := !is.na(prev_close) & abs(get(close_col) - prev_close) < sqrt(.Machine$double.eps)]
  panel[]
}

#' Compute Advance-Decline Breadth
#'
#' Aggregates per-date advancing and declining counts from a cross-sectional
#' asset panel.
#'
#' @param DT A `data.table` panel containing date, asset, and close columns.
#' @param date_col Date or timestamp column name.
#' @param asset_col Asset identifier column name.
#' @param close_col Close-price column name.
#'
#' @return A `data.table` with per-date breadth counts and advance-decline
#'   difference.
#' @export
calc_breadth_ad <- function(DT, date_col = "date", asset_col = "asset", close_col = "close") {
  .validate_breadth_panel(DT, c(date_col, asset_col, close_col))
  panel <- .breadth_adv_dec_panel(DT, date_col, asset_col, close_col)

  out <- panel[, .(
    breadth_adv = sum(is_adv, na.rm = TRUE),
    breadth_dec = sum(is_dec, na.rm = TRUE),
    breadth_unch = sum(is_unch, na.rm = TRUE)
  ), by = date_col]
  out[, breadth_ad := breadth_adv - breadth_dec]
  data.table::setorderv(out, date_col)
  out[]
}

#' Compute Advance-Decline Line
#'
#' Builds a cumulative advance-decline line from per-date breadth counts.
#'
#' @inheritParams calc_breadth_ad
#'
#' @return A `data.table` with per-date breadth counts, difference, and
#'   cumulative advance-decline line.
#' @export
calc_breadth_adl <- function(DT, date_col = "date", asset_col = "asset", close_col = "close") {
  out <- calc_breadth_ad(DT, date_col = date_col, asset_col = asset_col, close_col = close_col)
  out[, breadth_adl := cumsum(breadth_ad)]
  out[]
}

#' Compute Advance-Decline Ratio
#'
#' Computes the per-date advancing-to-declining ratio from a cross-sectional
#' asset panel.
#'
#' @inheritParams calc_breadth_ad
#'
#' @return A `data.table` with per-date breadth counts and ratio.
#' @export
calc_breadth_ratio <- function(DT, date_col = "date", asset_col = "asset", close_col = "close") {
  out <- calc_breadth_ad(DT, date_col = date_col, asset_col = asset_col, close_col = close_col)
  out[, breadth_ratio := breadth_adv / breadth_dec]
  out[breadth_dec == 0 & breadth_adv > 0, breadth_ratio := Inf]
  out[breadth_dec == 0 & breadth_adv == 0, breadth_ratio := NA_real_]
  out[]
}

#' Compute New-High New-Low Breadth
#'
#' Aggregates per-date new-high and new-low counts relative to the prior `n`
#' observations for each asset.
#'
#' @param DT A `data.table` panel containing date, asset, high, and low
#'   columns.
#' @param date_col Date or timestamp column name.
#' @param asset_col Asset identifier column name.
#' @param high_col High-price column name.
#' @param low_col Low-price column name.
#' @param n Integer lookback window for prior highs and lows.
#'
#' @return A `data.table` with per-date new-high/new-low counts and ratios.
#' @export
calc_breadth_high_low <- function(DT, date_col = "date", asset_col = "asset", high_col = "high", low_col = "low", n = 252) {
  .validate_breadth_panel(DT, c(date_col, asset_col, high_col, low_col))
  stopifnot(is.numeric(n), length(n) == 1, n > 0)

  panel <- data.table::copy(DT[, c(date_col, asset_col, high_col, low_col), with = FALSE])
  data.table::setorderv(panel, c(asset_col, date_col))
  panel[, prior_high := rolling_max(.lag_num(get(high_col), 1), n), by = asset_col]
  panel[, prior_low := rolling_min(.lag_num(get(low_col), 1), n), by = asset_col]
  panel[, is_new_high := !is.na(prior_high) & get(high_col) >= prior_high]
  panel[, is_new_low := !is.na(prior_low) & get(low_col) <= prior_low]

  out <- panel[, .(
    breadth_high = sum(is_new_high, na.rm = TRUE),
    breadth_low = sum(is_new_low, na.rm = TRUE)
  ), by = date_col]
  out[, breadth_high_low := breadth_high - breadth_low]
  out[, breadth_high_low_ratio := breadth_high / breadth_low]
  out[breadth_low == 0 & breadth_high > 0, breadth_high_low_ratio := Inf]
  out[breadth_low == 0 & breadth_high == 0, breadth_high_low_ratio := NA_real_]
  data.table::setorderv(out, date_col)
  out[]
}

#' Compute TRIN Breadth
#'
#' Computes per-date TRIN (Arms Index) values from advancing and declining
#' counts and their associated traded volume.
#'
#' @param DT A `data.table` panel containing date, asset, close, and volume
#'   columns.
#' @param date_col Date or timestamp column name.
#' @param asset_col Asset identifier column name.
#' @param close_col Close-price column name.
#' @param volume_col Traded-volume column name.
#'
#' @return A `data.table` with per-date advance/decline counts, volume totals,
#'   and TRIN.
#' @export
calc_breadth_trin <- function(DT, date_col = "date", asset_col = "asset", close_col = "close", volume_col = "volume") {
  .validate_breadth_panel(DT, c(date_col, asset_col, close_col, volume_col))
  panel <- .breadth_adv_dec_panel(DT, date_col, asset_col, close_col)
  panel[, adv_volume := ifelse(is_adv, get(volume_col), 0)]
  panel[, dec_volume := ifelse(is_dec, get(volume_col), 0)]

  out <- panel[, .(
    breadth_adv = sum(is_adv, na.rm = TRUE),
    breadth_dec = sum(is_dec, na.rm = TRUE),
    breadth_adv_volume = sum(adv_volume, na.rm = TRUE),
    breadth_dec_volume = sum(dec_volume, na.rm = TRUE)
  ), by = date_col]
  out[, breadth_trin := (breadth_adv / breadth_dec) / (breadth_adv_volume / breadth_dec_volume)]
  out[(breadth_dec == 0 | breadth_adv_volume == 0 | breadth_dec_volume == 0), breadth_trin := NA_real_]
  data.table::setorderv(out, date_col)
  out[]
}
