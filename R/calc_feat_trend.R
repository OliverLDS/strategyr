#' Add Exponential Moving Average Features
#'
#' Computes EMA columns from the `close` series of a candle `data.table` in
#' place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of EMA window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_EMA <- function(DT, ns = c(5, 8, 9, 10, 12, 20, 21, 26, 30, 50, 100, 200)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot('close' %in% names(DT))
  for (n in ns) {
    colname <- paste0("ema_", n)
    data.table::set(DT, j = colname, value = ema_ttr_fixed_step(DT[["close"]], n, FALSE))
  }
  invisible(DT)
}

#' Add Donchian Channel Features
#'
#' Computes rolling channel highs, lows, and midpoints from `high` and `low`
#' price columns on a candle `data.table` in place.
#'
#' @param DT A `data.table` containing `high` and `low`.
#' @param ns Integer vector of Donchian channel window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_DonchianChannels <- function(DT, ns = c(20, 55)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low") %in% names(DT)))

  for (n in ns) {
    high_col <- paste0("dc_high_", n)
    low_col <- paste0("dc_low_", n)
    mid_col <- paste0("dc_mid_", n)

    dc_high <- rolling_max(DT[["high"]], n)
    dc_low <- rolling_min(DT[["low"]], n)

    data.table::set(DT, j = high_col, value = dc_high)
    data.table::set(DT, j = low_col, value = dc_low)
    data.table::set(DT, j = mid_col, value = (dc_high + dc_low) / 2)
  }

  invisible(DT)
}
