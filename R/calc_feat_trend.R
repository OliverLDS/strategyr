#' Calculate Simple Moving Averages (SMA)
#'
#' Add one or more simple moving average (SMA) columns to a price
#' `data.table`, using a fast Rcpp-backed rolling mean.
#'
#' @param DT A [`data.table`][data.table::data.table()] containing at least
#'   a numeric column named `"close"`. The table is modified **by reference**.
#' @param ns Integer vector of window lengths (in bars) for which SMAs
#'   should be computed. For each value `n` in `ns`, a column named
#'   `sma_n` is added.
#'
#' @details
#' For a given window length `n`, the SMA at index `i` is the arithmetic mean
#' of the last `n` closing prices. The first `n - 1` observations of each
#' SMA series are set to `NA_real_`.
#'
#' This function is a convenience wrapper around the Rcpp helper
#' [rolling_mean()], and is part of the *indicator-generation* family in
#' \pkg{strategyr}.
#'
#' @return
#' The input `DT`, invisibly, with additional SMA columns appended
#' **by reference**.
#'
#' @examples
#' library(data.table)
#'
#' DT <- data.table(
#'   time  = 1:10,
#'   close = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
#' )
#'
#' calc_ind_SMA(DT, ns = c(3, 5))
#' DT[]
#'
#' @export
calc_SMA <- function(DT, ns = c(5, 8, 9, 10, 12, 20, 21, 26, 30, 50, 100, 200)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot('close' %in% names(DT))
  for (n in ns) {
    colname <- paste0("sma_", n)
    data.table::set(DT, j = colname, value = rolling_mean(DT[["close"]], n))
  }
  invisible(DT)
}

#' Calculate Exponential Moving Averages (EMA)
#'
#' Add one or more exponential moving average (EMA) columns to a price
#' [`data.table`][data.table::data.table()], using a fast Rcpp-backed
#' implementation compatible with TTR's EMA convention.
#'
#' @param DT A [`data.table`][data.table::data.table()] containing at least
#'   a numeric column named `"close"`. The table is modified **by reference**.
#' @param ns Integer vector of EMA lengths (in bars). For each value
#'   `n` in `ns`, a column named `ema_n` is added.
#'
#' @details
#' For each `n`, the EMA is computed using
#' \deqn{ \alpha = 2 / (n + 1) }
#' (i.e. standard TTR-style EMA, not Wilder smoothing). The initial EMA at
#' index `n` is initialised from the first `n` observations as in
#' [ema_ttr_fixed_step()], and the first `n - 1` values of each EMA column
#' are set to `NA_real_`.
#'
#' This function is a convenience wrapper around the Rcpp helper
#' [ema_ttr_fixed_step()], and is part of the indicator-generation family
#' in \pkg{strategyr}.
#'
#' @return
#' The input `DT`, invisibly, with additional EMA columns appended
#' **by reference**.
#'
#' @examples
#' library(data.table)
#'
#' DT <- data.table(
#'   time  = 1:10,
#'   close = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
#' )
#'
#' calc_ind_EMA(DT, ns = c(3, 5))
#' DT[]
#'
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

# gen_ind_DonchianChannels