.calc_RSI <- function(close, n) {
  d <- diff(close)
  gain <- pmax(d, 0)
  loss <- pmax(-d, 0)

  avg_gain <- ema_ttr_fixed_step(gain, n, TRUE)
  avg_loss <- ema_ttr_fixed_step(loss, n, TRUE)

  rs <- avg_gain / avg_loss
  out <- 100 - 100 / (1 + rs)
  out[avg_loss == 0 & avg_gain > 0] <- 100
  out[avg_loss == 0 & avg_gain == 0] <- 50

  c(NA_real_, out)
}

.calc_RSI_logr <- function(close, h) {
  len <- length(close)
  log_ret <- log(close[-1] / close[-len])
  long_gain <- pmax(log_ret, 0)
  short_gain <- pmax(-log_ret, 0)

  tau <- .h_to_tau(h)
  avg_long <- ema_fixed_step_tau(long_gain, tau)
  avg_short <- ema_fixed_step_tau(short_gain, tau)

  rs <- avg_long / avg_short
  out <- 100 - 100 / (1 + rs)
  out[avg_short == 0 & avg_long > 0] <- 100
  out[avg_short == 0 & avg_long == 0] <- 50

  c(NA_real_, out)
}

#' Add Relative Strength Index Features
#'
#' Computes both classic Wilder-style RSI columns and strategyr-specific
#' log-return RSI columns on a candle `data.table` in place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Optional integer vector of classic RSI windows.
#' @param hs Optional numeric vector of half-life values for log-return RSI.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_RSI <- function(DT, ns = c(9, 14, 21), hs = c(12, 24)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))

  if (!is.null(ns)) {
    for (n in ns) {
      data.table::set(DT, j = paste0("rsi_", n), value = .calc_RSI(DT[["close"]], n))
    }
  }

  if (!is.null(hs)) {
    for (h in hs) {
      data.table::set(DT, j = paste0("rsi_logr_", h), value = .calc_RSI_logr(DT[["close"]], h))
    }
  }

  invisible(DT)
}

#' Add Williams Percent R Features
#'
#' Computes Williams percent R columns from candle highs, lows, and closes in
#' place on the same `[0, 1]` scale used by `TTR::WPR()`.
#'
#' @param DT A `data.table` containing `high`, `low`, and `close`.
#' @param ns Integer vector of WPR windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_WPR <- function(DT, ns = c(14)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close") %in% names(DT)))

  high <- DT[["high"]]
  low <- DT[["low"]]
  close <- DT[["close"]]

  for (n in ns) {
    high_n <- rolling_max(high, n)
    low_n <- rolling_min(low, n)
    denom <- high_n - low_n
    wpr <- rep(NA_real_, length(close))
    valid <- !is.na(high_n) & !is.na(low_n)
    wpr[valid] <- (high_n[valid] - close[valid]) / denom[valid]
    wpr[valid & denom == 0] <- 0.5

    data.table::set(DT, j = paste0("wpr_", n), value = wpr)
  }

  invisible(DT)
}
