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

#' Add Stochastic Momentum Index Features
#'
#' Computes SMI and signal columns from candle highs, lows, and closes in place
#' using double-smoothed EMA numerators and denominators.
#'
#' @param DT A `data.table` containing `high`, `low`, and `close`.
#' @param n Integer lookback window for the price range.
#' @param nFast Integer fast EMA smoothing window.
#' @param nSlow Integer slow EMA smoothing window.
#' @param nSig Integer signal EMA smoothing window.
#' @param bounded Logical; if `TRUE`, use current-window highs and lows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_SMI <- function(DT, n = 13, nFast = 2, nSlow = 25, nSig = 9, bounded = TRUE) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close") %in% names(DT)))

  high <- DT[["high"]]
  low <- DT[["low"]]
  close <- DT[["close"]]

  if (bounded) {
    hmax <- rolling_max(high, n)
    lmin <- rolling_min(low, n)
  } else {
    hmax <- rolling_max(c(high[1], high[-length(high)]), n)
    lmin <- rolling_min(c(low[1], low[-length(low)]), n)
  }

  hmax[is.na(hmax)] <- high[is.na(hmax)]
  lmin[is.na(lmin)] <- low[is.na(lmin)]
  hl_diff <- hmax - lmin
  c_diff <- close - (hmax + lmin) / 2

  num1 <- .apply_after_first_non_na(c_diff, ema_ttr_fixed_step, n = nSlow, wilder = FALSE)
  den1 <- .apply_after_first_non_na(hl_diff, ema_ttr_fixed_step, n = nSlow, wilder = FALSE)
  num2 <- .apply_after_first_non_na(num1, ema_ttr_fixed_step, n = nFast, wilder = FALSE)
  den2 <- .apply_after_first_non_na(den1, ema_ttr_fixed_step, n = nFast, wilder = FALSE)

  smi <- 100 * (num2 / (den2 / 2))
  smi[den2 == 0] <- NA_real_
  signal <- .apply_after_first_non_na(smi, ema_ttr_fixed_step, n = nSig, wilder = FALSE)

  smi_col <- paste0("smi_", n, "_", nFast, "_", nSlow)
  sig_col <- paste0("smi_signal_", n, "_", nFast, "_", nSlow, "_", nSig)
  data.table::set(DT, j = smi_col, value = smi)
  data.table::set(DT, j = sig_col, value = signal)

  invisible(DT)
}

#' Add TRIX Features
#'
#' Computes triple-smoothed exponential trend columns and their signal lines
#' from the `close` series of a candle `data.table` in place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of EMA windows used for the triple smoothing.
#' @param signal_ns Integer vector of signal-line windows.
#' @param scale Numeric scale factor applied to the one-period discrete ROC of
#'   the triple EMA.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_TRIX <- function(DT, ns = c(20), signal_ns = c(9), scale = 100) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))

  close <- DT[["close"]]
  for (n in ns) {
    ema1 <- ema_ttr_fixed_step(close, n, FALSE)
    ema2 <- .apply_after_first_non_na(ema1, ema_ttr_fixed_step, n = n, wilder = FALSE)
    ema3 <- .apply_after_first_non_na(ema2, ema_ttr_fixed_step, n = n, wilder = FALSE)

    trix <- scale * (ema3 / .lag_num(ema3, 1) - 1)
    trix[is.nan(trix)] <- NA_real_
    trix_col <- paste0("trix_", n)
    data.table::set(DT, j = trix_col, value = trix)

    for (signal_n in signal_ns) {
      signal_col <- paste0("trix_signal_", n, "_", signal_n)
      signal_value <- .apply_after_first_non_na(trix, ema_ttr_fixed_step, n = signal_n, wilder = FALSE)
      data.table::set(DT, j = signal_col, value = signal_value)
    }
  }

  invisible(DT)
}

#' Add Ultimate Oscillator Features
#'
#' Computes Ultimate Oscillator columns from candle highs, lows, and closes in
#' place using weighted buy-pressure to true-range ratios across three windows.
#'
#' @param DT A `data.table` containing `high`, `low`, and `close`.
#' @param nss A list of integer vectors of length three giving the short,
#'   medium, and long windows.
#' @param wtss A list of numeric vectors of length three giving the
#'   corresponding window weights.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_ultimateOscillator <- function(DT, nss = list(c(7, 14, 28)), wtss = list(c(4, 2, 1))) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close") %in% names(DT)))
  stopifnot(length(nss) == length(wtss))

  high <- DT[["high"]]
  low <- DT[["low"]]
  close <- DT[["close"]]
  close_lag <- .lag_num(close, 1)

  true_low <- pmin(low, close_lag)
  tr <- pmax(high, close_lag) - true_low
  buy_pressure <- close - true_low

  true_low[is.na(close_lag)] <- NA_real_
  tr[is.na(close_lag)] <- NA_real_
  buy_pressure[is.na(close_lag)] <- NA_real_

  for (i in seq_along(nss)) {
    ns <- as.integer(nss[[i]])
    wts <- as.numeric(wtss[[i]])
    stopifnot(length(ns) == 3, length(wts) == 3)

    osc <- rep(0, length(close))
    for (j in seq_len(3)) {
      bp_sum <- rolling_sum(buy_pressure, ns[j])
      tr_sum <- rolling_sum(tr, ns[j])
      ratio <- bp_sum / tr_sum
      ratio[tr_sum == 0] <- NA_real_
      osc <- osc + wts[j] * ratio
    }

    colname <- paste0("uo_", paste(c(ns, wts), collapse = "_"))
    data.table::set(DT, j = colname, value = 100 * osc / sum(wts))
  }

  invisible(DT)
}

#' Add Know Sure Thing Features
#'
#' Computes KST and signal columns from weighted moving averages of multiple
#' rate-of-change series on a candle `data.table` in place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param nss A list of integer vectors giving the smoothing windows for each
#'   ROC component.
#' @param n_rocss A list of integer vectors giving the ROC lags.
#' @param signal_ns Integer vector of signal-line windows.
#' @param wtss A list of numeric vectors giving the component weights.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_KST <- function(
  DT,
  nss = list(c(10, 10, 10, 15)),
  n_rocss = list(c(10, 15, 20, 30)),
  signal_ns = c(9),
  wtss = list(1:4)
) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))
  stopifnot(length(nss) == length(n_rocss), length(nss) == length(wtss))

  close <- DT[["close"]]
  for (i in seq_along(nss)) {
    ns <- as.integer(nss[[i]])
    n_rocs <- as.integer(n_rocss[[i]])
    wts <- as.numeric(wtss[[i]])
    stopifnot(length(ns) == length(n_rocs), length(ns) == length(wts))

    kst <- rep(0, length(close))
    for (j in seq_along(ns)) {
      roc_j <- .lag_num(log(close), n_rocs[j])
      roc_j <- log(close) - roc_j
      roc_j[is.na(.lag_num(close, n_rocs[j]))] <- NA_real_
      ma_roc_j <- .apply_after_first_non_na(roc_j, rolling_mean, n = ns[j])
      kst <- kst + wts[j] * ma_roc_j
    }
    kst <- 100 * kst

    base_tag <- paste(c(ns, n_rocs, wts), collapse = "_")
    data.table::set(DT, j = paste0("kst_", base_tag), value = kst)

    for (signal_n in signal_ns) {
      signal_value <- .apply_after_first_non_na(kst, rolling_mean, n = signal_n)
      data.table::set(
        DT,
        j = paste0("kst_signal_", base_tag, "_", signal_n),
        value = signal_value
      )
    }
  }

  invisible(DT)
}

#' Add Chande Momentum Oscillator Features
#'
#' Computes CMO columns from the `close` series of a candle `data.table` in
#' place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of CMO windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_CMO <- function(DT, ns = c(14)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))

  close <- DT[["close"]]
  d <- c(NA_real_, diff(close))
  up <- ifelse(d > 0, d, 0)
  dn <- ifelse(d < 0, abs(d), 0)

  for (n in ns) {
    up_sum <- rolling_sum(up, n)
    dn_sum <- rolling_sum(dn, n)
    denom <- up_sum + dn_sum
    cmo <- 100 * (up_sum - dn_sum) / denom
    cmo[is.nan(cmo)] <- NA_real_
    data.table::set(DT, j = paste0("cmo_", n), value = cmo)
  }

  invisible(DT)
}
