#' Add Simple Moving Average Features
#'
#' Computes SMA columns from the `close` series of a candle `data.table` in
#' place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of SMA window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_SMA <- function(DT, ns = c(5, 8, 9, 10, 12, 20, 21, 26, 30, 50, 100, 200)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))
  for (n in ns) {
    colname <- paste0("sma_", n)
    data.table::set(DT, j = colname, value = rolling_mean(DT[["close"]], n))
  }
  invisible(DT)
}

#' Add Weighted Moving Average Features
#'
#' Computes linearly weighted moving-average columns from the `close` series of
#' a candle `data.table` in place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of WMA window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_WMA <- function(DT, ns = c(5, 8, 9, 10, 12, 20, 21, 26, 30, 50, 100, 200)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))
  for (n in ns) {
    colname <- paste0("wma_", n)
    data.table::set(DT, j = colname, value = rolling_wma(DT[["close"]], n))
  }
  invisible(DT)
}

#' Add Hull Moving Average Features
#'
#' Computes HMA columns from the `close` series of a candle `data.table` in
#' place using the standard weighted-moving-average construction.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of HMA window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_HMA <- function(DT, ns = c(9, 16, 20, 50)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))

  close <- DT[["close"]]
  for (n in ns) {
    n_fast <- trunc(n / 2)
    n_smooth <- trunc(sqrt(n))
    madiff <- 2 * rolling_wma(close, n_fast) - rolling_wma(close, n)
    data.table::set(DT, j = paste0("hma_", n), value = rolling_wma(madiff, n_smooth))
  }

  invisible(DT)
}

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

#' Add Double Exponential Moving Average Features
#'
#' Computes DEMA columns from the `close` series of a candle `data.table` in
#' place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of EMA window sizes.
#' @param v Numeric blending factor. `1` matches the conventional DEMA.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_DEMA <- function(DT, ns = c(10, 20, 50), v = 1) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))
  stopifnot(v >= 0, v <= 1)

  close <- DT[["close"]]
  for (n in ns) {
    ema1 <- ema_ttr_fixed_step(close, n, FALSE)
    ema2 <- .apply_after_first_non_na(ema1, ema_ttr_fixed_step, n = n, wilder = FALSE)
    data.table::set(DT, j = paste0("dema_", n), value = (1 + v) * ema1 - v * ema2)
  }

  invisible(DT)
}

#' Add Zero-Lag Exponential Moving Average Features
#'
#' Computes ZLEMA columns from the `close` series of a candle `data.table` in
#' place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of EMA window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_ZLEMA <- function(DT, ns = c(10, 20, 50)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))

  close <- DT[["close"]]
  for (n in ns) {
    lag_n <- floor((n - 1) / 2)
    adjusted <- close
    if (lag_n > 0) {
      adjusted[(lag_n + 1):length(close)] <- close[(lag_n + 1):length(close)] +
        (close[(lag_n + 1):length(close)] - close[seq_len(length(close) - lag_n)])
    }

    zlema <- rep(NA_real_, length(close))
    if (n <= length(close)) {
      alpha <- 2 / (n + 1)
      zlema[n] <- mean(close[seq_len(n)])
      if (n < length(close)) {
        for (i in (n + 1):length(close)) {
          zlema[i] <- alpha * adjusted[i] + (1 - alpha) * zlema[i - 1]
        }
      }
    }
    data.table::set(DT, j = paste0("zlema_", n), value = zlema)
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

.lag_num <- function(x, n) {
  len <- length(x)
  if (n <= 0) {
    return(x)
  }
  if (n >= len) {
    return(rep(NA_real_, len))
  }
  c(rep(NA_real_, n), x[seq_len(len - n)])
}

.apply_after_first_non_na <- function(x, fun, ...) {
  out <- rep(NA_real_, length(x))
  idx <- which(!is.na(x))
  if (!length(idx)) {
    return(out)
  }

  first_idx <- idx[1]
  out[first_idx:length(x)] <- fun(x[first_idx:length(x)], ...)
  out
}

.wilder_sum_ttr <- function(x, n) {
  out <- rep(NA_real_, length(x))
  if (n < 1L || n >= length(x)) {
    return(out)
  }

  seed <- sum(x[seq_len(n)], na.rm = TRUE)
  for (i in (n + 1L):length(x)) {
    if (is.na(x[i])) {
      next
    }
    seed <- seed * (n - 1L) / n + x[i]
    out[i] <- seed
  }

  out
}

#' Add Keltner Channel Features
#'
#' Computes Keltner channel mid, upper, and lower bands from an EMA centerline
#' and ATR envelope on a candle `data.table` in place.
#'
#' @param DT A `data.table` containing `high`, `low`, and `close`.
#' @param ns Integer vector of EMA/ATR window sizes.
#' @param ks Numeric vector of ATR multipliers.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_KeltnerChannels <- function(DT, ns = c(20), ks = c(2)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close") %in% names(DT)))

  calc_EMA(DT, ns = ns)
  calc_ATR(DT, ns = ns, hs = NULL)

  for (n in ns) {
    mid_col <- paste0("kc_mid_", n)
    atr_col <- paste0("atr_", n)
    ema_col <- paste0("ema_", n)

    data.table::set(DT, j = mid_col, value = DT[[ema_col]])

    for (k in ks) {
      k_tag <- .suffix_num(k)
      high_col <- paste0("kc_high_", n, "_", k_tag)
      low_col <- paste0("kc_low_", n, "_", k_tag)

      data.table::set(DT, j = high_col, value = DT[[ema_col]] + k * DT[[atr_col]])
      data.table::set(DT, j = low_col, value = DT[[ema_col]] - k * DT[[atr_col]])
    }
  }

  invisible(DT)
}

#' Add MACD Features
#'
#' Computes Moving Average Convergence Divergence, signal, and histogram
#' columns from the `close` series of a candle `data.table` in place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param fast Integer fast EMA window.
#' @param slow Integer slow EMA window.
#' @param signal Integer signal EMA window.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_MACD <- function(DT, fast = 12, slow = 26, signal = 9) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))
  stopifnot(fast > 0, slow > 0, signal > 0, fast < slow)

  calc_EMA(DT, ns = c(fast, slow))

  macd_col <- paste0("macd_", fast, "_", slow)
  signal_col <- paste0("macd_signal_", fast, "_", slow, "_", signal)
  hist_col <- paste0("macd_hist_", fast, "_", slow, "_", signal)

  macd_value <- DT[[paste0("ema_", fast)]] - DT[[paste0("ema_", slow)]]
  signal_value <- .apply_after_first_non_na(macd_value, ema_ttr_fixed_step, n = signal, wilder = FALSE)

  data.table::set(DT, j = macd_col, value = macd_value)
  data.table::set(DT, j = signal_col, value = signal_value)
  data.table::set(DT, j = hist_col, value = macd_value - signal_value)

  invisible(DT)
}

#' Add Rate of Change Features
#'
#' Computes rate-of-change columns from the `close` series of a candle
#' `data.table` in place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of lag windows.
#' @param scale Numeric scale factor, typically `100` for percent units.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_ROC <- function(DT, ns = c(5, 10, 20), scale = 100) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))

  close <- DT[["close"]]
  for (n in ns) {
    lag_close <- .lag_num(close, n)
    out <- scale * (close / lag_close - 1)
    out[is.na(lag_close) | lag_close == 0] <- NA_real_
    data.table::set(DT, j = paste0("roc_", n), value = out)
  }

  invisible(DT)
}

#' Add Bollinger Band Features
#'
#' Computes Bollinger band mid, standard deviation, upper, lower, and touch
#' flag columns from the `close` series of a candle `data.table` in place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of rolling window sizes.
#' @param ks Numeric vector of band-width multipliers.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_BollingerBands <- function(DT, ns = c(20), ks = c(2)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))

  close <- DT[["close"]]
  for (n in ns) {
    mid_col <- paste0("bb_mid_", n)
    sd_col <- paste0("bb_sd_", n)
    bb_mid <- rolling_mean(close, n)
    bb_sd <- rolling_sd(close, n, sample = FALSE)

    data.table::set(DT, j = mid_col, value = bb_mid)
    data.table::set(DT, j = sd_col, value = bb_sd)

    for (k in ks) {
      k_tag <- .suffix_num(k)
      high_col <- paste0("bb_high_", n, "_", k_tag)
      low_col <- paste0("bb_low_", n, "_", k_tag)
      touch_high_col <- paste0("bb_touch_high_", n, "_", k_tag)
      touch_low_col <- paste0("bb_touch_low_", n, "_", k_tag)

      bb_high <- bb_mid + k * bb_sd
      bb_low <- bb_mid - k * bb_sd
      touch_high <- ifelse(is.na(close) | is.na(bb_high), NA_integer_, as.integer(close >= bb_high))
      touch_low <- ifelse(is.na(close) | is.na(bb_low), NA_integer_, as.integer(close <= bb_low))

      data.table::set(DT, j = high_col, value = bb_high)
      data.table::set(DT, j = low_col, value = bb_low)
      data.table::set(DT, j = touch_high_col, value = touch_high)
      data.table::set(DT, j = touch_low_col, value = touch_low)
    }
  }

  invisible(DT)
}

#' Add Stochastic Oscillator Features
#'
#' Computes fast stochastic `\%K` and smoothed `\%D` columns from candle highs,
#' lows, and closes in place.
#'
#' @param DT A `data.table` containing `high`, `low`, and `close`.
#' @param ns Integer vector of stochastic windows.
#' @param d_ns Integer vector of `\%D` smoothing windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_StochasticOscillator <- function(DT, ns = c(14), d_ns = c(3)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close") %in% names(DT)))

  close <- DT[["close"]]
  for (n in ns) {
    high_n <- rolling_max(DT[["high"]], n)
    low_n <- rolling_min(DT[["low"]], n)
    denom <- high_n - low_n
    stoch_k <- 100 * (close - low_n) / denom
    stoch_k[denom == 0] <- NA_real_

    k_col <- paste0("stoch_k_", n)
    data.table::set(DT, j = k_col, value = stoch_k)

    for (d_n in d_ns) {
      d_col <- paste0("stoch_d_", n, "_", d_n)
      data.table::set(DT, j = d_col, value = .apply_after_first_non_na(stoch_k, rolling_mean, n = d_n))
    }
  }

  invisible(DT)
}

#' Add CCI Features
#'
#' Computes Commodity Channel Index columns from candle highs, lows, and closes
#' in place using typical price, its rolling mean, and rolling mean absolute
#' deviation.
#'
#' @param DT A `data.table` containing `high`, `low`, and `close`.
#' @param ns Integer vector of rolling windows.
#' @param constant Numeric scaling constant in the CCI denominator.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_CCI <- function(DT, ns = c(20), constant = 0.015) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close") %in% names(DT)))

  tp <- (DT[["high"]] + DT[["low"]] + DT[["close"]]) / 3
  for (n in ns) {
    mean_tp <- rolling_mean(tp, n)
    mad_tp <- rolling_mean_abs_dev(tp, n)
    denom <- constant * mad_tp
    cci <- (tp - mean_tp) / denom
    cci[denom == 0] <- NA_real_

    data.table::set(DT, j = paste0("cci_", n), value = cci)
  }

  invisible(DT)
}

#' Add Average Directional Index Features
#'
#' Computes positive and negative directional indicators, directional movement
#' index, and average directional index columns from candle highs, lows, and
#' closes in place.
#'
#' @param DT A `data.table` containing `high`, `low`, and `close`.
#' @param ns Integer vector of ADX windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_ADX <- function(DT, ns = c(14)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close") %in% names(DT)))

  high <- DT[["high"]]
  low <- DT[["low"]]
  close <- DT[["close"]]

  dH <- c(NA_real_, diff(high))
  dL <- c(NA_real_, -diff(low))
  dm_pos <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH > dL, dH, 0))
  dm_neg <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH < dL, dL, 0))

  prev_close <- .lag_num(close, 1)
  tr <- pmax(high - low, abs(high - prev_close), abs(low - prev_close))

  for (n in ns) {
    tr_sm <- .wilder_sum_ttr(tr, n)
    dm_pos_sm <- .wilder_sum_ttr(dm_pos, n)
    dm_neg_sm <- .wilder_sum_ttr(dm_neg, n)

    di_pos <- 100 * dm_pos_sm / tr_sm
    di_neg <- 100 * dm_neg_sm / tr_sm
    di_sum <- di_pos + di_neg
    dx <- 100 * abs(di_pos - di_neg) / di_sum
    dx[di_sum == 0] <- NA_real_
    adx <- .apply_after_first_non_na(dx, ema_ttr_fixed_step, n = n, wilder = TRUE)

    data.table::set(DT, j = paste0("adx_dip_", n), value = di_pos)
    data.table::set(DT, j = paste0("adx_din_", n), value = di_neg)
    data.table::set(DT, j = paste0("adx_dx_", n), value = dx)
    data.table::set(DT, j = paste0("adx_", n), value = adx)
  }

  invisible(DT)
}

#' Add Aroon Features
#'
#' Computes Aroon up, down, and oscillator columns from candle highs and lows
#' in place.
#'
#' @param DT A `data.table` containing `high` and `low`.
#' @param ns Integer vector of Aroon windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_aroon <- function(DT, ns = c(20)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low") %in% names(DT)))

  high <- DT[["high"]]
  low <- DT[["low"]]

  for (n in ns) {
    up <- aroon_up_pct_cpp(high, n)
    dn <- aroon_dn_pct_cpp(low, n)
    data.table::set(DT, j = paste0("aroon_up_", n), value = up)
    data.table::set(DT, j = paste0("aroon_dn_", n), value = dn)
    data.table::set(DT, j = paste0("aroon_osc_", n), value = up - dn)
  }

  invisible(DT)
}

#' Add Parabolic SAR Features
#'
#' Computes Parabolic SAR columns from candle highs and lows in place using the
#' classic acceleration step and maximum parameters.
#'
#' @param DT A `data.table` containing `high` and `low`.
#' @param accels A list of numeric vectors, each of length two, giving
#'   `c(step, max)` acceleration settings.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_SAR <- function(DT, accels = list(c(0.02, 0.2))) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low") %in% names(DT)))

  for (accel in accels) {
    stopifnot(length(accel) == 2)
    step_tag <- .suffix_num(accel[1])
    max_tag <- .suffix_num(accel[2])
    colname <- paste0("sar_", step_tag, "_", max_tag)
    data.table::set(DT, j = colname, value = sar_cpp(DT[["high"]], DT[["low"]], as.numeric(accel)))
  }

  invisible(DT)
}
