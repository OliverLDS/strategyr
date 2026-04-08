#' Add Min-Max Normalization Features
#'
#' Computes rolling min-max normalized columns in place for the selected input
#' columns.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param cols Character vector of numeric columns to normalize.
#' @param ns Integer vector of rolling window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_normalize <- function(DT, cols = "close", ns = c(20, 60)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(cols %in% names(DT)))

  for (col in cols) {
    x <- DT[[col]]
    for (n in ns) {
      xmin <- rolling_min(x, n)
      xmax <- rolling_max(x, n)
      out <- (x - xmin) / (xmax - xmin)
      out[is.nan(out)] <- NA_real_
      out[(xmax - xmin) == 0] <- NA_real_
      data.table::set(DT, j = paste0("normalize_", col, "_", n), value = out)
    }
  }

  invisible(DT)
}

#' Add Z-Score Features
#'
#' Computes rolling z-score columns in place for the selected input columns.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param cols Character vector of numeric columns to standardize.
#' @param ns Integer vector of rolling window sizes.
#' @param sample Logical; if `TRUE`, uses sample standard deviation. Defaults
#'   to `TRUE`.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_zscore <- function(DT, cols = "close", ns = c(20, 60), sample = TRUE) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(cols %in% names(DT)))

  for (col in cols) {
    x <- DT[[col]]
    for (n in ns) {
      mu <- rolling_mean(x, n)
      sigma <- rolling_sd(x, n, sample = sample)
      out <- (x - mu) / sigma
      out[is.nan(out)] <- NA_real_
      out[sigma == 0] <- NA_real_
      data.table::set(DT, j = paste0("zscore_", col, "_", n), value = out)
    }
  }

  invisible(DT)
}

.rolling_apply_complete <- function(x, n, fun) {
  len <- length(x)
  out <- rep(NA_real_, len)
  if (n <= 0 || n > len) {
    return(out)
  }

  for (i in seq.int(n, len)) {
    window <- x[seq.int(i - n + 1L, i)]
    if (anyNA(window)) {
      next
    }
    out[i] <- fun(window)
  }

  out
}

.excess_kurtosis <- function(x) {
  mu <- mean(x)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) {
    return(NA_real_)
  }
  mean((x - mu)^4) / (s^4) - 3
}

.skewness <- function(x) {
  mu <- mean(x)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) {
    return(NA_real_)
  }
  mean((x - mu)^3) / (s^3)
}

#' Add Rolling Percent-Rank Features
#'
#' Computes rolling percent-rank columns in place for the selected input
#' columns. The current observation is ranked against the most recent `n`
#' values, including itself.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param cols Character vector of numeric columns.
#' @param ns Integer vector of rolling window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_percent_rank <- function(DT, cols = "close", ns = c(20, 60)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(cols %in% names(DT)))

  rank_fun <- function(window) {
    (sum(window <= window[length(window)]) - 1) / (length(window) - 1)
  }

  for (col in cols) {
    x <- DT[[col]]
    for (n in ns) {
      out <- if (n <= 1) rep(NA_real_, length(x)) else .rolling_apply_complete(x, n, rank_fun)
      data.table::set(DT, j = paste0("percent_rank_", col, "_", n), value = out)
    }
  }

  invisible(DT)
}

#' Add Rolling Quantile Features
#'
#' Computes rolling quantile columns in place for the selected input columns.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param cols Character vector of numeric columns.
#' @param ns Integer vector of rolling window sizes.
#' @param probs Numeric vector of quantile probabilities in `[0, 1]`.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_rolling_quantile <- function(DT, cols = "close", ns = c(20, 60), probs = c(0.25, 0.5, 0.75)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(cols %in% names(DT)))
  stopifnot(all(!is.na(probs)), all(probs >= 0), all(probs <= 1))

  for (col in cols) {
    x <- DT[[col]]
    for (n in ns) {
      qmat <- rolling_quantiles(x, n, probs)
      for (j in seq_along(probs)) {
        prob_tag <- .suffix_num(probs[j])
        data.table::set(DT, j = paste0("quantile_", col, "_", n, "_", prob_tag), value = qmat[, j])
      }
    }
  }

  invisible(DT)
}

#' Add Realized Volatility Features
#'
#' Computes annualized rolling realized-volatility columns from close-to-close
#' log returns in place.
#'
#' @param DT A `data.table` containing a `close` column.
#' @param ns Integer vector of rolling windows measured in return observations.
#' @param annualization Numeric annualization factor. Defaults to `252`.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_realized_vol <- function(DT, ns = c(10, 20, 60), annualization = 252) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot("close" %in% names(DT))
  stopifnot(is.numeric(annualization), length(annualization) == 1, annualization > 0)

  logr <- c(NA_real_, diff(log(DT[["close"]])))
  for (n in ns) {
    out <- rep(NA_real_, length(logr))
    if (length(logr) > 1) {
      out[2:length(logr)] <- rolling_sd(logr[2:length(logr)], n, sample = FALSE) * sqrt(annualization)
    }
    data.table::set(DT, j = paste0("rv_", n), value = out)
  }

  invisible(DT)
}

#' Add Rolling Skewness Features
#'
#' Computes rolling skewness columns in place for the selected input columns.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param cols Character vector of numeric columns.
#' @param ns Integer vector of rolling window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_skewness <- function(DT, cols = "close", ns = c(20, 60)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(cols %in% names(DT)))

  for (col in cols) {
    x <- DT[[col]]
    for (n in ns) {
      data.table::set(DT, j = paste0("skew_", col, "_", n), value = .rolling_apply_complete(x, n, .skewness))
    }
  }

  invisible(DT)
}

#' Add Rolling Kurtosis Features
#'
#' Computes rolling excess-kurtosis columns in place for the selected input
#' columns.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param cols Character vector of numeric columns.
#' @param ns Integer vector of rolling window sizes.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_kurtosis <- function(DT, cols = "close", ns = c(20, 60)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(cols %in% names(DT)))

  for (col in cols) {
    x <- DT[[col]]
    for (n in ns) {
      data.table::set(DT, j = paste0("kurt_", col, "_", n), value = .rolling_apply_complete(x, n, .excess_kurtosis))
    }
  }

  invisible(DT)
}
