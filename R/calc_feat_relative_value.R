#' Add Spread Features
#'
#' Computes arithmetic spread columns between two numeric input columns in
#' place.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param x_col First numeric column name.
#' @param y_col Second numeric column name.
#' @param name Optional output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_spread <- function(DT, x_col = "close", y_col = "benchmark_close", name = NULL) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(x_col, y_col) %in% names(DT)))

  if (is.null(name)) {
    name <- paste0("spread_", x_col, "_", y_col)
  }
  data.table::set(DT, j = name, value = DT[[x_col]] - DT[[y_col]])
  invisible(DT)
}

#' Add Ratio Features
#'
#' Computes arithmetic ratio columns between two numeric input columns in
#' place.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param x_col First numeric column name.
#' @param y_col Second numeric column name.
#' @param name Optional output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_ratio <- function(DT, x_col = "close", y_col = "benchmark_close", name = NULL) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(x_col, y_col) %in% names(DT)))

  if (is.null(name)) {
    name <- paste0("ratio_", x_col, "_", y_col)
  }
  out <- DT[[x_col]] / DT[[y_col]]
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  data.table::set(DT, j = name, value = out)
  invisible(DT)
}

#' Add Log-Spread Features
#'
#' Computes log-price spread columns between two positive numeric input columns
#' in place.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param x_col First numeric column name.
#' @param y_col Second numeric column name.
#' @param name Optional output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_log_spread <- function(DT, x_col = "close", y_col = "benchmark_close", name = NULL) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(x_col, y_col) %in% names(DT)))

  if (is.null(name)) {
    name <- paste0("log_spread_", x_col, "_", y_col)
  }
  out <- log(DT[[x_col]]) - log(DT[[y_col]])
  out[!is.finite(out)] <- NA_real_
  data.table::set(DT, j = name, value = out)
  invisible(DT)
}

.rolling_cov <- function(x, y, n, sample = TRUE) {
  stopifnot(length(x) == length(y))
  mean_xy <- rolling_mean(x * y, n)
  mean_x <- rolling_mean(x, n)
  mean_y <- rolling_mean(y, n)
  cov_xy <- mean_xy - mean_x * mean_y
  if (sample && n > 1) {
    cov_xy <- cov_xy * n / (n - 1)
  }
  cov_xy
}

.rolling_return <- function(x, use_log = TRUE) {
  len <- length(x)
  out <- rep(NA_real_, len)
  if (len <= 1) {
    return(out)
  }
  if (use_log) {
    out[2:len] <- diff(log(x))
  } else {
    out[2:len] <- x[2:len] / x[1:(len - 1)] - 1
  }
  out
}

#' Add Rolling Tracking-Error Features
#'
#' Computes rolling tracking-error columns from the return difference between an
#' asset and benchmark series.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param x_col Asset-price column name.
#' @param y_col Benchmark-price column name.
#' @param ns Integer vector of rolling windows.
#' @param annualization Numeric annualization factor. Defaults to `252`.
#' @param use_log Logical; if `TRUE`, uses log returns.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_tracking_error <- function(DT, x_col = "close", y_col = "benchmark_close", ns = c(20, 60), annualization = 252, use_log = TRUE) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(x_col, y_col) %in% names(DT)))

  x_ret <- .rolling_return(DT[[x_col]], use_log = use_log)
  y_ret <- .rolling_return(DT[[y_col]], use_log = use_log)
  diff_ret <- x_ret - y_ret

  for (n in ns) {
    out <- rep(NA_real_, length(diff_ret))
    if (length(diff_ret) > 1) {
      out[2:length(diff_ret)] <- rolling_sd(diff_ret[2:length(diff_ret)], n, sample = TRUE) * sqrt(annualization)
    }
    data.table::set(DT, j = paste0("tracking_error_", n), value = out)
  }

  invisible(DT)
}

#' Add Rolling Beta Features
#'
#' Computes rolling beta columns from asset and benchmark return series.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param x_col Asset-price column name.
#' @param y_col Benchmark-price column name.
#' @param ns Integer vector of rolling windows.
#' @param use_log Logical; if `TRUE`, uses log returns.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_rolling_beta <- function(DT, x_col = "close", y_col = "benchmark_close", ns = c(20, 60), use_log = TRUE) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(x_col, y_col) %in% names(DT)))

  x_ret <- .rolling_return(DT[[x_col]], use_log = use_log)
  y_ret <- .rolling_return(DT[[y_col]], use_log = use_log)

  for (n in ns) {
    out <- rep(NA_real_, length(x_ret))
    if (length(x_ret) > 1) {
      cov_xy <- .rolling_cov(x_ret[2:length(x_ret)], y_ret[2:length(y_ret)], n, sample = TRUE)
      var_y <- rolling_sd(y_ret[2:length(y_ret)], n, sample = TRUE)^2
      beta <- cov_xy / var_y
      beta[is.nan(beta) | is.infinite(beta)] <- NA_real_
      out[2:length(x_ret)] <- beta
    }
    data.table::set(DT, j = paste0("beta_", n), value = out)
  }

  invisible(DT)
}

#' Add Rolling Correlation Features
#'
#' Computes rolling correlation columns from asset and benchmark return series.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param x_col Asset-price column name.
#' @param y_col Benchmark-price column name.
#' @param ns Integer vector of rolling windows.
#' @param use_log Logical; if `TRUE`, uses log returns.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_rolling_corr <- function(DT, x_col = "close", y_col = "benchmark_close", ns = c(20, 60), use_log = TRUE) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(x_col, y_col) %in% names(DT)))

  x_ret <- .rolling_return(DT[[x_col]], use_log = use_log)
  y_ret <- .rolling_return(DT[[y_col]], use_log = use_log)

  for (n in ns) {
    out <- rep(NA_real_, length(x_ret))
    if (length(x_ret) > 1) {
      cov_xy <- .rolling_cov(x_ret[2:length(x_ret)], y_ret[2:length(y_ret)], n, sample = TRUE)
      sd_x <- rolling_sd(x_ret[2:length(x_ret)], n, sample = TRUE)
      sd_y <- rolling_sd(y_ret[2:length(y_ret)], n, sample = TRUE)
      corr <- cov_xy / (sd_x * sd_y)
      corr[is.nan(corr) | is.infinite(corr)] <- NA_real_
      out[2:length(x_ret)] <- corr
    }
    data.table::set(DT, j = paste0("corr_", n), value = out)
  }

  invisible(DT)
}

#' Add Relative-Strength Features
#'
#' Computes rolling relative-strength columns as the ratio of cumulative asset
#' return to cumulative benchmark return over each lookback window.
#'
#' @param DT A `data.table` containing the selected input columns.
#' @param x_col Asset-price column name.
#' @param y_col Benchmark-price column name.
#' @param ns Integer vector of rolling windows.
#' @param use_log Logical; if `TRUE`, uses log-return aggregation.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_relative_strength <- function(DT, x_col = "close", y_col = "benchmark_close", ns = c(20, 60), use_log = TRUE) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(x_col, y_col) %in% names(DT)))

  x_ret <- .rolling_return(DT[[x_col]], use_log = use_log)
  y_ret <- .rolling_return(DT[[y_col]], use_log = use_log)

  for (n in ns) {
    if (use_log) {
      x_cum <- rolling_sum(x_ret, n)
      y_cum <- rolling_sum(y_ret, n)
      out <- exp(x_cum - y_cum)
    } else {
      x_base <- .lag_num(DT[[x_col]], n)
      y_base <- .lag_num(DT[[y_col]], n)
      out <- (DT[[x_col]] / x_base) / (DT[[y_col]] / y_base)
    }
    out[is.nan(out) | is.infinite(out)] <- NA_real_
    data.table::set(DT, j = paste0("relative_strength_", n), value = out)
  }

  invisible(DT)
}
