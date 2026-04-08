.validate_credit_panel <- function(DT, required_cols) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(required_cols %in% names(DT)))
}

.pick_nearest_tenor_row <- function(DT, tenor_col, tenor_value) {
  if (!nrow(DT)) {
    return(DT[0])
  }
  DT[which.min(abs(get(tenor_col) - tenor_value))]
}

#' Add Credit-Spread Features
#'
#' Computes credit-spread columns in place as the difference between an issuer
#' yield and a benchmark yield.
#'
#' @param DT A `data.table` containing issuer and benchmark yield columns.
#' @param yield_col Issuer-yield column name.
#' @param benchmark_yield_col Benchmark-yield column name.
#' @param name Optional output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_credit_spread <- function(DT, yield_col = "ytm", benchmark_yield_col = "benchmark_ytm", name = NULL) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(yield_col, benchmark_yield_col) %in% names(DT)))

  if (is.null(name)) {
    name <- "credit_spread"
  }
  data.table::set(DT, j = name, value = DT[[yield_col]] - DT[[benchmark_yield_col]])
  invisible(DT)
}

#' Add Excess-Spread Features
#'
#' Computes excess-spread columns in place as the difference between a credit
#' spread and a benchmark or sector spread.
#'
#' @param DT A `data.table` containing spread columns.
#' @param spread_col Credit-spread column name.
#' @param benchmark_spread_col Benchmark or sector spread column name.
#' @param name Optional output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_excess_spread <- function(DT, spread_col = "credit_spread", benchmark_spread_col = "benchmark_spread", name = NULL) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(spread_col, benchmark_spread_col) %in% names(DT)))

  if (is.null(name)) {
    name <- "excess_spread"
  }
  data.table::set(DT, j = name, value = DT[[spread_col]] - DT[[benchmark_spread_col]])
  invisible(DT)
}

#' Compute Spread-Curve Slope
#'
#' Computes the per-date linear slope of credit spread against tenor.
#'
#' @param DT A `data.table` credit panel containing date, tenor, and spread
#'   columns.
#' @param date_col Date or timestamp column name.
#' @param tenor_col Tenor column name.
#' @param spread_col Credit-spread column name.
#'
#' @return A `data.table` with one spread-curve slope estimate per date.
#' @export
calc_spread_curve_slope <- function(DT, date_col = "date", tenor_col = "tenor", spread_col = "credit_spread") {
  .validate_credit_panel(DT, c(date_col, tenor_col, spread_col))

  out <- DT[, {
    x <- get(tenor_col)
    y <- get(spread_col)
    valid <- is.finite(x) & is.finite(y)
    slope <- if (sum(valid) >= 2) stats::coef(stats::lm(y[valid] ~ x[valid]))[[2]] else NA_real_
    .(spread_curve_slope = slope)
  }, by = date_col]
  data.table::setorderv(out, date_col)
  out[]
}

#' Compute Spread-Curve Butterfly
#'
#' Computes a three-point spread-curve butterfly per date using the nearest
#' available tenors.
#'
#' @param DT A `data.table` credit panel containing date, tenor, and spread
#'   columns.
#' @param date_col Date or timestamp column name.
#' @param tenor_col Tenor column name.
#' @param spread_col Credit-spread column name.
#' @param tenors Numeric vector of length three giving the front, belly, and
#'   back target tenors.
#'
#' @return A `data.table` with one spread-curve butterfly value per date.
#' @export
calc_spread_curve_butterfly <- function(DT, date_col = "date", tenor_col = "tenor", spread_col = "credit_spread", tenors = c(2, 5, 10)) {
  stopifnot(length(tenors) == 3)
  .validate_credit_panel(DT, c(date_col, tenor_col, spread_col))

  out <- DT[, {
    leg1 <- .pick_nearest_tenor_row(.SD, tenor_col, tenors[1])
    leg2 <- .pick_nearest_tenor_row(.SD, tenor_col, tenors[2])
    leg3 <- .pick_nearest_tenor_row(.SD, tenor_col, tenors[3])
    value <- if (nrow(leg1) && nrow(leg2) && nrow(leg3)) {
      2 * leg2[[spread_col]][1] - leg1[[spread_col]][1] - leg3[[spread_col]][1]
    } else {
      NA_real_
    }
    .(spread_curve_butterfly = value)
  }, by = date_col]
  data.table::setorderv(out, date_col)
  out[]
}
