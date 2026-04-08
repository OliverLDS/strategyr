#' Add FX Forward-Points Features
#'
#' Computes forward-points columns from spot and forward FX rates in place.
#'
#' @param DT A `data.table` containing spot and forward columns.
#' @param spot_col Spot-rate column name.
#' @param forward_col Forward-rate column name.
#' @param tenor_tag Character label appended to the output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_fx_forward_points <- function(DT, spot_col = "spot", forward_col = "forward", tenor_tag = "1m") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(spot_col, forward_col) %in% names(DT)))

  out <- DT[[forward_col]] - DT[[spot_col]]
  data.table::set(DT, j = paste0("fx_forward_points_", tenor_tag), value = out)
  invisible(DT)
}

#' Add FX Carry Features
#'
#' Computes FX carry columns from domestic and foreign interest rates in place.
#'
#' @param DT A `data.table` containing domestic and foreign rate columns.
#' @param domestic_rate_col Domestic annualized rate column name.
#' @param foreign_rate_col Foreign annualized rate column name.
#' @param tenor_col Optional tenor-in-years column name. If `NULL`, carry is
#'   reported as annualized rate differential.
#' @param tenor_tag Character label appended to the output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_fx_carry <- function(DT, domestic_rate_col = "r_domestic", foreign_rate_col = "r_foreign", tenor_col = NULL, tenor_tag = "1m") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(domestic_rate_col, foreign_rate_col) %in% names(DT)))

  diff_rate <- DT[[domestic_rate_col]] - DT[[foreign_rate_col]]
  out <- diff_rate
  if (!is.null(tenor_col)) {
    stopifnot(tenor_col %in% names(DT))
    out <- diff_rate * DT[[tenor_col]]
  }
  data.table::set(DT, j = paste0("fx_carry_", tenor_tag), value = out)
  invisible(DT)
}

#' Add FX Basis Features
#'
#' Computes FX basis columns as the gap between observed forward points and
#' covered-interest-parity-implied forward points.
#'
#' @param DT A `data.table` containing spot, forward, domestic-rate, and
#'   foreign-rate columns.
#' @param spot_col Spot-rate column name.
#' @param forward_col Forward-rate column name.
#' @param domestic_rate_col Domestic annualized rate column name.
#' @param foreign_rate_col Foreign annualized rate column name.
#' @param tenor_col Tenor-in-years column name.
#' @param tenor_tag Character label appended to the output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_fx_basis <- function(DT, spot_col = "spot", forward_col = "forward", domestic_rate_col = "r_domestic", foreign_rate_col = "r_foreign", tenor_col = "tenor_years", tenor_tag = "1m") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(spot_col, forward_col, domestic_rate_col, foreign_rate_col, tenor_col) %in% names(DT)))

  spot <- DT[[spot_col]]
  forward <- DT[[forward_col]]
  tau <- DT[[tenor_col]]
  implied_forward <- spot * exp((DT[[domestic_rate_col]] - DT[[foreign_rate_col]]) * tau)
  out <- (forward - implied_forward) / spot
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  data.table::set(DT, j = paste0("fx_basis_", tenor_tag), value = out)
  invisible(DT)
}

#' Add FX Realized-Carry Features
#'
#' Computes realized-carry columns from spot return plus accrual carry over the
#' selected horizon.
#'
#' @param DT A `data.table` containing spot and domestic/foreign rate columns.
#' @param spot_col Spot-rate column name.
#' @param domestic_rate_col Domestic annualized rate column name.
#' @param foreign_rate_col Foreign annualized rate column name.
#' @param tenor_col Optional tenor-in-years column name. If `NULL`, a one-period
#'   accrual of `1 / annualization` is used.
#' @param annualization Numeric annualization factor used when `tenor_col` is
#'   `NULL`.
#' @param tenor_tag Character label appended to the output column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_fx_realized_carry <- function(DT, spot_col = "spot", domestic_rate_col = "r_domestic", foreign_rate_col = "r_foreign", tenor_col = NULL, annualization = 252, tenor_tag = "1m") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(spot_col, domestic_rate_col, foreign_rate_col) %in% names(DT)))

  spot <- DT[[spot_col]]
  spot_ret <- c(NA_real_, diff(log(spot)))
  accrual <- DT[[domestic_rate_col]] - DT[[foreign_rate_col]]
  if (!is.null(tenor_col)) {
    stopifnot(tenor_col %in% names(DT))
    accrual <- accrual * DT[[tenor_col]]
  } else {
    accrual <- accrual / annualization
  }

  out <- spot_ret + accrual
  data.table::set(DT, j = paste0("fx_realized_carry_", tenor_tag), value = out)
  invisible(DT)
}
