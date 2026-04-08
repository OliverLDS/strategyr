#' Validate Scalar Date Input
#'
#' Internal helper that normalizes a scalar date-like input to `Date`.
#'
#' @param x Date-like scalar.
#' @param name Character scalar used in error messages.
#'
#' @return A scalar `Date`.
#' @noRd
.validate_scalar_date <- function(x, name) {
  stopifnot(length(x) == 1)
  out <- as.Date(x)
  stopifnot(!is.na(out))
  out
}

.coupon_month_step <- function(freq) {
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  step <- 12 / freq
  stopifnot(abs(step - round(step)) < .Machine$double.eps^0.5)
  as.integer(round(step))
}

.coupon_schedule_backward <- function(maturity_date, freq, min_date = NULL) {
  maturity_date <- .validate_scalar_date(maturity_date, "maturity_date")
  step <- .coupon_month_step(freq)

  out <- maturity_date
  cur <- maturity_date
  repeat {
    nxt <- seq.Date(from = cur, by = paste0("-", step, " months"), length.out = 2)[2]
    if (!is.null(min_date) && nxt <= min_date) {
      break
    }
    out <- c(out, nxt)
    cur <- nxt
  }

  sort(out)
}

#' Compute Day-Count Fraction
#'
#' Computes the year fraction between two dates under a small set of common
#' fixed-income day-count conventions.
#'
#' @param start_date Scalar start date.
#' @param end_date Scalar end date.
#' @param convention Character scalar. Supported values are `"ACT/365"`,
#'   `"ACT/360"`, and `"30/360"`.
#'
#' @return Numeric scalar year fraction.
#' @export
calc_day_count_frac <- function(start_date, end_date, convention = c("ACT/365", "ACT/360", "30/360")) {
  start_date <- .validate_scalar_date(start_date, "start_date")
  end_date <- .validate_scalar_date(end_date, "end_date")
  stopifnot(end_date >= start_date)
  convention <- match.arg(convention)

  if (convention == "ACT/365") {
    return(as.numeric(end_date - start_date) / 365)
  }
  if (convention == "ACT/360") {
    return(as.numeric(end_date - start_date) / 360)
  }

  y1 <- as.integer(format(start_date, "%Y"))
  y2 <- as.integer(format(end_date, "%Y"))
  m1 <- as.integer(format(start_date, "%m"))
  m2 <- as.integer(format(end_date, "%m"))
  d1 <- min(as.integer(format(start_date, "%d")), 30L)
  d2 <- min(as.integer(format(end_date, "%d")), 30L)

  ((y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1)) / 360
}

#' Compute Coupon Schedule
#'
#' Generates a regular coupon-payment schedule from issue date to maturity for a
#' plain-vanilla fixed-coupon bond without stub handling.
#'
#' @param issue_date Scalar issue date.
#' @param maturity_date Scalar maturity date.
#' @param freq Integer coupon frequency per year.
#'
#' @return A vector of coupon-payment `Date`s after `issue_date` and up to
#'   `maturity_date`.
#' @export
calc_coupon_schedule <- function(issue_date, maturity_date, freq = 2) {
  issue_date <- .validate_scalar_date(issue_date, "issue_date")
  maturity_date <- .validate_scalar_date(maturity_date, "maturity_date")
  stopifnot(maturity_date > issue_date)

  sched <- .coupon_schedule_backward(
    maturity_date = maturity_date,
    freq = freq,
    min_date = issue_date
  )
  sched[sched > issue_date]
}

#' Compute Previous Coupon Date
#'
#' Returns the most recent regular coupon date on or before settlement.
#'
#' @param settle_date Scalar settlement date.
#' @param maturity_date Scalar maturity date.
#' @param freq Integer coupon frequency per year.
#' @param issue_date Optional scalar issue date used to bound the schedule.
#'
#' @return A scalar `Date`, or `NA` if no previous coupon exists within the
#'   bounded schedule.
#' @export
calc_bond_prev_coupon <- function(settle_date, maturity_date, freq = 2, issue_date = NULL) {
  settle_date <- .validate_scalar_date(settle_date, "settle_date")
  maturity_date <- .validate_scalar_date(maturity_date, "maturity_date")
  min_date <- if (is.null(issue_date)) NULL else .validate_scalar_date(issue_date, "issue_date")

  if (!is.null(min_date) && settle_date < min_date) {
    return(as.Date(NA))
  }

  sched <- .coupon_schedule_backward(maturity_date = maturity_date, freq = freq, min_date = min_date)
  prev <- sched[sched <= settle_date]
  if (length(prev) == 0) {
    as.Date(NA)
  } else {
    prev[length(prev)]
  }
}

#' Compute Next Coupon Date
#'
#' Returns the next regular coupon date strictly after settlement.
#'
#' @param settle_date Scalar settlement date.
#' @param maturity_date Scalar maturity date.
#' @param freq Integer coupon frequency per year.
#' @param issue_date Optional scalar issue date used to bound the schedule.
#'
#' @return A scalar `Date`, or `NA` if no future coupon exists within the
#'   bounded schedule.
#' @export
calc_bond_next_coupon <- function(settle_date, maturity_date, freq = 2, issue_date = NULL) {
  settle_date <- .validate_scalar_date(settle_date, "settle_date")
  maturity_date <- .validate_scalar_date(maturity_date, "maturity_date")
  min_date <- if (is.null(issue_date)) NULL else .validate_scalar_date(issue_date, "issue_date")

  if (!is.null(min_date) && settle_date < min_date) {
    sched <- calc_coupon_schedule(issue_date = min_date, maturity_date = maturity_date, freq = freq)
    return(if (length(sched) == 0) as.Date(NA) else sched[1])
  }

  sched <- .coupon_schedule_backward(maturity_date = maturity_date, freq = freq, min_date = min_date)
  next_coupon <- sched[sched > settle_date]
  if (length(next_coupon) == 0) {
    as.Date(NA)
  } else {
    next_coupon[1]
  }
}
