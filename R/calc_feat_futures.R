.validate_futures_curve_panel <- function(DT, required_cols) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(required_cols %in% names(DT)))
}

.extract_curve_pair <- function(DT, date_col, rank_col, price_col, time_to_expiry_col, rank_front, rank_deferred) {
  required <- c(date_col, rank_col, price_col)
  if (!is.null(time_to_expiry_col)) {
    required <- c(required, time_to_expiry_col)
  }
  .validate_futures_curve_panel(DT, required)

  front <- data.table::copy(DT[get(rank_col) == rank_front, required, with = FALSE])
  deferred <- data.table::copy(DT[get(rank_col) == rank_deferred, required, with = FALSE])

  data.table::setnames(front, price_col, "front_price")
  data.table::setnames(deferred, price_col, "deferred_price")
  if (!is.null(time_to_expiry_col)) {
    data.table::setnames(front, time_to_expiry_col, "front_tau")
    data.table::setnames(deferred, time_to_expiry_col, "deferred_tau")
  }

  out <- front[deferred, on = date_col, nomatch = 0]
  data.table::setorderv(out, date_col)
  out[]
}

#' Compute Front-Next Futures Spread
#'
#' Computes the front-versus-next futures spread per date from a futures curve
#' panel.
#'
#' @param DT A `data.table` futures panel containing date, contract-rank, and
#'   price columns.
#' @param date_col Date or timestamp column name.
#' @param rank_col Contract-rank column name. Rank `1` is the front contract.
#' @param price_col Price column name.
#' @param rank_front Integer front-contract rank.
#' @param rank_next Integer deferred-contract rank used as the next contract.
#'
#' @return A `data.table` with front price, next price, and absolute/relative
#'   spread per date.
#' @export
calc_front_next_spread <- function(DT, date_col = "date", rank_col = "contract_rank", price_col = "close", rank_front = 1L, rank_next = 2L) {
  out <- .extract_curve_pair(DT, date_col, rank_col, price_col, time_to_expiry_col = NULL, rank_front, rank_next)
  out[, front_next_spread := deferred_price - front_price]
  out[, front_next_spread_rel := front_next_spread / front_price]
  out[is.nan(front_next_spread_rel) | is.infinite(front_next_spread_rel), front_next_spread_rel := NA_real_]
  out[]
}

#' Compute Futures Curve Slope
#'
#' Computes the per-date linear slope of log futures price against time to
#' expiry.
#'
#' @param DT A `data.table` futures panel containing date, time-to-expiry, and
#'   price columns.
#' @param date_col Date or timestamp column name.
#' @param time_to_expiry_col Time-to-expiry column name expressed in years or a
#'   consistent time unit.
#' @param price_col Price column name.
#'
#' @return A `data.table` with one slope estimate per date.
#' @export
calc_futures_curve_slope <- function(DT, date_col = "date", time_to_expiry_col = "time_to_expiry", price_col = "close") {
  .validate_futures_curve_panel(DT, c(date_col, time_to_expiry_col, price_col))

  out <- DT[,
    {
      x <- get(time_to_expiry_col)
      y <- log(get(price_col))
      valid <- is.finite(x) & is.finite(y)
      slope <- if (sum(valid) >= 2) stats::coef(stats::lm(y[valid] ~ x[valid]))[[2]] else NA_real_
      .(curve_slope = slope)
    },
    by = date_col
  ]
  data.table::setorderv(out, date_col)
  out[]
}

#' Compute Futures Curve Butterfly
#'
#' Computes a simple three-point butterfly from selected futures-curve ranks.
#'
#' @param DT A `data.table` futures panel containing date, contract-rank, and
#'   price columns.
#' @param date_col Date or timestamp column name.
#' @param rank_col Contract-rank column name.
#' @param price_col Price column name.
#' @param ranks Integer vector of length three giving the front, belly, and
#'   back contract ranks.
#'
#' @return A `data.table` with one butterfly value per date.
#' @export
calc_futures_curve_butterfly <- function(DT, date_col = "date", rank_col = "contract_rank", price_col = "close", ranks = c(1L, 2L, 3L)) {
  stopifnot(length(ranks) == 3)
  .validate_futures_curve_panel(DT, c(date_col, rank_col, price_col))

  legs <- lapply(seq_along(ranks), function(i) {
    leg <- data.table::copy(DT[get(rank_col) == ranks[i], c(date_col, price_col), with = FALSE])
    data.table::setnames(leg, price_col, paste0("px_", i))
    leg
  })

  out <- Reduce(function(x, y) x[y, on = date_col, nomatch = 0], legs)
  colname <- paste0("curve_butterfly_", paste(ranks, collapse = "_"))
  out[, (colname) := 2 * px_2 - px_1 - px_3]
  data.table::setorderv(out, date_col)
  out[, c("px_1", "px_2", "px_3") := NULL]
  out[]
}

#' Compute Annualized Roll Yield
#'
#' Computes the annualized front-versus-deferred roll yield from a futures curve
#' panel. Positive values indicate backwardation and negative values indicate
#' contango under the selected pair.
#'
#' @param DT A `data.table` futures panel containing date, contract-rank,
#'   price, and time-to-expiry columns.
#' @param date_col Date or timestamp column name.
#' @param rank_col Contract-rank column name.
#' @param price_col Price column name.
#' @param time_to_expiry_col Time-to-expiry column name expressed in years.
#' @param rank_front Integer front-contract rank.
#' @param rank_deferred Integer deferred-contract rank.
#'
#' @return A `data.table` with annualized roll yield per date.
#' @export
calc_roll_yield <- function(DT, date_col = "date", rank_col = "contract_rank", price_col = "close", time_to_expiry_col = "time_to_expiry", rank_front = 1L, rank_deferred = 2L) {
  out <- .extract_curve_pair(DT, date_col, rank_col, price_col, time_to_expiry_col, rank_front, rank_deferred)
  tau_diff <- out$deferred_tau - out$front_tau
  roll_yield <- -log(out$deferred_price / out$front_price) / tau_diff
  roll_yield[!is.finite(roll_yield) | tau_diff <= 0] <- NA_real_
  out[, (paste0("roll_yield_", rank_front, "_", rank_deferred)) := roll_yield]
  out[]
}

#' Compute Contango-Backwardation Regime
#'
#' Classifies the front-versus-deferred futures curve regime per date as
#' contango (`1`), backwardation (`-1`), or flat (`0`).
#'
#' @param DT A `data.table` futures panel containing date, contract-rank, and
#'   price columns.
#' @param date_col Date or timestamp column name.
#' @param rank_col Contract-rank column name.
#' @param price_col Price column name.
#' @param rank_front Integer front-contract rank.
#' @param rank_deferred Integer deferred-contract rank.
#'
#' @return A `data.table` with one regime code per date.
#' @export
calc_contango_backwardation <- function(DT, date_col = "date", rank_col = "contract_rank", price_col = "close", rank_front = 1L, rank_deferred = 2L) {
  out <- .extract_curve_pair(DT, date_col, rank_col, price_col, time_to_expiry_col = NULL, rank_front, rank_deferred)
  regime <- sign(out$deferred_price - out$front_price)
  out[, (paste0("contango_backwardation_", rank_front, "_", rank_deferred)) := regime]
  out[]
}

#' Compute Term-Structure Carry
#'
#' Computes a simple annualized term-structure carry approximation from the
#' front-versus-deferred futures pair. Positive values indicate positive carry
#' from curve roll-down under backwardation.
#'
#' @param DT A `data.table` futures panel containing date, contract-rank,
#'   price, and time-to-expiry columns.
#' @param date_col Date or timestamp column name.
#' @param rank_col Contract-rank column name.
#' @param price_col Price column name.
#' @param time_to_expiry_col Time-to-expiry column name expressed in years.
#' @param rank_front Integer front-contract rank.
#' @param rank_deferred Integer deferred-contract rank.
#'
#' @return A `data.table` with annualized term-structure carry per date.
#' @export
calc_term_structure_carry <- function(DT, date_col = "date", rank_col = "contract_rank", price_col = "close", time_to_expiry_col = "time_to_expiry", rank_front = 1L, rank_deferred = 2L) {
  out <- .extract_curve_pair(DT, date_col, rank_col, price_col, time_to_expiry_col, rank_front, rank_deferred)
  tau_diff <- out$deferred_tau - out$front_tau
  carry <- (out$front_price / out$deferred_price - 1) / tau_diff
  carry[!is.finite(carry) | tau_diff <= 0] <- NA_real_
  out[, (paste0("term_structure_carry_", rank_front, "_", rank_deferred)) := carry]
  out[]
}
