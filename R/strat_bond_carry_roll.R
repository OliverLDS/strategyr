.ensure_bond_carry_roll_features <- function(DT, par_col = "par", c_rate_col = "c_rate", T_col = "T", freq_col = "freq", ytm_col = "ytm", accrual_frac_col = NULL, holding_years_col = NULL, funding_rate_col = NULL) {
  required <- c(par_col, c_rate_col, T_col, freq_col, ytm_col)
  if (!is.null(accrual_frac_col)) required <- c(required, accrual_frac_col)
  if (!is.null(holding_years_col)) required <- c(required, holding_years_col)
  if (!is.null(funding_rate_col)) required <- c(required, funding_rate_col)
  .validate_market_dt(DT, required)

  if (!"bond_carry" %in% names(DT)) {
    carry_vec <- mapply(
      function(par, c_rate, T, freq, ytm, accrual_frac, holding_years, funding_rate) {
        calc_bond_carry(
          par = par,
          c_rate = c_rate,
          T = T,
          freq = freq,
          ytm = ytm,
          accrual_frac = accrual_frac,
          holding_years = holding_years,
          funding_rate = funding_rate
        )
      },
      par = DT[[par_col]],
      c_rate = DT[[c_rate_col]],
      T = DT[[T_col]],
      freq = DT[[freq_col]],
      ytm = DT[[ytm_col]],
      accrual_frac = if (is.null(accrual_frac_col)) rep(0, nrow(DT)) else DT[[accrual_frac_col]],
      holding_years = if (is.null(holding_years_col)) 1 / DT[[freq_col]] else DT[[holding_years_col]],
      funding_rate = if (is.null(funding_rate_col)) rep(0, nrow(DT)) else DT[[funding_rate_col]]
    )
    data.table::set(DT, j = "bond_carry", value = as.numeric(carry_vec))
  }

  if (!"bond_roll_down_return" %in% names(DT)) {
    roll_vec <- mapply(
      function(par, c_rate, T, freq, ytm, accrual_frac, holding_years) {
        calc_bond_roll_down_return(
          par = par,
          c_rate = c_rate,
          T = T,
          freq = freq,
          ytm = ytm,
          accrual_frac = accrual_frac,
          holding_years = holding_years
        )
      },
      par = DT[[par_col]],
      c_rate = DT[[c_rate_col]],
      T = DT[[T_col]],
      freq = DT[[freq_col]],
      ytm = DT[[ytm_col]],
      accrual_frac = if (is.null(accrual_frac_col)) rep(0, nrow(DT)) else DT[[accrual_frac_col]],
      holding_years = if (is.null(holding_years_col)) 1 / DT[[freq_col]] else DT[[holding_years_col]]
    )
    data.table::set(DT, j = "bond_roll_down_return", value = as.numeric(roll_vec))
  }

  invisible(c("bond_carry", "bond_roll_down_return"))
}

.bond_carry_roll_signal <- function(carry_value, roll_value, long_threshold = 0, short_threshold = 0, target_size = 1.0) {
  score <- carry_value + roll_value
  out <- rep(0.0, length(score))
  valid <- !is.na(score)
  out[valid & score > long_threshold] <- target_size
  out[valid & score < short_threshold] <- -target_size
  out
}

#' Bond-Carry-and-Roll Target Positions
#'
#' Generates a simple bond target-position path from carry plus roll-down
#' return. Positive carry-and-roll targets a long exposure and negative
#' carry-and-roll targets a short exposure.
#'
#' @param DT A `data.table` containing bond carry-and-roll inputs or precomputed
#'   `bond_carry` and `bond_roll_down_return` columns.
#' @param par_col Face-value column name.
#' @param c_rate_col Coupon-rate column name.
#' @param T_col Maturity column name in years.
#' @param freq_col Coupon-frequency column name.
#' @param ytm_col Yield-to-maturity column name.
#' @param accrual_frac_col Optional accrual-fraction column name.
#' @param holding_years_col Optional holding-horizon column name in years.
#' @param funding_rate_col Optional financing-rate column name.
#' @param long_threshold Numeric threshold above which carry-and-roll is treated
#'   as positive enough to go long.
#' @param short_threshold Numeric threshold below which carry-and-roll is
#'   treated as negative enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing carry and roll-down
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_bond_carry_roll_tgt_pos <- function(DT, par_col = "par", c_rate_col = "c_rate", T_col = "T", freq_col = "freq", ytm_col = "ytm", accrual_frac_col = NULL, holding_years_col = NULL, funding_rate_col = NULL, long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_bond_carry_roll_features(
      DT,
      par_col = par_col,
      c_rate_col = c_rate_col,
      T_col = T_col,
      freq_col = freq_col,
      ytm_col = ytm_col,
      accrual_frac_col = accrual_frac_col,
      holding_years_col = holding_years_col,
      funding_rate_col = funding_rate_col
    )
  } else {
    cols_needed <- c("bond_carry", "bond_roll_down_return")
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .bond_carry_roll_signal(
    carry_value = DT[["bond_carry"]],
    roll_value = DT[["bond_roll_down_return"]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Bond-Carry-and-Roll Action Plan
#'
#' Applies the bond carry-and-roll rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_bond_carry_roll_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_bond_carry_roll_action_plan <- function(DT, state, par_col = "par", c_rate_col = "c_rate", T_col = "T", freq_col = "freq", ytm_col = "ytm", accrual_frac_col = NULL, holding_years_col = NULL, funding_rate_col = NULL, long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, strat_id = 602L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_bond_carry_roll_tgt_pos(
    DT,
    par_col = par_col,
    c_rate_col = c_rate_col,
    T_col = T_col,
    freq_col = freq_col,
    ytm_col = ytm_col,
    accrual_frac_col = accrual_frac_col,
    holding_years_col = holding_years_col,
    funding_rate_col = funding_rate_col,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size,
    compute_features = compute_features,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}
