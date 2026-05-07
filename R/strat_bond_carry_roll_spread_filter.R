.ensure_bond_carry_roll_spread_filter_features <- function(DT, par_col = "par", c_rate_col = "c_rate", T_col = "T", freq_col = "freq", ytm_col = "ytm", spread_col = "credit_spread", accrual_frac_col = NULL, holding_years_col = NULL, funding_rate_col = NULL, min_long_spread = NULL, max_short_spread = NULL) {
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
  if (!is.null(min_long_spread) || !is.null(max_short_spread)) {
    .validate_market_dt(DT, spread_col)
    cols_needed <- c(cols_needed, spread_col)
  }
  invisible(cols_needed)
}

.bond_carry_roll_spread_filter_signal <- function(carry_value, roll_value, spread_value = NULL, long_threshold = 0, short_threshold = 0, min_long_spread = NULL, max_short_spread = NULL, target_size = 1.0) {
  score <- carry_value + roll_value
  out <- rep(0.0, length(score))
  valid <- !is.na(score)

  long_ok <- valid & score > long_threshold
  short_ok <- valid & score < short_threshold
  if (!is.null(min_long_spread)) {
    long_ok <- long_ok & !is.na(spread_value) & spread_value >= min_long_spread
  }
  if (!is.null(max_short_spread)) {
    short_ok <- short_ok & !is.na(spread_value) & spread_value <= max_short_spread
  }

  out[long_ok] <- target_size
  out[short_ok] <- -target_size
  out
}

#' Bond-Carry-and-Roll-Spread-Filter Target Positions
#'
#' Generates a bond carry-and-roll target-position path that applies optional
#' spread filters before allowing long or short exposure.
#'
#' @param DT A `data.table` containing bond carry-and-roll inputs or precomputed
#'   `bond_carry` and `bond_roll_down_return` columns.
#' @param par_col Face-value column name.
#' @param c_rate_col Coupon-rate column name.
#' @param T_col Maturity column name in years.
#' @param freq_col Coupon-frequency column name.
#' @param ytm_col Yield-to-maturity column name.
#' @param spread_col Credit-spread column name used for optional trade filters.
#' @param accrual_frac_col Optional accrual-fraction column name.
#' @param holding_years_col Optional holding-horizon column name in years.
#' @param funding_rate_col Optional financing-rate column name.
#' @param long_threshold Numeric threshold above which carry-and-roll is treated
#'   as positive enough to go long.
#' @param short_threshold Numeric threshold below which carry-and-roll is
#'   treated as negative enough to go short.
#' @param min_long_spread Optional minimum spread required to allow long
#'   exposure.
#' @param max_short_spread Optional maximum spread allowed to permit short
#'   exposure.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing carry and roll-down
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_bond_carry_roll_spread_filter_tgt_pos <- function(DT, par_col = "par", c_rate_col = "c_rate", T_col = "T", freq_col = "freq", ytm_col = "ytm", spread_col = "credit_spread", accrual_frac_col = NULL, holding_years_col = NULL, funding_rate_col = NULL, long_threshold = 0, short_threshold = 0, min_long_spread = NULL, max_short_spread = NULL, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_bond_carry_roll_spread_filter_features(
      DT,
      par_col = par_col,
      c_rate_col = c_rate_col,
      T_col = T_col,
      freq_col = freq_col,
      ytm_col = ytm_col,
      spread_col = spread_col,
      accrual_frac_col = accrual_frac_col,
      holding_years_col = holding_years_col,
      funding_rate_col = funding_rate_col,
      min_long_spread = min_long_spread,
      max_short_spread = max_short_spread
    )
  } else {
    cols_needed <- c("bond_carry", "bond_roll_down_return")
    if (!is.null(min_long_spread) || !is.null(max_short_spread)) {
      cols_needed <- c(cols_needed, spread_col)
    }
    .validate_market_dt(DT, cols_needed)
  }

  spread_value <- if (spread_col %in% names(DT)) DT[[spread_col]] else NULL
  tgt_pos <- .bond_carry_roll_spread_filter_signal(
    carry_value = DT[["bond_carry"]],
    roll_value = DT[["bond_roll_down_return"]],
    spread_value = spread_value,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    min_long_spread = min_long_spread,
    max_short_spread = max_short_spread,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Bond-Carry-and-Roll-Spread-Filter Action Plan
#'
#' Applies the bond carry-and-roll rule with optional spread filters to the
#' latest row and translates the resulting target exposure into an executable
#' action plan.
#'
#' @inheritParams strat_bond_carry_roll_spread_filter_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_bond_carry_roll_spread_filter_action_plan <- function(DT, state, par_col = "par", c_rate_col = "c_rate", T_col = "T", freq_col = "freq", ytm_col = "ytm", spread_col = "credit_spread", accrual_frac_col = NULL, holding_years_col = NULL, funding_rate_col = NULL, long_threshold = 0, short_threshold = 0, min_long_spread = NULL, max_short_spread = NULL, target_size = 1.0, compute_features = TRUE, strat_id = 606L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_bond_carry_roll_spread_filter_tgt_pos(
    DT,
    par_col = par_col,
    c_rate_col = c_rate_col,
    T_col = T_col,
    freq_col = freq_col,
    ytm_col = ytm_col,
    spread_col = spread_col,
    accrual_frac_col = accrual_frac_col,
    holding_years_col = holding_years_col,
    funding_rate_col = funding_rate_col,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    min_long_spread = min_long_spread,
    max_short_spread = max_short_spread,
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
