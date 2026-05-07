.fx_trend_ema_colname <- function(spot_col, trend_n) {
  if (identical(spot_col, "close")) {
    return(paste0("ema_", trend_n))
  }
  paste0("ema_", trend_n, "_", spot_col)
}

.ensure_fx_carry_trend_features <- function(DT, domestic_rate_col, foreign_rate_col, tenor_col = NULL, tenor_tag = "1m", trend_n = 50L, spot_col = "spot") {
  carry_col <- .ensure_fx_carry_features(DT, domestic_rate_col = domestic_rate_col, foreign_rate_col = foreign_rate_col, tenor_col = tenor_col, tenor_tag = tenor_tag)
  .validate_market_dt(DT, spot_col)

  ema_col <- .fx_trend_ema_colname(spot_col, trend_n)
  if (!ema_col %in% names(DT)) {
    ema_value <- ema_ttr_fixed_step(DT[[spot_col]], trend_n, FALSE)
    data.table::set(DT, j = ema_col, value = ema_value)
  }

  invisible(c(carry_col, spot_col, ema_col))
}

.fx_carry_trend_signal <- function(carry_value, spot, ema_value, long_threshold = 0, short_threshold = 0, target_size = 1.0) {
  out <- rep(0.0, length(carry_value))
  valid <- !is.na(carry_value) & !is.na(spot) & !is.na(ema_value)
  out[valid & carry_value > long_threshold & spot > ema_value] <- target_size
  out[valid & carry_value < short_threshold & spot < ema_value] <- -target_size
  out
}

#' FX-Carry-Trend Target Positions
#'
#' Generates an FX target-position path that requires both carry and spot trend
#' agreement. Positive carry only targets a long exposure when spot is above its
#' EMA, while negative carry only targets a short exposure when spot is below
#' its EMA.
#'
#' @param DT A `data.table` containing FX carry inputs or a precomputed
#'   `fx_carry_*` column.
#' @param domestic_rate_col Domestic annualized rate column name.
#' @param foreign_rate_col Foreign annualized rate column name.
#' @param tenor_col Optional tenor-in-years column name.
#' @param tenor_tag Character label appended to the carry column name.
#' @param trend_n Integer EMA window used for the spot trend filter.
#' @param spot_col Spot-price column name.
#' @param long_threshold Numeric threshold above which carry is treated as
#'   positive enough to go long.
#' @param short_threshold Numeric threshold below which carry is treated as
#'   negative enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing carry and EMA features
#'   are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_fx_carry_trend_tgt_pos <- function(DT, domestic_rate_col = "r_domestic", foreign_rate_col = "r_foreign", tenor_col = NULL, tenor_tag = "1m", trend_n = 50L, spot_col = "spot", long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  carry_col <- paste0("fx_carry_", tenor_tag)
  ema_col <- .fx_trend_ema_colname(spot_col, trend_n)

  if (compute_features) {
    cols_needed <- .ensure_fx_carry_trend_features(
      DT,
      domestic_rate_col = domestic_rate_col,
      foreign_rate_col = foreign_rate_col,
      tenor_col = tenor_col,
      tenor_tag = tenor_tag,
      trend_n = trend_n,
      spot_col = spot_col
    )
  } else {
    cols_needed <- c(carry_col, spot_col, ema_col)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .fx_carry_trend_signal(
    carry_value = DT[[carry_col]],
    spot = DT[[spot_col]],
    ema_value = DT[[ema_col]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' FX-Carry-Trend Action Plan
#'
#' Applies the FX-carry-plus-trend rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_fx_carry_trend_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_fx_carry_trend_action_plan <- function(DT, state, domestic_rate_col = "r_domestic", foreign_rate_col = "r_foreign", tenor_col = NULL, tenor_tag = "1m", trend_n = 50L, spot_col = "spot", long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, strat_id = 612L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_fx_carry_trend_tgt_pos(
    DT,
    domestic_rate_col = domestic_rate_col,
    foreign_rate_col = foreign_rate_col,
    tenor_col = tenor_col,
    tenor_tag = tenor_tag,
    trend_n = trend_n,
    spot_col = spot_col,
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
