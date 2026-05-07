.ensure_gamma_scalp_support_features <- function(DT, rv_n = 20L, iv_col = "iv", annualization = 252) {
  .validate_market_dt(DT, c("close", "T", iv_col))
  rv_col <- paste0("rv_", rv_n)
  if (!rv_col %in% names(DT)) {
    calc_realized_vol(DT, ns = rv_n, annualization = annualization)
  }
  edge_col <- paste0("gamma_scalp_edge_", rv_n)
  if (!edge_col %in% names(DT)) {
    edge <- DT[[rv_col]] - DT[[iv_col]]
    edge[!is.finite(edge)] <- NA_real_
    data.table::set(DT, j = edge_col, value = edge)
  }
  invisible(c(iv_col, "T", rv_col, edge_col))
}

.gamma_scalp_support_signal <- function(edge_value, T_value, edge_threshold = 0, min_T = 5 / 252, max_T = 90 / 252, target_size = 1.0) {
  out <- rep(0.0, length(edge_value))
  valid <- !is.na(edge_value) & !is.na(T_value) & T_value >= min_T & T_value <= max_T
  out[valid & edge_value >= edge_threshold] <- target_size
  out
}

#' Gamma-Scalp-Support Target Positions
#'
#' Generates a support target-position path for long-gamma structures. The
#' strategy is active when realized volatility is sufficiently rich relative to
#' implied volatility and option expiry remains within a tradable window.
#'
#' @param DT A `data.table` containing `close`, time-to-expiry `T`, and an
#'   implied-volatility column.
#' @param rv_n Integer realized-volatility window.
#' @param iv_col Implied-volatility column name.
#' @param annualization Numeric annualization factor passed to
#'   `calc_realized_vol()`.
#' @param edge_threshold Minimum realized-minus-implied volatility edge required
#'   to activate the long-gamma support position.
#' @param min_T Minimum time to expiry in years.
#' @param max_T Maximum time to expiry in years.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing realized-volatility and
#'   gamma-edge features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_gamma_scalp_support_tgt_pos <- function(DT, rv_n = 20L, iv_col = "iv", annualization = 252, edge_threshold = 0, min_T = 5 / 252, max_T = 90 / 252, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  edge_col <- paste0("gamma_scalp_edge_", rv_n)
  if (compute_features) {
    cols_needed <- .ensure_gamma_scalp_support_features(DT, rv_n = rv_n, iv_col = iv_col, annualization = annualization)
  } else {
    cols_needed <- c("T", edge_col)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .gamma_scalp_support_signal(
    edge_value = DT[[edge_col]],
    T_value = DT[["T"]],
    edge_threshold = edge_threshold,
    min_T = min_T,
    max_T = max_T,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Gamma-Scalp-Support Action Plan
#'
#' Applies the long-gamma support rule to the latest row, translates the
#' resulting target exposure into an executable action plan, and optionally
#' attaches a delta-hedge adjustment recommendation.
#'
#' @inheritParams strat_gamma_scalp_support_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#' @param current_delta Optional current portfolio delta used for the hedge
#'   suggestion.
#' @param hedge_delta Optional delta contribution per hedge unit used for the
#'   hedge suggestion.
#' @param target_delta Numeric target delta used for the hedge suggestion.
#'
#' @return A list with an action plan and, when hedge inputs are supplied, a
#'   delta-hedge adjustment table.
#' @export
strat_gamma_scalp_support_action_plan <- function(DT, state, rv_n = 20L, iv_col = "iv", annualization = 252, edge_threshold = 0, min_T = 5 / 252, max_T = 90 / 252, target_size = 1.0, compute_features = TRUE, strat_id = 708L, tol_pos = 0.1, current_delta = NULL, hedge_delta = NULL, target_delta = 0, debug = FALSE) {
  tgt_pos <- strat_gamma_scalp_support_tgt_pos(
    DT,
    rv_n = rv_n,
    iv_col = iv_col,
    annualization = annualization,
    edge_threshold = edge_threshold,
    min_T = min_T,
    max_T = max_T,
    target_size = target_size,
    compute_features = compute_features,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)

  hedge_plan <- NULL
  if (!is.null(current_delta) && !is.null(hedge_delta)) {
    hedge_plan <- plan_delta_neutral_adjustment(current_delta = current_delta, target_delta = target_delta, hedge_delta = hedge_delta)
  }

  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos, hedge_plan = hedge_plan))
  }

  if (is.null(hedge_plan)) {
    return(plan)
  }
  list(plan = plan, hedge_plan = hedge_plan)
}
