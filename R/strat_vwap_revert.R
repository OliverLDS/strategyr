.ensure_vwap_revert_features <- function(DT, vwap_n, bid_col = "bid", ask_col = "ask", bid_size_col = "bid_size", ask_size_col = "ask_size") {
  .validate_market_dt(DT, c("close", "volume"))

  vwap_col <- paste0("vwap_", vwap_n)
  if (!vwap_col %in% names(DT)) {
    calc_VWAP(DT, ns = vwap_n)
  }

  if (all(c(bid_col, ask_col) %in% names(DT)) && !"bid_ask_spread_rel" %in% names(DT)) {
    calc_bid_ask_spread(DT, bid_col = bid_col, ask_col = ask_col)
  }
  if (all(c(bid_size_col, ask_size_col) %in% names(DT)) && !"order_imbalance" %in% names(DT)) {
    calc_order_imbalance(DT, bid_size_col = bid_size_col, ask_size_col = ask_size_col)
  }

  dev_col <- paste0("vwap_dev_", vwap_n)
  if (!dev_col %in% names(DT)) {
    dev <- DT[["close"]] / DT[[vwap_col]] - 1
    dev[!is.finite(dev)] <- NA_real_
    data.table::set(DT, j = dev_col, value = dev)
  }

  cols_needed <- c(vwap_col, dev_col)
  if ("order_imbalance" %in% names(DT)) {
    cols_needed <- c(cols_needed, "order_imbalance")
  }
  if ("bid_ask_spread_rel" %in% names(DT)) {
    cols_needed <- c(cols_needed, "bid_ask_spread_rel")
  }
  invisible(cols_needed)
}

.vwap_revert_signal <- function(vwap_dev, order_imbalance = NULL, rel_spread = NULL, entry_dev = 0.01, exit_dev = 0.0025, min_long_imbalance = -0.2, max_short_imbalance = 0.2, max_rel_spread = NULL, target_size = 1.0) {
  out <- rep(0.0, length(vwap_dev))
  pos_now <- 0.0

  for (i in seq_along(vwap_dev)) {
    dev <- vwap_dev[i]
    imb <- if (is.null(order_imbalance)) NA_real_ else order_imbalance[i]
    spr <- if (is.null(rel_spread)) NA_real_ else rel_spread[i]

    if (is.na(dev)) {
      out[i] <- pos_now
      next
    }

    spread_ok <- is.null(max_rel_spread) || is.na(spr) || spr <= max_rel_spread
    if (pos_now > 0 && dev >= -exit_dev) {
      pos_now <- 0.0
    } else if (pos_now < 0 && dev <= exit_dev) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0 && spread_ok) {
      if (dev <= -entry_dev && (is.null(order_imbalance) || is.na(imb) || imb >= min_long_imbalance)) {
        pos_now <- target_size
      } else if (dev >= entry_dev && (is.null(order_imbalance) || is.na(imb) || imb <= max_short_imbalance)) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }
  out
}

#' VWAP-Reversion Target Positions
#'
#' Generates a fair-value reversion target-position path from deviations between
#' price and rolling VWAP. Large positive or negative deviations open a
#' contrarian target, optionally filtered by queue imbalance and relative
#' bid-ask spread.
#'
#' @param DT A `data.table` containing at least `close` and `volume`.
#' @param vwap_n Integer rolling VWAP window.
#' @param entry_dev Numeric absolute VWAP-deviation threshold used for entries.
#' @param exit_dev Numeric absolute VWAP-deviation threshold used for exits.
#' @param min_long_imbalance Minimum acceptable order imbalance for long
#'   entries. More negative values allow stronger selling pressure.
#' @param max_short_imbalance Maximum acceptable order imbalance for short
#'   entries. More positive values allow stronger buying pressure.
#' @param max_rel_spread Optional maximum relative bid-ask spread filter.
#' @param bid_col Bid-price column name used when bid-ask features are missing.
#' @param ask_col Ask-price column name used when bid-ask features are missing.
#' @param bid_size_col Bid-size column name used when imbalance features are
#'   missing.
#' @param ask_size_col Ask-size column name used when imbalance features are
#'   missing.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing VWAP and optional
#'   microstructure features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_vwap_revert_tgt_pos <- function(DT, vwap_n = 20L, entry_dev = 0.01, exit_dev = 0.0025, min_long_imbalance = -0.2, max_short_imbalance = 0.2, max_rel_spread = NULL, bid_col = "bid", ask_col = "ask", bid_size_col = "bid_size", ask_size_col = "ask_size", target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  dev_col <- paste0("vwap_dev_", vwap_n)
  if (compute_features) {
    cols_needed <- .ensure_vwap_revert_features(
      DT,
      vwap_n = vwap_n,
      bid_col = bid_col,
      ask_col = ask_col,
      bid_size_col = bid_size_col,
      ask_size_col = ask_size_col
    )
  } else {
    cols_needed <- c(dev_col)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .vwap_revert_signal(
    vwap_dev = DT[[dev_col]],
    order_imbalance = if ("order_imbalance" %in% names(DT)) DT[["order_imbalance"]] else NULL,
    rel_spread = if ("bid_ask_spread_rel" %in% names(DT)) DT[["bid_ask_spread_rel"]] else NULL,
    entry_dev = entry_dev,
    exit_dev = exit_dev,
    min_long_imbalance = min_long_imbalance,
    max_short_imbalance = max_short_imbalance,
    max_rel_spread = max_rel_spread,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' VWAP-Reversion Action Plan
#'
#' Applies the VWAP-reversion rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_vwap_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_vwap_revert_action_plan <- function(DT, state, vwap_n = 20L, entry_dev = 0.01, exit_dev = 0.0025, min_long_imbalance = -0.2, max_short_imbalance = 0.2, max_rel_spread = NULL, bid_col = "bid", ask_col = "ask", bid_size_col = "bid_size", ask_size_col = "ask_size", target_size = 1.0, compute_features = TRUE, strat_id = 506L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_vwap_revert_tgt_pos(
    DT,
    vwap_n = vwap_n,
    entry_dev = entry_dev,
    exit_dev = exit_dev,
    min_long_imbalance = min_long_imbalance,
    max_short_imbalance = max_short_imbalance,
    max_rel_spread = max_rel_spread,
    bid_col = bid_col,
    ask_col = ask_col,
    bid_size_col = bid_size_col,
    ask_size_col = ask_size_col,
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
