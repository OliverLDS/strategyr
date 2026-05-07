.ensure_funding_basis_convergence_features <- function(DT, z_n = 20L, basis_col = NULL, spot_col = "spot", forward_col = "forward", domestic_rate_col = "r_domestic", foreign_rate_col = "r_foreign", tenor_col = "tenor_years", tenor_tag = "1m", funding_col = NULL, funding_weight = 1.0, sample = TRUE) {
  if (is.null(basis_col)) {
    basis_col <- paste0("fx_basis_", tenor_tag)
  }

  if (!basis_col %in% names(DT)) {
    calc_fx_basis(
      DT,
      spot_col = spot_col,
      forward_col = forward_col,
      domestic_rate_col = domestic_rate_col,
      foreign_rate_col = foreign_rate_col,
      tenor_col = tenor_col,
      tenor_tag = tenor_tag
    )
  }

  signal_col <- paste0("funding_basis_signal_", tenor_tag)
  if (!signal_col %in% names(DT)) {
    signal <- DT[[basis_col]]
    if (!is.null(funding_col)) {
      .validate_market_dt(DT, funding_col)
      signal <- signal + funding_weight * DT[[funding_col]]
    }
    signal[!is.finite(signal)] <- NA_real_
    data.table::set(DT, j = signal_col, value = signal)
  }

  z_col <- paste0("zscore_", signal_col, "_", z_n)
  if (!z_col %in% names(DT)) {
    calc_zscore(DT, cols = signal_col, ns = z_n, sample = sample)
  }

  invisible(c(basis_col, signal_col, z_col))
}

.funding_basis_convergence_signal <- function(zscore_value, entry_z = 2, exit_z = 0.5, target_size = 1.0) {
  out <- rep(0.0, length(zscore_value))
  pos_now <- 0.0

  for (i in seq_along(zscore_value)) {
    z <- zscore_value[i]
    if (is.na(z)) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && z >= -exit_z) {
      pos_now <- 0.0
    } else if (pos_now < 0 && z <= exit_z) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (z <= -entry_z) {
        pos_now <- target_size
      } else if (z >= entry_z) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Funding-Basis-Convergence Target Positions
#'
#' Generates a mean-reversion target-position path from a basis-plus-funding
#' dislocation signal. The strategy fades extreme negative or positive
#' standardized dislocations and exits as the signal normalizes.
#'
#' @param DT A `data.table` containing a precomputed basis column or the inputs
#'   needed to build an FX-style basis.
#' @param z_n Integer rolling window used for z-scores.
#' @param entry_z Numeric absolute z-score threshold used for entries.
#' @param exit_z Numeric absolute z-score threshold used for exits.
#' @param basis_col Optional basis column name. Defaults to `fx_basis_<tag>`.
#' @param spot_col Spot-price column name used when basis must be built.
#' @param forward_col Forward-price column name used when basis must be built.
#' @param domestic_rate_col Domestic-rate column used when basis must be built.
#' @param foreign_rate_col Foreign-rate column used when basis must be built.
#' @param tenor_col Tenor-in-years column used when basis must be built.
#' @param tenor_tag Character label appended to the built basis column name.
#' @param funding_col Optional funding-rate column added into the signal.
#' @param funding_weight Numeric scaling applied to `funding_col`.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing basis, signal, and
#'   z-score features are added to `DT` in place.
#' @param sample Logical; if `TRUE`, rolling z-score uses sample standard
#'   deviation.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_funding_basis_convergence_tgt_pos <- function(DT, z_n = 20L, entry_z = 2, exit_z = 0.5, basis_col = NULL, spot_col = "spot", forward_col = "forward", domestic_rate_col = "r_domestic", foreign_rate_col = "r_foreign", tenor_col = "tenor_years", tenor_tag = "1m", funding_col = NULL, funding_weight = 1.0, target_size = 1.0, compute_features = TRUE, sample = TRUE, debug = FALSE) {
  signal_col <- paste0("funding_basis_signal_", tenor_tag)
  z_col <- paste0("zscore_", signal_col, "_", z_n)

  if (compute_features) {
    cols_needed <- .ensure_funding_basis_convergence_features(
      DT,
      z_n = z_n,
      basis_col = basis_col,
      spot_col = spot_col,
      forward_col = forward_col,
      domestic_rate_col = domestic_rate_col,
      foreign_rate_col = foreign_rate_col,
      tenor_col = tenor_col,
      tenor_tag = tenor_tag,
      funding_col = funding_col,
      funding_weight = funding_weight,
      sample = sample
    )
  } else {
    cols_needed <- z_col
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .funding_basis_convergence_signal(
    zscore_value = DT[[z_col]],
    entry_z = entry_z,
    exit_z = exit_z,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Funding-Basis-Convergence Action Plan
#'
#' Applies the funding-and-basis convergence rule to the latest row and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_funding_basis_convergence_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_funding_basis_convergence_action_plan <- function(DT, state, z_n = 20L, entry_z = 2, exit_z = 0.5, basis_col = NULL, spot_col = "spot", forward_col = "forward", domestic_rate_col = "r_domestic", foreign_rate_col = "r_foreign", tenor_col = "tenor_years", tenor_tag = "1m", funding_col = NULL, funding_weight = 1.0, target_size = 1.0, compute_features = TRUE, sample = TRUE, strat_id = 611L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_funding_basis_convergence_tgt_pos(
    DT,
    z_n = z_n,
    entry_z = entry_z,
    exit_z = exit_z,
    basis_col = basis_col,
    spot_col = spot_col,
    forward_col = forward_col,
    domestic_rate_col = domestic_rate_col,
    foreign_rate_col = foreign_rate_col,
    tenor_col = tenor_col,
    tenor_tag = tenor_tag,
    funding_col = funding_col,
    funding_weight = funding_weight,
    target_size = target_size,
    compute_features = compute_features,
    sample = sample,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}
