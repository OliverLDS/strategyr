#' Backtest Performance Metrics
#'
#' Computes compact performance metrics from a path-dependent backtest equity
#' curve. The primary score is Sortino ratio, using log returns and downside
#' deviation below a minimum acceptable annual return.
#'
#' @param equity Numeric equity curve.
#' @param annualization Numeric periods per year.
#' @param risk_free_return Numeric annual risk-free return used for excess
#'   return.
#' @param min_acceptable_return Numeric annual return threshold used for
#'   downside deviation.
#'
#' @return A one-row `data.table` with return, risk, drawdown, and Sortino
#'   metrics.
#' @export
calc_backtest_performance <- function(equity, annualization = 252, risk_free_return = 0, min_acceptable_return = 0) {
  stopifnot(is.numeric(equity))
  stopifnot(length(annualization) == 1L, is.finite(annualization), annualization > 0)
  stopifnot(length(risk_free_return) == 1L, is.finite(risk_free_return))
  stopifnot(length(min_acceptable_return) == 1L, is.finite(min_acceptable_return))

  n_obs <- length(equity)
  empty_metrics <- function(sortino = NA_real_, total_return = NA_real_, annual_return = NA_real_, max_drawdown = NA_real_) {
    return(data.table::data.table(
      n_obs = n_obs,
      total_return = total_return,
      annual_return = annual_return,
      volatility = NA_real_,
      downside_deviation = NA_real_,
      sortino = sortino,
      max_drawdown = max_drawdown
    ))
  }

  if (n_obs < 2L || !is.finite(equity[[1L]]) || equity[[1L]] <= 0) {
    return(empty_metrics())
  }
  if (any(!is.finite(equity))) {
    return(empty_metrics())
  }
  if (any(equity <= 0)) {
    return(empty_metrics(
      sortino = -Inf,
      total_return = -1,
      annual_return = -1,
      max_drawdown = 1
    ))
  }

  log_ret <- diff(log(equity))
  excess_ret <- log_ret - risk_free_return / annualization
  downside_gap <- pmin(log_ret - min_acceptable_return / annualization, 0)
  downside_deviation <- sqrt(mean(downside_gap^2, na.rm = TRUE)) * sqrt(annualization)
  mean_excess <- mean(excess_ret, na.rm = TRUE) * annualization

  sortino <- if (is.finite(downside_deviation) && downside_deviation > 0) {
    mean_excess / downside_deviation
  } else if (is.finite(mean_excess) && mean_excess > 0) {
    Inf
  } else if (is.finite(mean_excess) && mean_excess < 0) {
    -Inf
  } else {
    NA_real_
  }

  eq_norm <- equity / equity[[1L]]
  drawdown <- 1 - eq_norm / cummax(eq_norm)
  total_return <- tail(eq_norm, 1L) - 1
  years <- length(log_ret) / annualization
  annual_return <- if (years > 0) tail(eq_norm, 1L)^(1 / years) - 1 else NA_real_

  data.table::data.table(
    n_obs = n_obs,
    total_return = total_return,
    annual_return = annual_return,
    volatility = stats::sd(log_ret, na.rm = TRUE) * sqrt(annualization),
    downside_deviation = downside_deviation,
    sortino = sortino,
    max_drawdown = max(drawdown, na.rm = TRUE)
  )
}

.mine_param_grid <- function(param_grid) {
  if (data.table::is.data.table(param_grid)) {
    grid <- data.table::copy(param_grid)
  } else if (is.data.frame(param_grid)) {
    grid <- data.table::as.data.table(param_grid)
  } else if (is.list(param_grid) && length(param_grid) > 0L) {
    grid <- data.table::as.data.table(expand.grid(param_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  } else {
    stop("`param_grid` must be a non-empty list, data.frame, or data.table.", call. = FALSE)
  }

  if (nrow(grid) == 0L) {
    stop("`param_grid` must contain at least one parameter row.", call. = FALSE)
  }

  grid
}

.mine_market_dt <- function(DT, from = NULL, to = NULL) {
  .validate_market_dt(DT, c("datetime", "open", "high", "low", "close"))
  market_dt <- data.table::copy(DT)
  market_dt <- market_dt[order(datetime)]

  if (!is.null(from)) {
    market_dt <- market_dt[datetime >= as.POSIXct(from)]
  }
  if (!is.null(to)) {
    market_dt <- market_dt[datetime < as.POSIXct(as.Date(to) + 1)]
  }
  if (nrow(market_dt) == 0L) {
    stop("No market rows remain after applying `from`/`to` filters.", call. = FALSE)
  }

  market_dt
}

.mine_run_strategy_backtest <- function(
  market_dt,
  strategy_fun,
  strategy_params,
  strat_id,
  asset_id,
  ctr_size,
  ctr_step,
  lev,
  fee_rt,
  fund_rt,
  tol_pos,
  rec
) {
  DT_i <- data.table::copy(market_dt)
  tgt_pos <- do.call(strategy_fun, c(list(DT = DT_i), strategy_params))
  if (!is.numeric(tgt_pos) || length(tgt_pos) != nrow(DT_i)) {
    stop("Strategy function must return a numeric target-position vector matching `nrow(DT)`.", call. = FALSE)
  }

  eq <- backtest_rcpp(
    timestamp = as.numeric(DT_i$datetime),
    open = DT_i$open,
    high = DT_i$high,
    low = DT_i$low,
    close = DT_i$close,
    tgt_pos = data.table::fifelse(is.na(tgt_pos), 0, tgt_pos),
    pos_strat = rep(as.integer(strat_id), nrow(DT_i)),
    tol_pos = rep(tol_pos, nrow(DT_i)),
    strat = as.integer(strat_id),
    asset = as.integer(asset_id),
    ctr_size = ctr_size,
    ctr_step = ctr_step,
    lev = lev,
    fee_rt = fee_rt,
    fund_rt = fund_rt,
    rec = rec
  )

  list(tgt_pos = tgt_pos, equity = eq)
}

.mine_order_results <- function(result_dt, score_col) {
  if (!score_col %in% names(result_dt)) {
    stop("`score_col` is not present in the mining result.", call. = FALSE)
  }
  data.table::setorderv(result_dt, cols = score_col, order = -1L, na.last = TRUE)
  result_dt[, rank := seq_len(.N)]
  data.table::setcolorder(result_dt, c("rank", setdiff(names(result_dt), "rank")))
  result_dt
}

.mine_buy_hold_total_return <- function(market_dt) {
  if (nrow(market_dt) < 1L) {
    return(NA_real_)
  }
  first_open <- market_dt$open[[1L]]
  last_close <- market_dt$close[[nrow(market_dt)]]
  if (!is.finite(first_open) || !is.finite(last_close) || first_open <= 0) {
    return(NA_real_)
  }
  last_close / first_open - 1
}

.mine_empty_asset_year_result <- function(param_cols) {
  out <- data.table::data.table(
    rank = integer(),
    asset = character(),
    asset_index = integer(),
    year = integer(),
    param_id = integer()
  )
  for (col in param_cols) {
    out[, (col) := vector("list", 0L)]
  }
  cbind(
    out,
    data.table::data.table(
      n_obs = integer(),
      total_return = numeric(),
      annual_return = numeric(),
      volatility = numeric(),
      downside_deviation = numeric(),
      sortino = numeric(),
      max_drawdown = numeric(),
      buy_hold_total_return = numeric(),
      excess_total_return = numeric()
    )
  )
}

#' Mine Strategy Parameters
#'
#' Loops over a strategy parameter grid for one fixed asset and backtesting
#' period, then ranks parameter sets by Sortino ratio by default.
#'
#' @param DT Candle `data.table` containing `datetime`, `open`, `high`, `low`,
#'   and `close`.
#' @param strategy_fun Strategy target-position function. It must accept `DT` as
#'   its first argument and return a numeric target-position vector.
#' @param param_grid Non-empty list, `data.frame`, or `data.table` of parameter
#'   values. Lists are expanded with `expand.grid()`.
#' @param from,to Optional date filters for the fixed backtesting period.
#' @param score_col Metric used for descending ranking.
#' @param keep_paths Logical; when `TRUE`, include list-columns with target
#'   positions and equity curves.
#' @param strat_id Integer strategy identifier passed to `backtest_rcpp()`.
#' @param asset_id Integer asset identifier passed to `backtest_rcpp()`.
#' @param ctr_size,ctr_step,lev,fee_rt,fund_rt,tol_pos Backtest execution
#'   assumptions passed to `backtest_rcpp()`.
#' @param rec Logical; when `TRUE`, the backtest engine records execution traces
#'   on the equity attribute. Keep this `FALSE` for larger mining jobs.
#' @param annualization,risk_free_return,min_acceptable_return Performance
#'   metric assumptions passed to `calc_backtest_performance()`.
#'
#' @return A ranked `data.table` containing parameter values and performance
#'   metrics.
#' @export
mine_strategy_params <- function(
  DT,
  strategy_fun,
  param_grid,
  from = NULL,
  to = NULL,
  score_col = "sortino",
  keep_paths = FALSE,
  strat_id = 0L,
  asset_id = 0L,
  ctr_size = 1.0,
  ctr_step = 1.0,
  lev = 10.0,
  fee_rt = 0.0,
  fund_rt = 0.0,
  tol_pos = 0.1,
  rec = FALSE,
  annualization = 252,
  risk_free_return = 0,
  min_acceptable_return = 0
) {
  stopifnot(is.function(strategy_fun))
  stopifnot(is.logical(keep_paths), length(keep_paths) == 1L)
  grid <- .mine_param_grid(param_grid)
  market_dt <- .mine_market_dt(DT, from = from, to = to)

  rows <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    params <- as.list(grid[i])
    bt <- .mine_run_strategy_backtest(
      market_dt = market_dt,
      strategy_fun = strategy_fun,
      strategy_params = params,
      strat_id = strat_id,
      asset_id = asset_id,
      ctr_size = ctr_size,
      ctr_step = ctr_step,
      lev = lev,
      fee_rt = fee_rt,
      fund_rt = fund_rt,
      tol_pos = tol_pos,
      rec = rec
    )
    perf <- calc_backtest_performance(
      bt$equity,
      annualization = annualization,
      risk_free_return = risk_free_return,
      min_acceptable_return = min_acceptable_return
    )
    rows[[i]] <- data.table::data.table(param_id = i, grid[i], perf)
    if (keep_paths) {
      rows[[i]][, `:=`(tgt_pos = list(bt$tgt_pos), equity = list(bt$equity))]
    }
  }

  .mine_order_results(data.table::rbindlist(rows, fill = TRUE), score_col = score_col)
}

#' Mine Strategy Parameters Across Asset-Year Pairs
#'
#' First mines the best parameter rows on selected seed assets over the full
#' period. It then evaluates the unique selected parameter rows across every
#' valid asset-year pair, keeps only pairs where strategy total return beats a
#' simple buy-and-hold return, and ranks the survivors by Sortino ratio.
#'
#' @param market_data_list Named or unnamed list of candle `data.table`s.
#' @param strategy_fun Strategy target-position function. It must accept `DT` as
#'   its first argument and return a numeric target-position vector.
#' @param param_grid Non-empty list, `data.frame`, or `data.table` of parameter
#'   values used for seed-asset parameter mining.
#' @param seed_assets Character vector of asset names used to select candidate
#'   parameter rows.
#' @param seed_n_best Integer number of top parameter rows retained per seed
#'   asset.
#' @param asset_names Optional asset labels. Defaults to names of
#'   `market_data_list`, or `asset_1`, `asset_2`, ...
#' @param years Optional integer vector of calendar years to evaluate. Defaults
#'   to all years present after `from`/`to` filtering.
#' @param min_year_rows Minimum OHLC rows required for an asset-year pair.
#' @inheritParams mine_strategy_params
#'
#' @return A list with `seed_params`, `candidate_params`, and
#'   `asset_year_results` `data.table`s.
#' @export
mine_strategy_asset_years <- function(
  market_data_list,
  strategy_fun,
  param_grid,
  seed_assets = c("SPY", "AGG", "IAU", "IBIT", "USO", "UUP"),
  seed_n_best = 1L,
  asset_names = NULL,
  years = NULL,
  from = NULL,
  to = NULL,
  min_year_rows = 200L,
  score_col = "sortino",
  keep_paths = FALSE,
  strat_id = 0L,
  asset_id = 0L,
  ctr_size = 1.0,
  ctr_step = 1.0,
  lev = 10.0,
  fee_rt = 0.0,
  fund_rt = 0.0,
  tol_pos = 0.1,
  rec = FALSE,
  annualization = 252,
  risk_free_return = 0,
  min_acceptable_return = 0
) {
  stopifnot(is.list(market_data_list), length(market_data_list) > 0L)
  stopifnot(is.function(strategy_fun))
  stopifnot(is.character(seed_assets), length(seed_assets) > 0L)
  stopifnot(length(seed_n_best) == 1L, is.finite(seed_n_best), seed_n_best >= 1L)
  stopifnot(length(min_year_rows) == 1L, is.finite(min_year_rows), min_year_rows >= 1L)
  stopifnot(is.logical(keep_paths), length(keep_paths) == 1L)

  grid <- .mine_param_grid(param_grid)
  param_cols <- names(grid)

  if (is.null(asset_names)) {
    asset_names <- names(market_data_list)
    if (is.null(asset_names) || any(asset_names == "")) {
      asset_names <- paste0("asset_", seq_along(market_data_list))
    }
  }
  stopifnot(length(asset_names) == length(market_data_list))

  seed_idx <- match(seed_assets, asset_names)
  keep_seed <- !is.na(seed_idx)
  if (!any(keep_seed)) {
    stop("None of `seed_assets` were found in `asset_names`.", call. = FALSE)
  }
  seed_assets <- seed_assets[keep_seed]
  seed_idx <- seed_idx[keep_seed]

  seed_rows <- vector("list", length(seed_idx))
  for (i in seq_along(seed_idx)) {
    seed_res <- mine_strategy_params(
      DT = market_data_list[[seed_idx[[i]]]],
      strategy_fun = strategy_fun,
      param_grid = grid,
      from = from,
      to = to,
      score_col = score_col,
      keep_paths = FALSE,
      strat_id = strat_id,
      asset_id = asset_id,
      ctr_size = ctr_size,
      ctr_step = ctr_step,
      lev = lev,
      fee_rt = fee_rt,
      fund_rt = fund_rt,
      tol_pos = tol_pos,
      rec = rec,
      annualization = annualization,
      risk_free_return = risk_free_return,
      min_acceptable_return = min_acceptable_return
    )
    top_n <- min(as.integer(seed_n_best), nrow(seed_res))
    seed_rows[[i]] <- seed_res[seq_len(top_n)]
    seed_rows[[i]][, `:=`(
      seed_asset = seed_assets[[i]],
      seed_asset_index = seed_idx[[i]],
      seed_rank = rank
    )]
  }

  seed_params <- data.table::rbindlist(seed_rows, fill = TRUE)
  candidate_params <- unique(seed_params[, ..param_cols])
  candidate_params[, param_id := seq_len(.N)]
  data.table::setcolorder(candidate_params, c("param_id", param_cols))

  rows <- list()
  row_i <- 0L
  for (asset_i in seq_along(market_data_list)) {
    market_dt <- tryCatch(
      .mine_market_dt(market_data_list[[asset_i]], from = from, to = to),
      error = function(e) NULL
    )
    if (is.null(market_dt) || nrow(market_dt) < min_year_rows) {
      next
    }

    market_dt[, .mine_year := as.integer(format(datetime, "%Y"))]
    eval_years <- sort(unique(market_dt$.mine_year))
    if (!is.null(years)) {
      eval_years <- intersect(eval_years, as.integer(years))
    }

    for (yr in eval_years) {
      year_dt <- market_dt[.mine_year == yr]
      year_dt[, .mine_year := NULL]
      if (nrow(year_dt) < min_year_rows) {
        next
      }
      buy_hold_total_return <- .mine_buy_hold_total_return(year_dt)

      for (param_i in seq_len(nrow(candidate_params))) {
        params <- as.list(candidate_params[param_i, ..param_cols])
        bt <- .mine_run_strategy_backtest(
          market_dt = year_dt,
          strategy_fun = strategy_fun,
          strategy_params = params,
          strat_id = strat_id,
          asset_id = asset_id,
          ctr_size = ctr_size,
          ctr_step = ctr_step,
          lev = lev,
          fee_rt = fee_rt,
          fund_rt = fund_rt,
          tol_pos = tol_pos,
          rec = rec
        )
        perf <- calc_backtest_performance(
          bt$equity,
          annualization = annualization,
          risk_free_return = risk_free_return,
          min_acceptable_return = min_acceptable_return
        )
        if (!is.finite(perf$total_return[[1L]]) ||
            !is.finite(buy_hold_total_return) ||
            perf$total_return[[1L]] <= buy_hold_total_return) {
          next
        }

        row_i <- row_i + 1L
        rows[[row_i]] <- data.table::data.table(
          asset = asset_names[[asset_i]],
          asset_index = asset_i,
          year = yr,
          candidate_params[param_i],
          perf,
          buy_hold_total_return = buy_hold_total_return,
          excess_total_return = perf$total_return[[1L]] - buy_hold_total_return
        )
        if (keep_paths) {
          rows[[row_i]][, `:=`(tgt_pos = list(bt$tgt_pos), equity = list(bt$equity))]
        }
      }
    }
  }

  asset_year_results <- if (length(rows) > 0L) {
    .mine_order_results(data.table::rbindlist(rows, fill = TRUE), score_col = score_col)
  } else {
    .mine_empty_asset_year_result(param_cols)
  }

  list(
    seed_params = seed_params,
    candidate_params = candidate_params,
    asset_year_results = asset_year_results
  )
}

#' Mine Strategy Assets
#'
#' Loops over a list of assets for one fixed strategy parameter set and
#' backtesting period, then ranks assets by Sortino ratio by default.
#'
#' @param market_data_list Named or unnamed list of candle `data.table`s.
#' @param strategy_fun Strategy target-position function. It must accept `DT` as
#'   its first argument and return a numeric target-position vector.
#' @param strategy_params Named list of fixed strategy parameters.
#' @param asset_names Optional asset labels. Defaults to names of
#'   `market_data_list`, or `asset_1`, `asset_2`, ...
#' @inheritParams mine_strategy_params
#'
#' @return A ranked `data.table` containing asset labels and performance metrics.
#' @export
mine_strategy_assets <- function(
  market_data_list,
  strategy_fun,
  strategy_params = list(),
  asset_names = NULL,
  from = NULL,
  to = NULL,
  score_col = "sortino",
  keep_paths = FALSE,
  strat_id = 0L,
  asset_id = 0L,
  ctr_size = 1.0,
  ctr_step = 1.0,
  lev = 10.0,
  fee_rt = 0.0,
  fund_rt = 0.0,
  tol_pos = 0.1,
  rec = FALSE,
  annualization = 252,
  risk_free_return = 0,
  min_acceptable_return = 0
) {
  stopifnot(is.list(market_data_list), length(market_data_list) > 0L)
  stopifnot(is.function(strategy_fun))
  stopifnot(is.list(strategy_params))
  stopifnot(is.logical(keep_paths), length(keep_paths) == 1L)

  if (is.null(asset_names)) {
    asset_names <- names(market_data_list)
    if (is.null(asset_names) || any(asset_names == "")) {
      asset_names <- paste0("asset_", seq_along(market_data_list))
    }
  }
  stopifnot(length(asset_names) == length(market_data_list))

  rows <- vector("list", length(market_data_list))
  for (i in seq_along(market_data_list)) {
    market_dt <- .mine_market_dt(market_data_list[[i]], from = from, to = to)
    bt <- .mine_run_strategy_backtest(
      market_dt = market_dt,
      strategy_fun = strategy_fun,
      strategy_params = strategy_params,
      strat_id = strat_id,
      asset_id = asset_id,
      ctr_size = ctr_size,
      ctr_step = ctr_step,
      lev = lev,
      fee_rt = fee_rt,
      fund_rt = fund_rt,
      tol_pos = tol_pos,
      rec = rec
    )
    perf <- calc_backtest_performance(
      bt$equity,
      annualization = annualization,
      risk_free_return = risk_free_return,
      min_acceptable_return = min_acceptable_return
    )
    rows[[i]] <- data.table::data.table(asset = asset_names[[i]], asset_index = i, perf)
    if (keep_paths) {
      rows[[i]][, `:=`(tgt_pos = list(bt$tgt_pos), equity = list(bt$equity))]
    }
  }

  .mine_order_results(data.table::rbindlist(rows, fill = TRUE), score_col = score_col)
}
