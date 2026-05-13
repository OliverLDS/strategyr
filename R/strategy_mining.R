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

.sum_optional_cost <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  stopifnot(is.numeric(x))
  sum(x[is.finite(x)], na.rm = TRUE)
}

#' Standard Backtest Performance Summary
#'
#' Computes a broader one-row performance summary from a path-dependent
#' backtest equity curve and, optionally, the strategy target-position path.
#' This is intended for strategy comparison reports and examples where Sortino
#' alone is too narrow.
#'
#' @param equity Numeric equity curve.
#' @param tgt_pos Optional numeric target-position path aligned with `equity`.
#' @param annualization Numeric periods per year.
#' @param risk_free_return Numeric annual risk-free return used for excess
#'   return.
#' @param min_acceptable_return Numeric annual return threshold used for
#'   downside deviation.
#' @param fee_paid Optional numeric fee-cost vector or scalar.
#' @param funding_paid Optional numeric funding-cost vector or scalar.
#' @param leverage Optional numeric leverage or exposure path aligned with
#'   `equity`.
#' @param recorder Optional recorder list from `attr(backtest_result,
#'   "recorder")`. When supplied with `fee_rt`, trade fees are estimated from
#'   recorded trade quantity and price.
#' @param fee_rt Optional fee rate used with `recorder` when `fee_paid` is not
#'   supplied.
#' @param recorder_contract_size Contract-size multiplier used when estimating
#'   fees from recorder quantity and price.
#' @param turnover_tol Numeric tolerance below which target-position changes are
#'   ignored for turnover and trade counts.
#'
#' @return A one-row `data.table` with return, risk, drawdown, exposure,
#'   turnover, trade-count, and optional cost metrics.
#' @export
calc_strategy_performance_summary <- function(
  equity,
  tgt_pos = NULL,
  annualization = 252,
  risk_free_return = 0,
  min_acceptable_return = 0,
  fee_paid = NULL,
  funding_paid = NULL,
  leverage = NULL,
  recorder = attr(equity, "recorder", exact = TRUE),
  fee_rt = NULL,
  recorder_contract_size = 1.0,
  turnover_tol = 0
) {
  if (is.data.frame(equity) || data.table::is.data.table(equity)) {
    equity_dt <- data.table::as.data.table(equity)
    if (!"equity" %in% names(equity_dt)) {
      stop("When `equity` is a table it must contain an `equity` column.", call. = FALSE)
    }
    if (is.null(fee_paid) && "fee_paid" %in% names(equity_dt)) {
      fee_paid <- equity_dt$fee_paid
    }
    if (is.null(funding_paid) && "funding_paid" %in% names(equity_dt)) {
      funding_paid <- equity_dt$funding_paid
    }
    if (is.null(leverage) && "gross_exposure" %in% names(equity_dt)) {
      leverage <- equity_dt$gross_exposure
    }
    equity <- equity_dt$equity
  }

  stopifnot(is.numeric(equity))
  stopifnot(length(annualization) == 1L, is.finite(annualization), annualization > 0)
  stopifnot(length(risk_free_return) == 1L, is.finite(risk_free_return))
  stopifnot(length(min_acceptable_return) == 1L, is.finite(min_acceptable_return))
  stopifnot(length(turnover_tol) == 1L, is.finite(turnover_tol), turnover_tol >= 0)
  stopifnot(length(recorder_contract_size) == 1L, is.finite(recorder_contract_size), recorder_contract_size > 0)

  compact <- calc_backtest_performance(
    equity,
    annualization = annualization,
    risk_free_return = risk_free_return,
    min_acceptable_return = min_acceptable_return
  )

  n_obs <- length(equity)
  ret <- if (n_obs >= 2L && all(is.finite(equity)) && all(equity > 0)) diff(log(equity)) else numeric()
  excess_ret <- ret - risk_free_return / annualization
  volatility <- compact$volatility[[1L]]
  mean_excess <- if (length(excess_ret)) mean(excess_ret, na.rm = TRUE) * annualization else NA_real_
  sharpe <- if (is.finite(volatility) && volatility > 0) {
    mean_excess / volatility
  } else if (is.finite(mean_excess) && mean_excess > 0) {
    Inf
  } else if (is.finite(mean_excess) && mean_excess < 0) {
    -Inf
  } else {
    NA_real_
  }

  hit_rate <- if (length(ret)) mean(ret > 0, na.rm = TRUE) else NA_real_
  avg_return <- if (length(ret)) mean(ret, na.rm = TRUE) else NA_real_
  best_return <- if (length(ret)) max(ret, na.rm = TRUE) else NA_real_
  worst_return <- if (length(ret)) min(ret, na.rm = TRUE) else NA_real_
  calmar <- if (is.finite(compact$max_drawdown[[1L]]) && compact$max_drawdown[[1L]] > 0) {
    compact$annual_return[[1L]] / compact$max_drawdown[[1L]]
  } else {
    NA_real_
  }

  if (is.null(fee_paid) && !is.null(recorder) && !is.null(fee_rt)) {
    if (is.list(recorder) && all(c("ctr_qty", "price") %in% names(recorder))) {
      fee_paid <- abs(recorder$ctr_qty * recorder$price * recorder_contract_size) * fee_rt
    }
  }

  exposure <- avg_abs_exposure <- max_abs_exposure <- turnover <- avg_turnover <- trade_count <- NA_real_
  if (!is.null(tgt_pos)) {
    stopifnot(is.numeric(tgt_pos), length(tgt_pos) == n_obs)
    pos <- data.table::fifelse(is.na(tgt_pos), 0, tgt_pos)
    abs_pos <- abs(pos)
    exposure <- mean(abs_pos > turnover_tol, na.rm = TRUE)
    avg_abs_exposure <- mean(abs_pos, na.rm = TRUE)
    max_abs_exposure <- max(abs_pos, na.rm = TRUE)
    pos_change <- if (length(pos) >= 2L) abs(diff(pos)) else numeric()
    pos_change[pos_change <= turnover_tol] <- 0
    turnover <- sum(pos_change, na.rm = TRUE)
    avg_turnover <- if (length(pos_change)) mean(pos_change, na.rm = TRUE) else 0
    trade_count <- sum(pos_change > 0, na.rm = TRUE)
  }

  avg_leverage <- max_leverage <- NA_real_
  if (!is.null(leverage)) {
    stopifnot(is.numeric(leverage))
    lev <- abs(leverage[is.finite(leverage)])
    avg_leverage <- if (length(lev)) mean(lev) else NA_real_
    max_leverage <- if (length(lev)) max(lev) else NA_real_
  } else if (!is.na(avg_abs_exposure)) {
    avg_leverage <- avg_abs_exposure
    max_leverage <- max_abs_exposure
  }

  data.table::data.table(
    compact,
    sharpe = sharpe,
    calmar = calmar,
    hit_rate = hit_rate,
    avg_return = avg_return,
    best_return = best_return,
    worst_return = worst_return,
    exposure = exposure,
    avg_abs_exposure = avg_abs_exposure,
    max_abs_exposure = max_abs_exposure,
    avg_leverage = avg_leverage,
    max_leverage = max_leverage,
    turnover = turnover,
    avg_turnover = avg_turnover,
    trade_count = trade_count,
    fee_paid = .sum_optional_cost(fee_paid),
    funding_paid = .sum_optional_cost(funding_paid),
    total_cost = sum(c(.sum_optional_cost(fee_paid), .sum_optional_cost(funding_paid)), na.rm = TRUE)
  )
}

.strategy_compare_spec <- function(spec, name, index) {
  if (is.function(spec)) {
    return(list(
      label = name,
      fun = spec,
      params = list(),
      strat_id = as.integer(index),
      asset_id = 0L
    ))
  }
  if (!is.list(spec) || is.null(spec$fun) || !is.function(spec$fun)) {
    stop("Each strategy spec must be a function or a list with a strategy function in `fun`.", call. = FALSE)
  }
  list(
    label = if (!is.null(spec$label)) as.character(spec$label) else name,
    fun = spec$fun,
    params = if (!is.null(spec$params)) spec$params else list(),
    strat_id = if (!is.null(spec$strat_id)) as.integer(spec$strat_id) else as.integer(index),
    asset_id = if (!is.null(spec$asset_id)) as.integer(spec$asset_id) else 0L
  )
}

#' Rank Strategy Results
#'
#' Orders a strategy result table by a selected score column and adds a `rank`
#' column. This helper is shared by comparison workflows and can also be used on
#' custom result tables.
#'
#' @param result_dt A `data.table` or `data.frame` containing strategy metrics.
#' @param score_col Column used for ranking.
#' @param decreasing Logical; when `TRUE`, larger scores rank better.
#'
#' @return A ranked `data.table`.
#' @export
rank_strategy_results <- function(result_dt, score_col = "sortino", decreasing = TRUE) {
  stopifnot(data.table::is.data.table(result_dt) || is.data.frame(result_dt))
  stopifnot(length(score_col) == 1L, is.character(score_col))
  stopifnot(is.logical(decreasing), length(decreasing) == 1L)

  out <- data.table::as.data.table(data.table::copy(result_dt))
  if (!score_col %in% names(out)) {
    stop("`score_col` is not present in `result_dt`.", call. = FALSE)
  }
  data.table::setorderv(out, cols = score_col, order = if (decreasing) -1L else 1L, na.last = TRUE)
  out[, rank := seq_len(.N)]
  data.table::setcolorder(out, c("rank", setdiff(names(out), "rank")))
  out
}

.mine_result_table <- function(x, table = c("auto", "seed_params", "candidate_params", "asset_year_results", "train_results", "test_results")) {
  table <- match.arg(table)
  if (data.table::is.data.table(x) || is.data.frame(x)) {
    return(data.table::as.data.table(data.table::copy(x)))
  }
  if (!is.list(x)) {
    stop("`x` must be a mining result table or a mining result list.", call. = FALSE)
  }

  table_names <- c("test_results", "asset_year_results", "seed_params", "train_results", "candidate_params")
  if (table != "auto") {
    table_names <- table
  }
  for (nm in table_names) {
    if (!is.null(x[[nm]]) && (data.table::is.data.table(x[[nm]]) || is.data.frame(x[[nm]]))) {
      return(data.table::as.data.table(data.table::copy(x[[nm]])))
    }
  }
  stop("No mining result table was found in `x`.", call. = FALSE)
}

.mine_metric_cols <- c(
  "rank", "param_id", "strategy", "strategy_index", "strat_id", "asset_id",
  "asset", "asset_index", "year", "window_id", "train_rank", "train_score",
  "train_total_return", "train_max_drawdown", "seed_asset", "seed_asset_index",
  "seed_rank", "n_obs", "total_return", "annual_return", "volatility",
  "downside_deviation", "sortino", "sharpe", "calmar", "hit_rate",
  "avg_return", "best_return", "worst_return", "max_drawdown", "exposure",
  "avg_abs_exposure", "max_abs_exposure", "turnover", "avg_turnover",
  "trade_count", "fee_paid", "funding_paid", "total_cost",
  "buy_hold_total_return", "excess_total_return", "signal_start",
  "trade_start", "trade_end", "train_start", "train_end", "test_start",
  "test_end", "warmup_n_obs", "warmup_requested_days",
  "warmup_insufficient", "tgt_pos", "equity"
)

.mine_infer_param_cols <- function(result_dt, param_cols = NULL) {
  if (!is.null(param_cols)) {
    stopifnot(is.character(param_cols), length(param_cols) > 0L)
    missing_cols <- setdiff(param_cols, names(result_dt))
    if (length(missing_cols) > 0L) {
      stop("`param_cols` are not present in the result table: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    return(param_cols)
  }
  setdiff(names(result_dt), .mine_metric_cols)
}

#' Select Strategy Parameter Candidates
#'
#' Extracts reusable parameter rows from a ranked mining result. This is useful
#' after parameter, asset-year, or walk-forward mining when the next step is to
#' rerun a strategy with the best discovered settings.
#'
#' @param x A mining result `data.table`, or a result list returned by
#'   `mine_strategy_asset_years()` or `mine_strategy_walk_forward()`.
#' @param n Integer number of rows to keep.
#' @param param_cols Optional character vector of parameter columns. When
#'   omitted, known metric and metadata columns are excluded.
#' @param score_col Metric used for ranking when `rank` is absent or
#'   `rerank = TRUE`.
#' @param decreasing Logical; when `TRUE`, larger scores rank better.
#' @param unique_params Logical; when `TRUE`, duplicate parameter rows are
#'   removed after ranking.
#' @param include_metrics Logical; when `TRUE`, include score and selected
#'   diagnostics beside the parameter columns.
#' @param table Which table to use when `x` is a mining result list. `"auto"`
#'   prefers out-of-sample results.
#' @param rerank Logical; when `TRUE`, rank by `score_col` before selecting.
#'
#' @return A `data.table` containing selected parameter rows.
#' @export
select_strategy_params <- function(
  x,
  n = 1L,
  param_cols = NULL,
  score_col = "sortino",
  decreasing = TRUE,
  unique_params = TRUE,
  include_metrics = FALSE,
  table = c("auto", "seed_params", "candidate_params", "asset_year_results", "train_results", "test_results"),
  rerank = FALSE
) {
  stopifnot(length(n) == 1L, is.finite(n), n >= 1L)
  stopifnot(length(score_col) == 1L, is.character(score_col))
  stopifnot(is.logical(decreasing), length(decreasing) == 1L)
  stopifnot(is.logical(unique_params), length(unique_params) == 1L)
  stopifnot(is.logical(include_metrics), length(include_metrics) == 1L)
  stopifnot(is.logical(rerank), length(rerank) == 1L)

  result_dt <- .mine_result_table(x, table = match.arg(table))
  if (nrow(result_dt) == 0L) {
    return(data.table::data.table())
  }
  cols <- .mine_infer_param_cols(result_dt, param_cols = param_cols)
  if (length(cols) == 0L) {
    stop("No parameter columns could be inferred. Supply `param_cols` explicitly.", call. = FALSE)
  }

  ranked <- if (rerank || !"rank" %in% names(result_dt)) {
    rank_strategy_results(result_dt, score_col = score_col, decreasing = decreasing)
  } else {
    data.table::copy(result_dt)
  }
  if ("rank" %in% names(ranked)) {
    data.table::setorderv(ranked, "rank", order = 1L, na.last = TRUE)
  }
  if (unique_params) {
    ranked <- unique(ranked, by = cols)
  }
  keep_n <- min(as.integer(n), nrow(ranked))
  out <- ranked[seq_len(keep_n), ..cols]

  if (include_metrics) {
    metric_cols <- intersect(
      c("rank", score_col, "total_return", "annual_return", "max_drawdown", "sharpe", "calmar", "hit_rate", "exposure", "turnover", "trade_count", "window_id", "asset", "year"),
      names(ranked)
    )
    out <- cbind(ranked[seq_len(keep_n), ..metric_cols], out)
  }

  out[]
}

#' Summarize Walk-Forward Mining Results
#'
#' Aggregates out-of-sample walk-forward test results into a compact diagnostic
#' table. The summary focuses on test-window performance, train/test score
#' decay, and warmup quality.
#'
#' @param x Result list returned by `mine_strategy_walk_forward()`, or a
#'   `data.table` containing walk-forward test results.
#' @param score_col Metric used as the main score column.
#' @param group_cols Optional grouping columns, such as parameter columns. When
#'   `NULL`, all test rows are summarized together.
#'
#' @return A `data.table` with window counts, average performance metrics,
#'   positive-window rate, average train/test score decay, and warmup counts.
#' @export
summarize_walk_forward_results <- function(x, score_col = "sortino", group_cols = NULL) {
  stopifnot(length(score_col) == 1L, is.character(score_col))
  test_dt <- .mine_result_table(x, table = "test_results")
  if (nrow(test_dt) == 0L) {
    return(data.table::data.table())
  }
  if (!score_col %in% names(test_dt)) {
    stop("`score_col` is not present in the walk-forward test results.", call. = FALSE)
  }
  if (is.null(group_cols)) {
    group_cols <- character()
  } else {
    stopifnot(is.character(group_cols))
    missing_cols <- setdiff(group_cols, names(test_dt))
    if (length(missing_cols) > 0L) {
      stop("`group_cols` are not present in the test results: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  }

  cols_needed <- c(
    group_cols, score_col, "total_return", "max_drawdown", "sharpe",
    "exposure", "turnover", "trade_count", "train_score",
    "warmup_insufficient", "window_id"
  )
  for (col in setdiff(cols_needed, names(test_dt))) {
    test_dt[, (col) := NA_real_]
  }

  summarize_one <- function(dt) {
    score <- dt[[score_col]]
    train_score <- dt$train_score
    score_decay <- train_score - score
    both_pos_inf <- is.infinite(train_score) & train_score > 0 & is.infinite(score) & score > 0
    both_neg_inf <- is.infinite(train_score) & train_score < 0 & is.infinite(score) & score < 0
    score_decay[both_pos_inf | both_neg_inf] <- 0
    data.table::data.table(
      n_windows = data.table::uniqueN(dt$window_id, na.rm = TRUE),
      n_rows = nrow(dt),
      avg_score = mean(score, na.rm = TRUE),
      median_score = stats::median(score, na.rm = TRUE),
      best_score = max(score, na.rm = TRUE),
      worst_score = min(score, na.rm = TRUE),
      avg_total_return = mean(dt$total_return, na.rm = TRUE),
      positive_return_rate = mean(dt$total_return > 0, na.rm = TRUE),
      avg_max_drawdown = mean(dt$max_drawdown, na.rm = TRUE),
      avg_sharpe = mean(dt$sharpe, na.rm = TRUE),
      avg_exposure = mean(dt$exposure, na.rm = TRUE),
      avg_turnover = mean(dt$turnover, na.rm = TRUE),
      avg_trade_count = mean(dt$trade_count, na.rm = TRUE),
      avg_train_score = mean(train_score, na.rm = TRUE),
      avg_score_decay = mean(score_decay, na.rm = TRUE),
      warmup_insufficient_count = sum(dt$warmup_insufficient %in% TRUE, na.rm = TRUE)
    )
  }

  if (length(group_cols) == 0L) {
    out <- summarize_one(test_dt)
  } else {
    out <- test_dt[, summarize_one(.SD), by = group_cols]
  }
  rank_strategy_results(out, score_col = "avg_score", decreasing = TRUE)
}

#' Filter Walk-Forward Results for Overfit Risk
#'
#' Applies simple out-of-sample robustness gates to walk-forward mining results.
#' This helper is intentionally conservative: it filters on realized test-window
#' summaries rather than on in-sample fit quality.
#'
#' @param x Result list from `mine_strategy_walk_forward()` or a walk-forward
#'   test-result table.
#' @param score_col Metric used as the main score column.
#' @param group_cols Optional parameter columns used to evaluate stability by
#'   parameter set. When `NULL`, all rows are evaluated together.
#' @param min_windows Minimum number of out-of-sample windows required.
#' @param min_positive_return_rate Minimum fraction of windows with positive
#'   total return.
#' @param min_avg_score Minimum average out-of-sample score.
#' @param max_avg_score_decay Maximum allowed average train-minus-test score
#'   decay.
#' @param max_warmup_insufficient_rate Maximum fraction of rows with
#'   insufficient warmup history.
#' @param min_avg_total_return Minimum average test-window total return.
#'
#' @return A ranked `data.table` of groups that pass the robustness filters,
#'   with filter thresholds recorded as columns.
#' @export
filter_walk_forward_results <- function(
  x,
  score_col = "sortino",
  group_cols = NULL,
  min_windows = 3L,
  min_positive_return_rate = 0.5,
  min_avg_score = -Inf,
  max_avg_score_decay = Inf,
  max_warmup_insufficient_rate = 0,
  min_avg_total_return = -Inf
) {
  stopifnot(length(min_windows) == 1L, is.finite(min_windows), min_windows >= 1L)
  stopifnot(length(min_positive_return_rate) == 1L, is.finite(min_positive_return_rate), min_positive_return_rate >= 0, min_positive_return_rate <= 1)
  stopifnot(length(min_avg_score) == 1L, is.finite(min_avg_score) || is.infinite(min_avg_score))
  stopifnot(length(max_avg_score_decay) == 1L, is.finite(max_avg_score_decay) || is.infinite(max_avg_score_decay))
  stopifnot(length(max_warmup_insufficient_rate) == 1L, is.finite(max_warmup_insufficient_rate), max_warmup_insufficient_rate >= 0, max_warmup_insufficient_rate <= 1)
  stopifnot(length(min_avg_total_return) == 1L, is.finite(min_avg_total_return) || is.infinite(min_avg_total_return))

  summary_dt <- summarize_walk_forward_results(x, score_col = score_col, group_cols = group_cols)
  if (nrow(summary_dt) == 0L) {
    return(summary_dt)
  }
  summary_dt[, warmup_insufficient_rate := data.table::fifelse(n_rows > 0, warmup_insufficient_count / n_rows, NA_real_)]
  summary_dt[, `:=`(
    min_windows_required = as.integer(min_windows),
    min_positive_return_rate_required = min_positive_return_rate,
    min_avg_score_required = min_avg_score,
    max_avg_score_decay_allowed = max_avg_score_decay,
    max_warmup_insufficient_rate_allowed = max_warmup_insufficient_rate,
    min_avg_total_return_required = min_avg_total_return
  )]
  summary_dt[
    n_windows >= min_windows &
      positive_return_rate >= min_positive_return_rate &
      avg_score >= min_avg_score &
      avg_score_decay <= max_avg_score_decay &
      warmup_insufficient_rate <= max_warmup_insufficient_rate &
      avg_total_return >= min_avg_total_return
  ][]
}

#' Compare Strategy Backtests
#'
#' Runs multiple `strat_*_tgt_pos()` functions over the same market data and
#' execution assumptions, summarizes each path, and ranks the results by a
#' selected metric.
#'
#' @param DT Candle `data.table` containing `datetime`, `open`, `high`, `low`,
#'   and `close`.
#' @param strategies Named list of strategy functions or strategy specs. A spec
#'   is a list with `fun`, optional `params`, optional `strat_id`, optional
#'   `asset_id`, and optional `label`.
#' @param from,to Optional date filters for the fixed backtesting period.
#' @param score_col Metric used for ranking.
#' @param keep_paths Logical; when `TRUE`, include list-columns with target
#'   positions and equity curves.
#' @inheritParams mine_strategy_params
#' @inheritParams calc_strategy_performance_summary
#'
#' @return A ranked `data.table` containing strategy labels, identifiers, and
#'   performance summary metrics.
#' @export
compare_strategy_backtests <- function(
  DT,
  strategies,
  from = NULL,
  to = NULL,
  score_col = "sortino",
  keep_paths = FALSE,
  ctr_size = 1.0,
  ctr_step = 1.0,
  lev = 10.0,
  fee_rt = 0.0,
  fund_rt = 0.0,
  tol_pos = 0.1,
  rec = FALSE,
  annualization = 252,
  risk_free_return = 0,
  min_acceptable_return = 0,
  turnover_tol = 0
) {
  stopifnot(is.list(strategies), length(strategies) > 0L)
  stopifnot(is.logical(keep_paths), length(keep_paths) == 1L)

  market_dt <- .mine_market_dt(DT, from = from, to = to)
  labels <- names(strategies)
  if (is.null(labels)) {
    labels <- paste0("strategy_", seq_along(strategies))
  }
  labels[labels == ""] <- paste0("strategy_", which(labels == ""))

  rows <- vector("list", length(strategies))
  for (i in seq_along(strategies)) {
    spec <- .strategy_compare_spec(strategies[[i]], name = labels[[i]], index = i)
    if (!is.list(spec$params)) {
      stop("Strategy spec `params` must be a list.", call. = FALSE)
    }
    bt <- .mine_run_strategy_backtest(
      market_dt = market_dt,
      strategy_fun = spec$fun,
      strategy_params = spec$params,
      strat_id = spec$strat_id,
      asset_id = spec$asset_id,
      ctr_size = ctr_size,
      ctr_step = ctr_step,
      lev = lev,
      fee_rt = fee_rt,
      fund_rt = fund_rt,
      tol_pos = tol_pos,
      rec = rec
    )
    summary <- calc_strategy_performance_summary(
      equity = bt$equity,
      tgt_pos = bt$tgt_pos,
      annualization = annualization,
      risk_free_return = risk_free_return,
      min_acceptable_return = min_acceptable_return,
      turnover_tol = turnover_tol
    )
    rows[[i]] <- data.table::data.table(
      strategy = spec$label,
      strategy_index = i,
      strat_id = spec$strat_id,
      asset_id = spec$asset_id,
      summary
    )
    if (keep_paths) {
      rows[[i]][, `:=`(tgt_pos = list(bt$tgt_pos), equity = list(bt$equity))]
    }
  }

  rank_strategy_results(data.table::rbindlist(rows, fill = TRUE), score_col = score_col, decreasing = TRUE)
}

.mine_add_years <- function(x, years) {
  x <- as.Date(x)
  if (abs(years - round(years)) < .Machine$double.eps^0.5) {
    return(as.Date(seq(x, by = paste(as.integer(round(years)), "years"), length.out = 2L)[[2L]]))
  }
  x + as.integer(ceiling(365.25 * years))
}

.mine_empty_walk_forward_result <- function(param_cols) {
  out <- data.table::data.table(
    rank = integer(),
    window_id = integer(),
    train_start = as.Date(character()),
    train_end = as.Date(character()),
    test_start = as.Date(character()),
    test_end = as.Date(character()),
    train_rank = integer()
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
      sharpe = numeric(),
      calmar = numeric(),
      hit_rate = numeric(),
      exposure = numeric(),
      turnover = numeric(),
      trade_count = numeric(),
      signal_start = as.POSIXct(character()),
      warmup_n_obs = integer(),
      warmup_requested_days = integer(),
      warmup_insufficient = logical()
    )
  )
}

#' Walk-Forward Strategy Mining
#'
#' Performs rolling train/test strategy mining. For each window, parameter rows
#' are ranked on the training slice, the best rows are evaluated on the
#' following test slice, and test signals are computed on a wider window that can
#' include warmup history.
#'
#' @param DT Candle `data.table` containing `datetime`, `open`, `high`, `low`,
#'   and `close`.
#' @param strategy_fun Strategy target-position function. It must accept `DT` as
#'   its first argument and return a numeric target-position vector.
#' @param param_grid Non-empty list, `data.frame`, or `data.table` of parameter
#'   values.
#' @param train_years,test_years Numeric train and test window lengths in
#'   calendar years.
#' @param step_years Numeric step between successive train windows in calendar
#'   years.
#' @param n_best Integer number of top training parameter rows tested in each
#'   out-of-sample window.
#' @param min_train_rows,min_test_rows Minimum OHLC rows required for each train
#'   or test slice.
#' @param warmup_days,warmup_years Warmup history used for test signal
#'   construction. `warmup_years`, when supplied, overrides `warmup_days`.
#' @inheritParams mine_strategy_params
#' @inheritParams calc_strategy_performance_summary
#'
#' @return A list with `windows`, `train_results`, and ranked `test_results`
#'   `data.table`s.
#' @export
mine_strategy_walk_forward <- function(
  DT,
  strategy_fun,
  param_grid,
  train_years = 3,
  test_years = 1,
  step_years = 1,
  n_best = 1L,
  from = NULL,
  to = NULL,
  min_train_rows = 200L,
  min_test_rows = 50L,
  warmup_days = 365L,
  warmup_years = NULL,
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
  min_acceptable_return = 0,
  turnover_tol = 0
) {
  stopifnot(is.function(strategy_fun))
  stopifnot(length(train_years) == 1L, is.finite(train_years), train_years > 0)
  stopifnot(length(test_years) == 1L, is.finite(test_years), test_years > 0)
  stopifnot(length(step_years) == 1L, is.finite(step_years), step_years > 0)
  stopifnot(length(n_best) == 1L, is.finite(n_best), n_best >= 1L)
  stopifnot(length(min_train_rows) == 1L, is.finite(min_train_rows), min_train_rows >= 1L)
  stopifnot(length(min_test_rows) == 1L, is.finite(min_test_rows), min_test_rows >= 1L)
  stopifnot(is.logical(keep_paths), length(keep_paths) == 1L)

  grid <- .mine_param_grid(param_grid)
  param_cols <- names(grid)
  warmup_requested_days <- .mine_warmup_days(warmup_days, warmup_years)
  signal_from <- from
  if (!is.null(from) && warmup_requested_days > 0L) {
    signal_from <- as.Date(from) - warmup_requested_days
  }

  market_dt <- .mine_market_dt(DT, from = signal_from, to = to)
  trade_dt <- data.table::copy(market_dt)
  if (!is.null(from)) {
    trade_dt <- trade_dt[datetime >= as.POSIXct(from)]
  }
  if (!is.null(to)) {
    trade_dt <- trade_dt[datetime < as.POSIXct(as.Date(to) + 1)]
  }
  if (nrow(trade_dt) < min_train_rows + min_test_rows) {
    stop("Not enough rows remain for the requested walk-forward windows.", call. = FALSE)
  }

  first_date <- as.Date(trade_dt$datetime[[1L]])
  last_date <- as.Date(trade_dt$datetime[[nrow(trade_dt)]])
  train_rows <- list()
  test_rows <- list()
  window_rows <- list()
  window_id <- 0L
  train_start <- first_date

  repeat {
    train_end_excl <- .mine_add_years(train_start, train_years)
    test_start <- train_end_excl
    test_end_excl <- .mine_add_years(test_start, test_years)
    if (test_start > last_date || test_end_excl <= test_start) {
      break
    }

    train_dt <- trade_dt[datetime >= as.POSIXct(train_start) & datetime < as.POSIXct(train_end_excl)]
    test_dt <- trade_dt[datetime >= as.POSIXct(test_start) & datetime < as.POSIXct(test_end_excl)]
    if (nrow(train_dt) >= min_train_rows && nrow(test_dt) >= min_test_rows) {
      window_id <- window_id + 1L
      window_rows[[window_id]] <- data.table::data.table(
        window_id = window_id,
        train_start = train_start,
        train_end = as.Date(train_end_excl) - 1L,
        test_start = test_start,
        test_end = as.Date(test_end_excl) - 1L,
        n_train = nrow(train_dt),
        n_test = nrow(test_dt)
      )

      train_res <- mine_strategy_params(
        DT = train_dt,
        strategy_fun = strategy_fun,
        param_grid = grid,
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
      top_n <- min(as.integer(n_best), nrow(train_res))
      train_keep <- train_res[seq_len(top_n)]
      train_keep[, `:=`(
        window_id = window_id,
        train_start = train_start,
        train_end = as.Date(train_end_excl) - 1L,
        test_start = test_start,
        test_end = as.Date(test_end_excl) - 1L,
        train_rank = rank
      )]
      train_rows[[window_id]] <- train_keep

      signal_start_target <- as.POSIXct(as.Date(test_dt$datetime[[1L]]) - warmup_requested_days)
      signal_dt <- market_dt[datetime >= signal_start_target & datetime <= test_dt$datetime[[nrow(test_dt)]]]
      warmup_n_obs <- sum(signal_dt$datetime < test_dt$datetime[[1L]])
      warmup_insufficient <- warmup_requested_days > 0L && warmup_n_obs == 0L
      signal_start <- signal_dt$datetime[[1L]]

      for (i in seq_len(top_n)) {
        params <- as.list(train_keep[i, ..param_cols])
        bt <- .mine_run_strategy_backtest_with_signal(
          signal_dt = signal_dt,
          trade_dt = test_dt,
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
        perf <- calc_strategy_performance_summary(
          equity = bt$equity,
          tgt_pos = bt$tgt_pos,
          annualization = annualization,
          risk_free_return = risk_free_return,
          min_acceptable_return = min_acceptable_return,
          turnover_tol = turnover_tol
        )
        row <- data.table::data.table(
          window_id = window_id,
          train_start = train_start,
          train_end = as.Date(train_end_excl) - 1L,
          test_start = test_start,
          test_end = as.Date(test_end_excl) - 1L,
          train_rank = train_keep$rank[[i]],
          train_score = train_keep[[score_col]][[i]],
          train_total_return = train_keep$total_return[[i]],
          train_max_drawdown = train_keep$max_drawdown[[i]],
          train_keep[i, ..param_cols],
          perf,
          signal_start = signal_start,
          warmup_n_obs = warmup_n_obs,
          warmup_requested_days = warmup_requested_days,
          warmup_insufficient = warmup_insufficient
        )
        if (keep_paths) {
          row[, `:=`(tgt_pos = list(bt$tgt_pos), equity = list(bt$equity))]
        }
        test_rows[[length(test_rows) + 1L]] <- row
      }
    }

    train_start <- .mine_add_years(train_start, step_years)
    if (train_start >= last_date) {
      break
    }
  }

  windows <- if (length(window_rows)) data.table::rbindlist(window_rows, fill = TRUE) else data.table::data.table()
  train_results <- if (length(train_rows)) data.table::rbindlist(train_rows, fill = TRUE) else data.table::data.table()
  test_results <- if (length(test_rows)) {
    rank_strategy_results(data.table::rbindlist(test_rows, fill = TRUE), score_col = score_col, decreasing = TRUE)
  } else {
    .mine_empty_walk_forward_result(param_cols)
  }

  list(
    windows = windows,
    train_results = train_results,
    test_results = test_results
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

.mine_run_strategy_backtest_with_signal <- function(
  signal_dt,
  trade_dt,
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
  DT_i <- data.table::copy(signal_dt)
  tgt_pos_signal <- do.call(strategy_fun, c(list(DT = DT_i), strategy_params))
  if (!is.numeric(tgt_pos_signal) || length(tgt_pos_signal) != nrow(DT_i)) {
    stop("Strategy function must return a numeric target-position vector matching `nrow(DT)`.", call. = FALSE)
  }

  trade_idx <- match(trade_dt$datetime, DT_i$datetime)
  if (anyNA(trade_idx)) {
    stop("Trade rows must be present in the signal data.", call. = FALSE)
  }
  tgt_pos <- tgt_pos_signal[trade_idx]

  eq <- backtest_rcpp(
    timestamp = as.numeric(trade_dt$datetime),
    open = trade_dt$open,
    high = trade_dt$high,
    low = trade_dt$low,
    close = trade_dt$close,
    tgt_pos = data.table::fifelse(is.na(tgt_pos), 0, tgt_pos),
    pos_strat = rep(as.integer(strat_id), nrow(trade_dt)),
    tol_pos = rep(tol_pos, nrow(trade_dt)),
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

.mine_warmup_days <- function(warmup_days, warmup_years) {
  if (!is.null(warmup_years)) {
    stopifnot(length(warmup_years) == 1L, is.finite(warmup_years), warmup_years >= 0)
    return(as.integer(ceiling(365.25 * warmup_years)))
  }
  stopifnot(length(warmup_days) == 1L, is.finite(warmup_days), warmup_days >= 0)
  as.integer(ceiling(warmup_days))
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
      excess_total_return = numeric(),
      signal_start = as.POSIXct(character()),
      trade_start = as.POSIXct(character()),
      trade_end = as.POSIXct(character()),
      warmup_n_obs = integer(),
      warmup_requested_days = integer(),
      warmup_insufficient = logical()
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
#' Asset-year strategy signals are computed on a wider signal window that can
#' include warmup history before the trade year. The reported performance is
#' always computed on the trade/evaluation asset-year slice only.
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
#' @param warmup_days,warmup_years Warmup history used for signal construction
#'   before each asset-year trade window. `warmup_years`, when supplied,
#'   overrides `warmup_days`.
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
  warmup_days = 365L,
  warmup_years = NULL,
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
  warmup_requested_days <- .mine_warmup_days(warmup_days, warmup_years)

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
  signal_from <- from
  if (!is.null(from) && warmup_requested_days > 0L) {
    signal_from <- as.Date(from) - warmup_requested_days
  }
  for (asset_i in seq_along(market_data_list)) {
    market_dt <- tryCatch(
      .mine_market_dt(market_data_list[[asset_i]], from = signal_from, to = to),
      error = function(e) NULL
    )
    if (is.null(market_dt) || nrow(market_dt) < min_year_rows) {
      next
    }

    trade_market_dt <- data.table::copy(market_dt)
    if (!is.null(from)) {
      trade_market_dt <- trade_market_dt[datetime >= as.POSIXct(from)]
    }
    if (nrow(trade_market_dt) < min_year_rows) {
      next
    }

    trade_market_dt[, .mine_year := as.integer(format(datetime, "%Y"))]
    eval_years <- sort(unique(trade_market_dt$.mine_year))
    if (!is.null(years)) {
      eval_years <- intersect(eval_years, as.integer(years))
    }

    for (yr in eval_years) {
      year_dt <- trade_market_dt[.mine_year == yr]
      year_dt[, .mine_year := NULL]
      if (nrow(year_dt) < min_year_rows) {
        next
      }
      trade_start <- year_dt$datetime[[1L]]
      trade_end <- year_dt$datetime[[nrow(year_dt)]]
      signal_start_target <- as.POSIXct(as.Date(trade_start) - warmup_requested_days)
      signal_dt <- market_dt[
        datetime >= signal_start_target &
          datetime <= trade_end
      ]
      warmup_n_obs <- sum(signal_dt$datetime < trade_start)
      signal_start <- signal_dt$datetime[[1L]]
      warmup_insufficient <- warmup_requested_days > 0L && warmup_n_obs == 0L
      buy_hold_total_return <- .mine_buy_hold_total_return(year_dt)

      for (param_i in seq_len(nrow(candidate_params))) {
        params <- as.list(candidate_params[param_i, ..param_cols])
        bt <- .mine_run_strategy_backtest_with_signal(
          signal_dt = signal_dt,
          trade_dt = year_dt,
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
          excess_total_return = perf$total_return[[1L]] - buy_hold_total_return,
          signal_start = signal_start,
          trade_start = trade_start,
          trade_end = trade_end,
          warmup_n_obs = warmup_n_obs,
          warmup_requested_days = warmup_requested_days,
          warmup_insufficient = warmup_insufficient
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
