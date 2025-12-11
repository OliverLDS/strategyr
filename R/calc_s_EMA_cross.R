#' @export
calc_EMA_cross <- function(DT, fast = 20, slow = 50) {
  stopifnot(fast < slow)
  stopifnot(data.table::is.data.table(DT))
  fast_col <- sprintf("ema_%d", fast)
  slow_col <- sprintf("ema_%d", slow)
  stopifnot(all(c(fast_col, slow_col) %in% names(DT)))

  feat1_col  <- sprintf("ema_cross_type_%d_%d", fast, slow)
  feat2_col <- sprintf("ema_cross_age_%d_%d", fast, slow)
  res <- twoline_cross(DT[[fast_col]], DT[[slow_col]])
  data.table::set(DT, j = feat1_col, value = res$cycle_type)
  data.table::set(DT, j = feat2_col, value = res$bsc)

  invisible(DT)
}

#' @export
calc_s_EMA_cross <- function(DT, fast = 20, slow = 50, f_window = 42L, h = 300) {
  stopifnot(fast < slow)
  stopifnot(data.table::is.data.table(DT))
  feat1_col  <- sprintf("ema_cross_type_%d_%d", fast, slow)
  feat2_col <- sprintf("ema_cross_age_%d_%d", fast, slow)
  stopifnot(all(c(feat1_col, feat2_col, 'open', 'close', 'atr_logr_12', 'atr_logr_up_12', 'atr_logr_down_12') %in% names(DT))) # temporarily hard code 12 here
  
  f_logr <- log(data.table::shift(DT$close, f_window, type = 'lead')/data.table::shift(DT$open, 1L, type = 'lead'))
  f_atr <- data.table::shift(DT$atr_logr_12, f_window, type = 'lead') - DT$atr_logr_12
	f_atru <- data.table::shift(DT$atr_logr_up_12, f_window, type = 'lead') - DT$atr_logr_up_12
	f_atrd <- data.table::shift(DT$atr_logr_down_12, f_window, type = 'lead') - DT$atr_logr_down_12
  ema_cross_id <- as.integer(10000*DT[[feat1_col]] + DT[[feat2_col]])
  s_logr_dt <- event_score(ema_cross_id, f_logr, h = h, coef = 1.0)
  s_atr_dt <- event_score(ema_cross_id, f_atr, h = h, coef = 1.0)
  s_atru_dt <- event_score(ema_cross_id, f_atru, h = h, coef = 1.0)
  s_atrd_dt <- event_score(ema_cross_id, f_atrd, h = h, coef = 1.0)
  DT[, `:=`(
    s_ema_cross_ret = s_logr_dt$score, 
    se_ema_cross_ret = s_logr_dt$error,
    s_ema_cross_atr = s_atr_dt$score,
    se_ema_cross_atr = s_atr_dt$error,
    s_ema_cross_atru = s_atru_dt$score,
    se_ema_cross_atru = s_atru_dt$error,
    s_ema_cross_atrd = s_atrd_dt$score,
    se_ema_cross_atrd = s_atrd_dt$error
  )]
  
  invisible(DT)
}