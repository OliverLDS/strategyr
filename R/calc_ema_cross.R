#' @export
calc_EMA_cross <- function(DT, fast = 20, slow = 50, low_atr_threshold = 5, freshness_floor = 18, tp_ratio = 0.05, sl_ratio = 0.02, debug_mode = FALSE) {
  stopifnot(fast < slow)
  stopifnot(data.table::is.data.table(DT))
  fast_col <- sprintf("ema_%d", fast)
  slow_col <- sprintf("ema_%d", slow)
  low_atr_col <- sprintf("low_atr_%d", low_atr_threshold)
  stopifnot(all(c(fast_col, slow_col, 'close', low_atr_col) %in% names(DT)))
  
  fast_value <- DT[[fast_col]] |> data.table::nafill(fill = 0.0)
  slow_value <- DT[[slow_col]] |> data.table::nafill(fill = 0.0)
  close_value <- DT[['close']]
  low_atr_value <- DT[[low_atr_col]]
  low_atr_value <- ifelse(is.na(low_atr_value), FALSE, low_atr_value)

  res <- twoline_cross(fast_value, slow_value, close_value, low_atr_value)
  if (debug_mode) {
    feat1_col <- sprintf("cross_type_%d_%d", fast, slow)
    feat2_col <- sprintf("cross_age_%d_%d", fast, slow)
    feat3_col <- sprintf("max_ir_%d_%d", fast, slow)
    feat4_col <- sprintf("min_ir_%d_%d", fast, slow)
    feat5_col <- sprintf("atr_ok_on_age_%d_%d", fast, slow)
    feat6_col <- sprintf("atr_ok_off_age_%d_%d", fast, slow)
    feat7_col <- sprintf("ema_cross_is_fresh_%d_%d_%d", fast, slow, freshness_floor)
    data.table::set(DT, j = feat1_col, value = res$cross_type)
    data.table::set(DT, j = feat2_col, value = res$cross_age)
    data.table::set(DT, j = feat3_col, value = res$max_ir)
    data.table::set(DT, j = feat4_col, value = res$min_ir)
    data.table::set(DT, j = feat5_col, value = res$atr_ok_on_age)
    data.table::set(DT, j = feat6_col, value = res$atr_ok_off_age)
    data.table::set(DT, j = feat7_col, value = res$cross_age <= freshness_floor)
  } else {
    tgt_pos_col <- sprintf("tgt_pos_%d_%d_%d", fast, slow, low_atr_threshold)
    pos_ok <- (res$cross_age <= freshness_floor) & low_atr_value & (res$max_ir < tp_ratio) & (res$min_ir > - sl_ratio)
    tgt_pos <- ifelse(pos_ok, res$cross_type, 0.0)
    data.table::set(DT, j = tgt_pos_col, value = tgt_pos)
  }
  
  invisible(DT)
}