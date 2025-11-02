.get_cycles_vector <- function(pivots, N = 360L) { # N’s value determines how fresh the cycles are
  stopifnot(inherits(pivots, "data.table"))
  DT <- copy(pivots)[order(idx)]
  setkey(DT, idx)

  DT[, {
    curr_idx <- .BY$idx
    lo <- curr_idx - N + 1L
    # window of pivots whose bar index falls within [lo, curr_idx]
    win <- DT[idx %between% c(lo, curr_idx)]

    if (nrow(win) == 0L) {
      list(cycle_m_start = as.POSIXct(NA), cycle_m_end = as.POSIXct(NA),
           cycle_m_bg_price = as.numeric(NA), cycle_m_ed_price = as.numeric(NA))
    } else {
      i_min <- which.min(win$price)      # first min breaks ties
      i_max <- which.max(win$price)      # first max breaks ties

      t_min <- win$datetime[i_min]
      t_max <- win$datetime[i_max]

      if (t_min <= t_max) {
        list(cycle_m_start = t_min, cycle_m_end = t_max,
             cycle_m_bg_price = win$price[i_min], cycle_m_ed_price = win$price[i_max])
      } else {
        list(cycle_m_start = t_max, cycle_m_end = t_min,
             cycle_m_bg_price = win$price[i_max], cycle_m_ed_price = win$price[i_min])
      }
    }
  }, by = .(idx)][DT, on = "idx"][, -c('idx', 'type', 'price')][!duplicated(datetime)]
  # The reason we have duplicated datetime is in some extreme situations, the same 4H bar can be both H and L pivots, which leads to duplicated cycles.
}

#' @export
gen_ind_dca_ladder <- function(DT, span = 3, latest_n = NULL, refined = TRUE, min_swing = 0.05, main_N = 360L, fresh_N = NA_integer_) {
  attr <- attributes(DT)
  pivots <- get_now_pivots(DT, span = span, latest_n = latest_n, refined = refined, min_swing = min_swing)
  
  main_cycles_dt <- .get_cycles_vector(pivots, main_N)
  setkey(main_cycles_dt, datetime)
  res <- main_cycles_dt[DT, on = "datetime", roll = TRUE]
  setcolorder(res, c(names(DT), setdiff(names(res), names(DT))))
  
  if (!is.na(fresh_N)) {
    fresh_cycles_dt <- .get_cycles_vector(pivots, fresh_N)
    setkey(fresh_cycles_dt, datetime)
    res2 <- fresh_cycles_dt[res, on = "datetime", roll = TRUE]
    setcolorder(res2, c(names(res), setdiff(names(res2), names(res))))
    return(invisible(res2))
  }
  
  data.table::setattr(res, "inst_id", attr$inst_id)
  data.table::setattr(res, "bar", attr$bar)
  
  invisible(res)
}