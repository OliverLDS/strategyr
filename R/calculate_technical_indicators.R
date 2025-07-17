#' Calculate Fixed-Window Quantile Support/Resistance Zones
#'
#' Uses rolling quantiles over a fixed window to estimate smooth, spike-resistant
#' support and resistance zones from high and low prices.
#'
#' @param high Numeric vector of high prices.
#' @param low Numeric vector of low prices.
#' @param window Integer. Size of the rolling window. Default is 20.
#'
#' @return A tibble with four columns:
#' \describe{
#'   \item{support_strong_qt_f}{Numeric. 10\% quantile of lows.}
#'   \item{support_weak_qt_f}{Numeric. 20\% quantile of lows.}
#'   \item{resistance_weak_qt_f}{Numeric. 80\% quantile of highs.}
#'   \item{resistance_strong_qt_f}{Numeric. 90\% quantile of highs.}
#' }
.calc_zones_quantile_fixed <- function(high, low, window = 20) {
  stopifnot(length(high) == length(low))

  n <- length(high)
  sup_str <- sup_weak <- res_weak <- res_str <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    # Skip until 'window' bars are available
    if (i < window) next

    hi_win <- high[(i - window + 1):i]
    lo_win <- low[(i - window + 1):i]

    # --- quantiles give smooth, spike-resistant levels -------------------
    res_str[i] <- quantile(hi_win, 0.90, na.rm = TRUE)  # strong resistance
    res_weak[i] <- quantile(hi_win, 0.80, na.rm = TRUE) # weaker resistance
    sup_str[i]  <- quantile(lo_win, 0.10, na.rm = TRUE) # strong support
    sup_weak[i] <- quantile(lo_win, 0.20, na.rm = TRUE) # weaker support
  }

  tibble::tibble(
    support_strong_qt_f  = sup_str,
    support_weak_qt_f    = sup_weak,
    resistance_weak_qt_f = res_weak,
    resistance_strong_qt_f = res_str
  )
}

#' Calculate Dynamic Quantile Zones in Quiet Regimes
#'
#' Estimates support and resistance zones using quantiles over variable-length
#' windows during low-volatility periods. Ideal for range-bound conditions.
#'
#' @param high Numeric vector of high prices.
#' @param low Numeric vector of low prices.
#' @param close Numeric vector of close prices.
#' @param atr14 Numeric vector of ATR(14) values.
#' @param mom_threshold Maximum ATR/price ratio to define low-momentum regime.
#' @param min_window Minimum number of bars required to calculate zones.
#' @param max_window Maximum window size to look back from current bar.
#'
#' @return A tibble with four columns:
#' \describe{
#'   \item{support_strong_qt_d}{Numeric. 10\% quantile of lows during quiet periods.}
#'   \item{support_weak_qt_d}{Numeric. 20\% quantile of lows.}
#'   \item{resistance_weak_qt_d}{Numeric. 80\% quantile of highs.}
#'   \item{resistance_strong_qt_d}{Numeric. 90\% quantile of highs.}
#' }
.calc_zones_quantile_dyn <- function(high, low, close, atr14,
                                    mom_threshold = 0.015,
                                    min_window    = 20,
                                    max_window    = 120) {

  stopifnot(length(high) == length(low),
            length(low)  == length(close),
            length(close) == length(atr14))

  n <- length(high)

  # Pre-allocate
  sup_str <- sup_weak <- res_weak <- res_str <- rep(NA_real_, n)

  win_start <- NA_integer_   # first index of current quiet regime

  for (i in seq_len(n)) {

    # ---- 1. is momentum weak? ------------------------------------------
    weak_now <- !is.na(atr14[i]) && !is.na(close[i]) &&
                (atr14[i] / close[i]) < mom_threshold

    if (weak_now) {
      if (is.na(win_start)) win_start <- i            # start new window

      left <- max(win_start, i - max_window + 1)      # dynamic window
      wlen <- i - left + 1

      if (wlen >= min_window) {
        hi_win <- high[left:i]
        lo_win <- low [left:i]

        res_str[i] <- quantile(hi_win, 0.90, na.rm = TRUE)
        res_weak[i] <- quantile(hi_win, 0.80, na.rm = TRUE)
        sup_str[i]  <- quantile(lo_win, 0.10, na.rm = TRUE)
        sup_weak[i] <- quantile(lo_win, 0.20, na.rm = TRUE)
      }
      # else: still building window → leave NA
    } else {
      win_start <- NA_integer_                        # reset window

      # ---- 2. carry previous value if it exists -------------------------
      if (i > 1) {
        sup_str[i]  <- sup_str [i - 1]
        sup_weak[i] <- sup_weak[i - 1]
        res_weak[i] <- res_weak[i - 1]
        res_str[i]  <- res_str [i - 1]
      }
      # if i == 1 we just keep the pre-filled NA
    }
  }

  tibble::tibble(
    support_strong_qt_d     = sup_str,
    support_weak_qt_d       = sup_weak,
    resistance_weak_qt_d    = res_weak,
    resistance_strong_qt_d  = res_str
  )
}

#' Identify Pivot Highs and Lows
#'
#' Flags swing highs and lows based on a local window (span) around each candle.
#'
#' @param high Numeric vector of high prices.
#' @param low Numeric vector of low prices.
#' @param span Integer. Number of bars to the left and right to compare against.
#'
#' @return A list with two logical vectors:
#' \describe{
#'   \item{is_piv_hi}{TRUE where a pivot high is detected.}
#'   \item{is_piv_lo}{TRUE where a pivot low is detected.}
#' }
.pivot_flags <- function(high, low, span = 2) {
  # swing high: current high > highs on both sides within 'span'
  # swing low : current low  <  lows on both sides within 'span'
  n  <- length(high)
  hi <- lo <- rep(FALSE, n)

  for (i in (span + 1):(n - span)) {
    hi[i] <- all(high[i] >  high[(i - span):(i + span)][- (span + 1)])
    lo[i] <- all(low[i]  <  low[(i - span):(i + span)][- (span + 1)])
  }
  list(is_piv_hi = hi, is_piv_lo = lo)
}

#' Build Support/Resistance Zones from Pivot Points
#'
#' Collapses pivot highs/lows into tolerant zones by scoring nearby clusters.
#'
#' @param df Data frame containing columns: high, low, ATR column, is_piv_hi, is_piv_lo.
#' @param k Number of most recent pivots to consider.
#' @param tol_mult Tolerance multiplier applied to ATR to merge close pivots.
#' @param atr_col Name of ATR column in df. Default is "atr_14".
#'
#' @return A tibble with two support and two resistance zones and scores:
#' \describe{
#'   \item{support_1, support_2}{Primary and secondary support levels.}
#'   \item{resistance_1, resistance_2}{Primary and secondary resistance levels.}
#'   \item{support_score_1, support_score_2}{Count of pivot hits near the support.}
#'   \item{resistance_score_1, resistance_score_2}{Count of pivot hits near resistance.}
#' }
.build_zones_from_pivots <- function(df, k = 6, tol_mult = 0.15,
                                    atr_col = "atr_14") {
  # df must contain: high, low, <atr_col>, is_piv_hi, is_piv_lo
  n <- nrow(df)

  # Pre-allocate result vectors
  sup1 <- sup2 <- res1 <- res2 <- rep(NA_real_, n)
  sup1_sc <- sup2_sc <- res1_sc <- res2_sc <- rep(NA_integer_, n)

  for (i in seq_len(n)) {

    # --- pull pivots *up to* bar i ---------------------------------------
    piv_hi <- df$high[df$is_piv_hi & seq_len(n) <= i]
    piv_lo <- df$low [df$is_piv_lo & seq_len(n) <= i]

    # --------------------------------------------------------------------
    # Resistance side
    # --------------------------------------------------------------------
    if (length(piv_hi)) {
      atr  <- df[[atr_col]][i]
      tol  <- ifelse(is.na(atr), 0, atr * tol_mult)

      # Take last k pivots, sort desc (recent highest first)
      hi_raw <- sort(tail(piv_hi, k), decreasing = TRUE)

      # Collapse nearby pivots into single level using tolerance 'tol'
      hi_lvls <- Reduce(function(acc, x) {
        if (!length(acc) || abs(x - acc[length(acc)]) > tol)
          acc <- c(acc, x)
        acc
      }, hi_raw, numeric(0))

      # Score: how many pivots sit near each level
      count_hits <- function(lvl) sum(abs(piv_hi - lvl) <= tol)
      scores <- vapply(hi_lvls, count_hits, integer(1))

      res1[i]     <- hi_lvls[1]
      res1_sc[i]  <- scores[1]
      res2[i]     <- if (length(hi_lvls) > 1) hi_lvls[2] else hi_lvls[1]
      res2_sc[i]  <- if (length(scores)   > 1) scores[2]  else scores[1]
    }

    # --------------------------------------------------------------------
    # Support side – symmetric to above
    # --------------------------------------------------------------------
    if (length(piv_lo)) {
      atr  <- df[[atr_col]][i]
      tol  <- ifelse(is.na(atr), 0, atr * tol_mult)

      lo_raw <- sort(tail(piv_lo, k))               # ascending
      lo_lvls <- Reduce(function(acc, x) {
        if (!length(acc) || abs(x - acc[length(acc)]) > tol)
          acc <- c(acc, x)
        acc
      }, lo_raw, numeric(0))

      count_hits <- function(lvl) sum(abs(piv_lo - lvl) <= tol)
      scores <- vapply(lo_lvls, count_hits, integer(1))

      sup1[i]     <- lo_lvls[1]
      sup1_sc[i]  <- scores[1]
      sup2[i]     <- if (length(lo_lvls) > 1) lo_lvls[2] else lo_lvls[1]
      sup2_sc[i]  <- if (length(scores)   > 1) scores[2]  else scores[1]
    }
  }

  tibble::tibble(
    support_1         = sup1,   support_2         = sup2,
    resistance_1      = res1,   resistance_2      = res2,
    support_score_1   = sup1_sc, support_score_2  = sup2_sc,
    resistance_score_1 = res1_sc, resistance_score_2 = res2_sc
  )
}

#' Calculate Pivot-Based Support/Resistance Zones
#'
#' Combines pivot detection with scoring logic to estimate key levels.
#'
#' @param df Data frame with high, low, and ATR columns.
#' @param span Integer. Span used for identifying pivot points.
#' @param k Number of pivots to use for level clustering.
#' @param tol_mult Tolerance multiplier for grouping levels.
#' @param atr_col Name of the ATR column. Default is "atr_14".
#'
#' @return A tibble with key pivot zone levels and scores.
.calc_zones_pivots <- function(df, span = 2, k = 6, tol_mult = 0.15,
                              atr_col = "atr_14") {
  # df must contain 'high', 'low', and ATR column
  flags <- .pivot_flags(df$high, df$low, span = span)
  df <- dplyr::mutate(df,
                      is_piv_hi = flags$is_piv_hi,
                      is_piv_lo = flags$is_piv_lo)
  .build_zones_from_pivots(df, k = k, tol_mult = tol_mult,
                          atr_col = atr_col)
}

#' Add ARIMAX Fan Forecast to Return Series
#'
#' Fits an ARIMA model with exogenous volume to predict 1-step-ahead return mean
#' and quantiles. Produces fan-style probabilistic forecast for returns.
#'
#' @param df Data frame containing `ret_4h_raw` and `vol_lag`.
#' @param lookback Window size for ARIMA fitting.
#' @param arima_order Order of ARIMA model. Default is c(1, 0, 0).
#' @param probs Numeric vector of quantile levels. Default: c(0.10, 0.25, 0.75, 0.90).
#'
#' @return A tibble with predicted mean and quantiles for each bar.
.add_arimax_fan <- function(df, lookback = 200,
                           arima_order = c(1, 0, 0),
                           probs = c(0.10, 0.25, 0.75, 0.90)) {

  n <- nrow(df)
  out_mat <- matrix(NA_real_, n, 1 + length(probs))   # mean + four quantiles
  colnames(out_mat) <- c("fwd_mean",
                         paste0("fwd_p", probs * 100))

  for (i in (lookback + 1):(n - 1)) {                 # -1 because we need vol for newxreg
    win_idx   <- (i - lookback):(i - 1)              # past window only
    y_train   <- df$ret_4h_raw[win_idx]
    x_train   <- df$vol_lag [win_idx]                # exogenous volume
    x_next    <- df$vol_lag [i]                      # volume known at time i

    # skip if any NA in window
    if (anyNA(c(y_train, x_train, x_next))) next
    
    mod <- tryCatch({
      stats::arima(y_train, order = arima_order, xreg = x_train)
    }, error = function(e) return(NULL))
    
    if (is.null(mod)) next

    fc  <- predict(mod, n.ahead = 1, newxreg = x_next)
    mu  <- fc$pred[1]
    sd_ <- sqrt(fc$se[1]^2)

    q   <- qnorm(probs, mean = mu, sd = sd_)         # quantiles under N(µ, σ²)

    out_mat[i + 1, ] <- c(mu, q)                     # store at *t+1* row
  }

  tibble::as_tibble(out_mat)
}

#' Right Shift a Vector by One (Lag)
#'
#' Returns the input vector with a leading NA, dropping the last value.
#'
#' @param vector Numeric or logical vector.
#'
#' @return Vector of the same length, shifted right by one position.
.lag <- function(vector) c(NA, vector)[1:length(vector)]

#' Calculate Technical Indicators and Zone Levels
#'
#' Computes standard TA indicators (MA, EMA, RSI, ATR, Bollinger Bands, trend,
#' squeeze, momentum), and optionally zone levels and ARIMA forecasts.
#'
#' @param df Data frame with OHLCV columns: open, high, low, close, volume.
#' @param if_calculate_pivot_zone Logical. If TRUE, compute pivot and quantile zones.
#' @param if_calculate_arima Logical. If TRUE, compute ARIMA fan forecast on return.
#'
#' @return A data frame with added indicator and signal columns.
#' @export
calculate_technical_indicators <- function(df, if_calculate_pivote_zone = FALSE, if_calculate_arima = FALSE) {
  
  close_vector <- df$close
  df$rsi_14 <- TTR::RSI(close_vector, 14)
  df$sma_5 <- TTR::SMA(close_vector, 5)
  df$sma_20 <- TTR::SMA(close_vector, 20)
  df$sma_50 <- TTR::SMA(close_vector, 50)
  df$ema_5 <- TTR::EMA(close_vector, 5)
  df$ema_20 <- TTR::EMA(close_vector, 20)
  df$ema_50 <- TTR::EMA(close_vector, 50)
  
  atr_df <- as.data.frame(TTR::ATR(df[, c("high", "low", "close")], n = 14))
  bb_df <- as.data.frame(TTR::BBands(close_vector, n = 20))
  
  df$atr_14 <- atr_df$atr
  df$atr_ma_20 <- TTR::SMA(atr_df$atr, n = 20)
  df$bb_upper <- bb_df$up
  df$bb_lower <- bb_df$dn
  df$bb_width <- bb_df$up - bb_df$dn
  
  # 1) Trend
  df$trend_fast_up <- df$ema_5 > df$ema_20
  df$trend_med_up <- df$sma_20 > df$sma_50
  df$slope_ema20 <- df$ema_20 - .lag(df$ema_20)
  
  # 2) Volatility
  df$atr_pct <- df$atr_14 / df$close
  df$atr_ratio <- df$atr_14 / df$atr_ma_20
  df$bb_pct <- df$bb_width / df$close
  df$atr_rising <- df$atr_14 > df$atr_ma_20
  
  # 3) Price location
  df$bb_pcent <- (df$close - df$bb_lower) / df$bb_width
  df$z_sma20 <- (df$close - df$sma_20) / df$atr_14
  df$high_max_25   = slider::slide_dbl(df$high, ~ max(.x, na.rm = TRUE), .before = 24, .complete = TRUE)
  df$low_min_25    = slider::slide_dbl(df$low, ~ min(.x, na.rm = TRUE), .before = 24, .complete = TRUE)
  
  # 4) Momentum
  df$rsi_overbought <- df$rsi_14 > 70
  df$rsi_oversold <- df$rsi_14 < 30
  df$rsi_delta <- df$rsi_14 - .lag(df$rsi_14)
  
  # 5) Squeeze flag
  df$squeeze <- df$bb_pct < quantile(df$bb_pct, 0.15, na.rm = TRUE)
  
  # 6) Dynamic risk parameters
  df$sl_dist <- 1.5 * df$atr_14
  df$tp_dist <- 3   * df$atr_14
  
  #---- calculate pivote zones----
  if(if_calculate_pivote_zone) {
    zones_a <- .calc_zones_quantile_fixed(df$high, df$low, window = 20)
    zones_b <- .calc_zones_quantile_dyn(
      high   = df$high,
      low    = df$low,
      close  = df$close,
      atr14  = df$atr_14,      # must exist
      mom_threshold = 0.015,      # tweak per instrument
      min_window    = 20,
      max_window    = 120
    )
    zones_c <- .calc_zones_pivots(df, span = 2, k = 6, tol_mult = 0.15)
    
    df <- dplyr::bind_cols(df, zones_a)
    df <- dplyr::bind_cols(df, zones_b)
    df <- dplyr::bind_cols(df, zones_c)
  }
  
  #---- calculate arima forecast ----
  if(if_calculate_arima) {
    df$log_close <- log(df$close)
    df$ret_raw <- c(NA, diff(df$log_close))
    df$vol_lag <- .lag(df$volume)
    
    fan_df <- .add_arimax_fan(df, lookback = 200)
    df  <- dplyr::bind_cols(df, fan_df)
  }
  
  df
} 

 



