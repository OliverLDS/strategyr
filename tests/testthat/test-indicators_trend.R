test_that("rolling mean absolute deviation matches reference", {
  x <- c(1, 2, 5, 4, 7, 8)
  n <- 3

  expect_equal(
    strategyr:::rolling_mean_abs_dev(x, n),
    ref_rolling_mean_abs_dev(x, n)
  )
})

test_that("rolling sum matches reference", {
  x <- c(1, 2, 5, 4, 7, 8)
  n <- 3

  expect_equal(
    strategyr:::rolling_sum(x, n),
    ref_rolling_sum(x, n)
  )
})

test_that("calc_SMA matches TTR SMA", {
  x <- seq(100, 160, length.out = 60)
  DT <- data.table::data.table(close = x)

  calc_SMA(DT, ns = 20)

  expect_equal(
    DT$sma_20,
    as.numeric(TTR::SMA(x, n = 20)),
    tolerance = 1e-12
  )
})

test_that("calc_KeltnerChannels adds expected channel columns", {
  DT <- data.table::data.table(
    high = c(11, 12, 13, 14, 15, 16),
    low = c(9, 10, 11, 12, 13, 14),
    close = c(10, 11, 12, 13, 14, 15)
  )

  calc_KeltnerChannels(DT, ns = 3, ks = 2)

  expect_true(all(c("kc_mid_3", "kc_high_3_2", "kc_low_3_2") %in% names(DT)))
  expect_equal(DT$kc_mid_3, DT$ema_3)
  expect_equal(DT$kc_high_3_2, DT$ema_3 + 2 * DT$atr_3)
  expect_equal(DT$kc_low_3_2, DT$ema_3 - 2 * DT$atr_3)
})

test_that("calc_MACD adds expected columns and histogram identity", {
  DT <- data.table::data.table(close = c(10, 11, 12, 13, 12, 11, 10, 11))

  calc_MACD(DT, fast = 3, slow = 5, signal = 2)

  expect_true(all(c("macd_3_5", "macd_signal_3_5_2", "macd_hist_3_5_2") %in% names(DT)))
  expect_equal(DT$macd_3_5, DT$ema_3 - DT$ema_5)
  expect_equal(DT$macd_hist_3_5_2, DT$macd_3_5 - DT$macd_signal_3_5_2)
})

test_that("calc_MACD matches TTR on shared non-NA region", {
  x <- seq(100, 160, length.out = 60)
  DT <- data.table::data.table(close = x)

  calc_MACD(DT, fast = 12, slow = 26, signal = 9)
  macd_ref <- TTR::MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE)

  expect_equal(DT$macd_12_26, as.numeric(macd_ref[, "macd"]), tolerance = 1e-12)

  sig_ref <- as.numeric(macd_ref[, "signal"])
  idx <- which(!is.na(sig_ref))
  expect_true(length(idx) > 0)
  expect_equal(DT$macd_signal_12_26_9[idx], sig_ref[idx], tolerance = 1e-12)
  expect_equal(DT$macd_hist_12_26_9[idx], DT$macd_12_26[idx] - sig_ref[idx], tolerance = 1e-12)
})

test_that("calc_ROC matches manual reference", {
  DT <- data.table::data.table(close = c(100, 110, 121, 133.1, 146.41))

  calc_ROC(DT, ns = 2, scale = 100)

  expect_equal(DT$roc_2, c(NA, NA, 21, 21, 21), tolerance = 1e-12)
})

test_that("calc_ROC matches TTR discrete ROC", {
  x <- seq(100, 160, length.out = 40)
  DT <- data.table::data.table(close = x)

  calc_ROC(DT, ns = 5, scale = 1)

  expect_equal(
    DT$roc_5,
    as.numeric(TTR::ROC(x, n = 5, type = "discrete")),
    tolerance = 1e-12
  )
})

test_that("calc_BollingerBands adds expected band and touch columns", {
  DT <- data.table::data.table(close = c(1, 2, 3, 4, 10))

  calc_BollingerBands(DT, ns = 3, ks = 1)

  expect_true(all(c("bb_mid_3", "bb_sd_3", "bb_high_3_1", "bb_low_3_1",
                    "bb_touch_high_3_1", "bb_touch_low_3_1") %in% names(DT)))
  expect_equal(DT$bb_high_3_1, DT$bb_mid_3 + DT$bb_sd_3)
  expect_equal(DT$bb_low_3_1, DT$bb_mid_3 - DT$bb_sd_3)
  expect_equal(DT$bb_touch_high_3_1[5], 1L)
  expect_equal(DT$bb_touch_low_3_1[5], 0L)
})

test_that("calc_BollingerBands matches TTR BBands", {
  x <- seq(100, 130, length.out = 40)
  DT <- data.table::data.table(close = x)

  calc_BollingerBands(DT, ns = 20, ks = 2)
  bb_ref <- TTR::BBands(x, n = 20, sd = 2, maType = "SMA")

  expect_equal(DT$bb_mid_20, as.numeric(bb_ref[, "mavg"]), tolerance = 1e-12)
  expect_equal(DT$bb_high_20_2, as.numeric(bb_ref[, "up"]), tolerance = 1e-12)
  expect_equal(DT$bb_low_20_2, as.numeric(bb_ref[, "dn"]), tolerance = 1e-12)
})

test_that("calc_StochasticOscillator adds expected columns", {
  DT <- data.table::data.table(
    high = c(10, 12, 14, 16, 18),
    low = c(5, 6, 7, 8, 9),
    close = c(7, 11, 13, 15, 17)
  )

  calc_StochasticOscillator(DT, ns = 3, d_ns = 2)

  expect_true(all(c("stoch_k_3", "stoch_d_3_2") %in% names(DT)))
  expect_equal(DT$stoch_k_3[3], 100 * (13 - 5) / (14 - 5))
  expect_equal(DT$stoch_d_3_2, strategyr:::.apply_after_first_non_na(DT$stoch_k_3, strategyr:::rolling_mean, n = 2))
})

test_that("calc_StochasticOscillator matches TTR on shared non-NA region", {
  DT <- data.table::data.table(
    high = seq(103, 162, length.out = 60),
    low = seq(99, 158, length.out = 60),
    close = seq(101, 160, length.out = 60)
  )

  calc_StochasticOscillator(DT, ns = 14, d_ns = 3)
  stoch_ref <- TTR::stoch(
    as.matrix(DT[, .(high, low, close)]),
    nFastK = 14,
    nFastD = 3,
    nSlowD = 1,
    maType = "SMA",
    bounded = TRUE
  )

  expect_equal(DT$stoch_k_14 / 100, as.numeric(stoch_ref[, "fastK"]), tolerance = 1e-12)

  d_ref <- as.numeric(stoch_ref[, "fastD"])
  idx <- which(!is.na(d_ref))
  expect_true(length(idx) > 0)
  expect_equal(DT$stoch_d_14_3[idx] / 100, d_ref[idx], tolerance = 1e-12)
})

test_that("calc_CCI matches manual reference", {
  DT <- data.table::data.table(
    high = c(11, 12, 13, 14, 15),
    low = c(9, 10, 11, 12, 13),
    close = c(10, 11, 12, 13, 14)
  )

  tp <- (DT$high + DT$low + DT$close) / 3
  ref_mean <- strategyr:::rolling_mean(tp, 3)
  ref_mad <- ref_rolling_mean_abs_dev(tp, 3)
  ref_cci <- (tp - ref_mean) / (0.015 * ref_mad)
  ref_cci[ref_mad == 0] <- NA_real_

  calc_CCI(DT, ns = 3)

  expect_equal(DT$cci_3, ref_cci)
})

test_that("calc_CCI matches TTR CCI", {
  DT <- data.table::data.table(
    high = seq(101, 160, length.out = 60),
    low = seq(99, 158, length.out = 60),
    close = seq(100, 159, length.out = 60)
  )

  calc_CCI(DT, ns = 20)

  expect_equal(
    DT$cci_20,
    as.numeric(TTR::CCI(as.matrix(DT[, .(high, low, close)]), n = 20, c = 0.015, maType = "SMA")),
    tolerance = 1e-12
  )
})

test_that("calc_RSI matches TTR RSI", {
  x <- cumsum(c(100, rnorm(59, sd = 1)))
  DT <- data.table::data.table(close = x)

  calc_RSI(DT, ns = 14, hs = NULL)

  expect_equal(
    DT$rsi_14,
    as.numeric(TTR::RSI(x, n = 14)),
    tolerance = 1e-12
  )
})

test_that("calc_RSI adds log-return RSI columns on [0, 100] scale", {
  x <- exp(seq(log(100), log(130), length.out = 60))
  DT <- data.table::data.table(close = x)

  calc_RSI(DT, ns = NULL, hs = c(12, 24))

  expect_true(all(c("rsi_logr_12", "rsi_logr_24") %in% names(DT)))
  expect_true(all(is.na(DT$rsi_logr_12) | (DT$rsi_logr_12 >= 0 & DT$rsi_logr_12 <= 100)))
  expect_true(all(is.na(DT$rsi_logr_24) | (DT$rsi_logr_24 >= 0 & DT$rsi_logr_24 <= 100)))
})

test_that("calc_WMA matches TTR WMA", {
  x <- seq(100, 130, length.out = 60)
  DT <- data.table::data.table(close = x)

  calc_WMA(DT, ns = 10)

  expect_equal(
    DT$wma_10,
    as.numeric(TTR::WMA(x, n = 10)),
    tolerance = 1e-12
  )
})

test_that("calc_HMA matches TTR HMA", {
  x <- seq(100, 160, length.out = 80) + sin(seq_len(80))
  DT <- data.table::data.table(close = x)

  calc_HMA(DT, ns = 20)

  expect_equal(
    DT$hma_20,
    as.numeric(TTR::HMA(x, n = 20)),
    tolerance = 1e-10
  )
})

test_that("calc_DEMA matches TTR DEMA", {
  x <- seq(100, 160, length.out = 80)
  DT <- data.table::data.table(close = x)

  calc_DEMA(DT, ns = 10)

  expect_equal(
    DT$dema_10,
    as.numeric(TTR::DEMA(x, n = 10, v = 1, wilder = FALSE)),
    tolerance = 1e-10
  )
})

test_that("calc_ZLEMA adds expected columns with EMA-like warmup", {
  x <- seq(100, 160, length.out = 80) + sin(seq_len(80))
  DT <- data.table::data.table(close = x)

  calc_ZLEMA(DT, ns = 10)

  expect_true("zlema_10" %in% names(DT))
  expect_true(all(is.na(DT$zlema_10[1:9])))
  expect_false(is.na(DT$zlema_10[10]))
  expect_true(all(is.finite(DT$zlema_10[10:length(x)])))
})

test_that("calc_ADX matches TTR ADX on shared non-NA region", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2,
    close = seq(101, 200, length.out = 120) + cos(seq_len(120)) / 3
  )

  calc_ADX(DT, ns = 14)
  adx_ref <- TTR::ADX(as.matrix(DT[, .(high, low, close)]), n = 14)

  idx <- which(!is.na(adx_ref[, "DIp"]))
  expect_true(length(idx) > 0)
  expect_equal(DT$adx_dip_14[idx], as.numeric(adx_ref[idx, "DIp"]), tolerance = 1e-10)
  expect_equal(DT$adx_din_14[idx], as.numeric(adx_ref[idx, "DIn"]), tolerance = 1e-10)
  expect_equal(DT$adx_dx_14[idx], as.numeric(adx_ref[idx, "DX"]), tolerance = 1e-10)

  idx_adx <- which(!is.na(adx_ref[, "ADX"]))
  expect_true(length(idx_adx) > 0)
  expect_equal(DT$adx_14[idx_adx], as.numeric(adx_ref[idx_adx, "ADX"]), tolerance = 1e-10)
})

test_that("calc_aroon matches TTR aroon", {
  DT <- data.table::data.table(
    high = seq(101, 160, length.out = 60) + sin(seq_len(60)) / 10,
    low = seq(99, 158, length.out = 60) + cos(seq_len(60)) / 10
  )

  calc_aroon(DT, ns = 14)
  ref <- TTR::aroon(as.matrix(DT[, .(high, low)]), n = 14)

  expect_equal(DT$aroon_up_14, as.numeric(ref[, "aroonUp"]), tolerance = 1e-12)
  expect_equal(DT$aroon_dn_14, as.numeric(ref[, "aroonDn"]), tolerance = 1e-12)
  expect_equal(DT$aroon_osc_14, as.numeric(ref[, "oscillator"]), tolerance = 1e-12)
})

test_that("calc_SAR matches TTR SAR", {
  DT <- data.table::data.table(
    high = c(10, 11, 12, 13, 12, 14, 15, 14, 16, 15, 17, 16),
    low = c(9, 10, 11, 12, 11, 13, 14, 13, 15, 14, 16, 15)
  )

  calc_SAR(DT, accels = list(c(0.02, 0.2)))

  expect_equal(
    DT$sar_0p02_0p2,
    as.numeric(TTR::SAR(as.matrix(DT[, .(high, low)]), accel = c(0.02, 0.2))),
    tolerance = 1e-10
  )
})

test_that("calc_MFI matches TTR MFI", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2,
    close = seq(101, 200, length.out = 120) + cos(seq_len(120)) / 3,
    volume = seq(1000, 4000, length.out = 120)
  )

  calc_MFI(DT, ns = 14)

  expect_equal(
    DT$mfi_14,
    as.numeric(TTR::MFI(as.matrix(DT[, .(high, low, close)]), DT$volume, n = 14)),
    tolerance = 1e-10
  )
})

test_that("calc_OBV matches TTR OBV", {
  DT <- data.table::data.table(
    close = c(10, 11, 11, 10.5, 10.8, 10.8, 11.2),
    volume = c(100, 120, 140, 160, 180, 200, 220)
  )

  calc_OBV(DT)

  expect_equal(
    DT$obv,
    as.numeric(TTR::OBV(DT$close, DT$volume)),
    tolerance = 1e-12
  )
})

test_that("calc_VWAP matches TTR VWAP", {
  DT <- data.table::data.table(
    close = seq(100, 130, length.out = 60),
    volume = seq(1000, 4000, length.out = 60)
  )

  calc_VWAP(DT, ns = 10)

  expect_equal(
    DT$vwap_10,
    as.numeric(TTR::VWAP(DT$close, DT$volume, n = 10)),
    tolerance = 1e-12
  )
})

test_that("calc_VWMA matches TTR VWMA", {
  DT <- data.table::data.table(
    close = seq(100, 130, length.out = 60),
    volume = seq(1000, 4000, length.out = 60)
  )

  calc_VWMA(DT, ns = 10)

  expect_equal(
    DT$vwma_10,
    as.numeric(TTR::VWMA(DT$close, DT$volume, n = 10)),
    tolerance = 1e-12
  )
})

test_that("calc_CMF matches TTR CMF", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2,
    close = seq(101, 200, length.out = 120) + cos(seq_len(120)) / 3,
    volume = seq(1000, 4000, length.out = 120)
  )

  calc_CMF(DT, ns = 20)

  expect_equal(
    DT$cmf_20,
    as.numeric(TTR::CMF(as.matrix(DT[, .(high, low, close)]), DT$volume, n = 20)),
    tolerance = 1e-12
  )
})

test_that("calc_WPR matches TTR WPR", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2,
    close = seq(101, 200, length.out = 120) + cos(seq_len(120)) / 3
  )

  calc_WPR(DT, ns = 14)

  expect_equal(
    DT$wpr_14,
    as.numeric(TTR::WPR(as.matrix(DT[, .(high, low, close)]), n = 14)),
    tolerance = 1e-12
  )
})

test_that("calc_SMI matches TTR SMI on shared non-NA region", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2,
    close = seq(101, 200, length.out = 120) + cos(seq_len(120)) / 3
  )

  calc_SMI(DT, n = 13, nFast = 2, nSlow = 25, nSig = 9)
  ref <- TTR::SMI(as.matrix(DT[, .(high, low, close)]), n = 13, nFast = 2, nSlow = 25, nSig = 9)

  idx <- which(!is.na(ref[, "SMI"]))
  expect_true(length(idx) > 0)
  expect_equal(DT$smi_13_2_25[idx], as.numeric(ref[idx, "SMI"]), tolerance = 1e-10)

  idx_sig <- which(!is.na(ref[, "signal"]))
  expect_true(length(idx_sig) > 0)
  expect_equal(DT$smi_signal_13_2_25_9[idx_sig], as.numeric(ref[idx_sig, "signal"]), tolerance = 1e-10)
})

test_that("calc_TRIX matches TTR TRIX on shared non-NA region", {
  DT <- data.table::data.table(close = seq(100, 200, length.out = 160) + sin(seq_len(160)))

  calc_TRIX(DT, ns = 20, signal_ns = 9)
  trix_ref <- TTR::TRIX(DT$close, n = 20, nSig = 9, maType = "EMA", percent = TRUE)

  idx_trix <- which(!is.na(trix_ref[, "TRIX"]))
  expect_true(length(idx_trix) > 0)
  expect_equal(DT$trix_20[idx_trix], as.numeric(trix_ref[idx_trix, "TRIX"]), tolerance = 1e-10)

  idx_sig <- which(!is.na(trix_ref[, "signal"]))
  expect_true(length(idx_sig) > 0)
  expect_equal(DT$trix_signal_20_9[idx_sig], as.numeric(trix_ref[idx_sig, "signal"]), tolerance = 1e-10)
})

test_that("calc_ultimateOscillator matches TTR ultimateOscillator", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2,
    close = seq(101, 200, length.out = 120) + cos(seq_len(120)) / 3
  )

  calc_ultimateOscillator(DT, nss = list(c(7, 14, 28)), wtss = list(c(4, 2, 1)))

  expect_equal(
    DT$uo_7_14_28_4_2_1,
    as.numeric(TTR::ultimateOscillator(as.matrix(DT[, .(high, low, close)]), n = c(7, 14, 28), wts = c(4, 2, 1))),
    tolerance = 1e-10
  )
})

test_that("calc_KST matches TTR KST", {
  x <- seq(100, 200, length.out = 180) + sin(seq_len(180))
  DT <- data.table::data.table(close = x)

  calc_KST(
    DT,
    nss = list(c(10, 10, 10, 15)),
    n_rocss = list(c(10, 15, 20, 30)),
    signal_ns = 9,
    wtss = list(1:4)
  )

  ref <- TTR::KST(x, n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), nSig = 9)

  expect_equal(
    DT$kst_10_10_10_15_10_15_20_30_1_2_3_4,
    as.numeric(ref[, "kst"]),
    tolerance = 1e-10
  )
  expect_equal(
    DT$kst_signal_10_10_10_15_10_15_20_30_1_2_3_4_9,
    as.numeric(ref[, "signal"]),
    tolerance = 1e-10
  )
})

test_that("calc_CMO matches TTR CMO", {
  x <- seq(100, 160, length.out = 80) + sin(seq_len(80))
  DT <- data.table::data.table(close = x)

  calc_CMO(DT, ns = 14)

  expect_equal(
    DT$cmo_14,
    as.numeric(TTR::CMO(x, n = 14)),
    tolerance = 1e-10
  )
})

test_that("calc_chaikinAD matches TTR chaikinAD", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2,
    close = seq(101, 200, length.out = 120) + cos(seq_len(120)) / 3,
    volume = seq(1000, 4000, length.out = 120)
  )

  calc_chaikinAD(DT)

  expect_equal(
    DT$chaikin_ad,
    as.numeric(TTR::chaikinAD(as.matrix(DT[, .(high, low, close)]), DT$volume)),
    tolerance = 1e-10
  )
})

test_that("calc_chaikinVolatility matches TTR chaikinVolatility", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2
  )

  calc_chaikinVolatility(DT, ns = 10)

  expect_equal(
    DT$chaikin_volatility_10,
    as.numeric(TTR::chaikinVolatility(as.matrix(DT[, .(high, low)]), n = 10)),
    tolerance = 1e-10
  )
})

test_that("calc_EMV matches TTR EMV", {
  DT <- data.table::data.table(
    high = seq(103, 202, length.out = 120) + sin(seq_len(120)),
    low = seq(99, 198, length.out = 120) + sin(seq_len(120)) / 2,
    volume = seq(1000, 4000, length.out = 120)
  )

  calc_EMV(DT, ns = 9, vol_divisor = 10000)
  ref <- TTR::EMV(as.matrix(DT[, .(high, low)]), DT$volume, n = 9, vol.divisor = 10000)

  expect_equal(DT$emv, as.numeric(ref[, "emv"]), tolerance = 1e-10)
  expect_equal(DT$emv_ma_9, as.numeric(ref[, "maEMV"]), tolerance = 1e-10)
})
