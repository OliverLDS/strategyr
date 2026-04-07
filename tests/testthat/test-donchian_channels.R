ref_rolling_max <- function(x, n) {
  len <- length(x)
  out <- rep(NA_real_, len)
  if (n <= 0 || n > len) {
    return(out)
  }

  for (i in seq_len(len)) {
    if (i >= n) {
      w <- x[(i - n + 1):i]
      out[i] <- if (any(is.na(w))) NA_real_ else max(w)
    }
  }
  out
}

ref_rolling_min <- function(x, n) {
  len <- length(x)
  out <- rep(NA_real_, len)
  if (n <= 0 || n > len) {
    return(out)
  }

  for (i in seq_len(len)) {
    if (i >= n) {
      w <- x[(i - n + 1):i]
      out[i] <- if (any(is.na(w))) NA_real_ else min(w)
    }
  }
  out
}

test_that("rolling max and min match reference", {
  x <- c(3, 1, 4, 2, 5, 0, 6)
  n <- 3

  expect_equal(strategyr:::rolling_max(x, n), ref_rolling_max(x, n))
  expect_equal(strategyr:::rolling_min(x, n), ref_rolling_min(x, n))
})

test_that("rolling max and min propagate NA windows", {
  x <- c(3, 1, NA, 2, 5, 0)
  n <- 3

  expect_equal(strategyr:::rolling_max(x, n), ref_rolling_max(x, n))
  expect_equal(strategyr:::rolling_min(x, n), ref_rolling_min(x, n))
})

test_that("calc_DonchianChannels adds expected columns", {
  DT <- data.table::data.table(
    high = c(10, 11, 12, 13, 14),
    low = c(5, 6, 7, 8, 9)
  )

  calc_DonchianChannels(DT, ns = 3)

  expect_true(all(c("dc_high_3", "dc_low_3", "dc_mid_3") %in% names(DT)))
  expect_equal(DT$dc_high_3, c(NA, NA, 12, 13, 14))
  expect_equal(DT$dc_low_3, c(NA, NA, 5, 6, 7))
  expect_equal(DT$dc_mid_3, c(NA, NA, 8.5, 9.5, 10.5))
})

test_that("rolling max and min error on n=0", {
  x <- 1:5
  expect_error(strategyr:::rolling_max(x, 0))
  expect_error(strategyr:::rolling_min(x, 0))
})
