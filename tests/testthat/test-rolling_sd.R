ref_runSD <- function(x, n, sample = FALSE) {
  as.numeric(TTR::runSD(x, n, sample = sample))
}

test_that("rolling sd matches TTR::runSD", {
  x <- rnorm(100)
  n <- 10

  expect_equal(
    rolling_sd(x, n, sample = FALSE),
    ref_runSD(x, n, sample = FALSE),
    tolerance = 1e-12
  )

  expect_equal(
    rolling_sd(x, n, sample = TRUE),
    ref_runSD(x, n, sample = TRUE),
    tolerance = 1e-12
  )
})

testthat::test_that("rolling sd errors on n=0", {
  x <- 1:5
  testthat::expect_error(rolling_sd(x, 0))
})

testthat::test_that("rolling sd errors on sample=TRUE with n<2", {
  x <- 1:5
  testthat::expect_error(rolling_sd(x, 1, sample = TRUE))
})
