ref_runMean <- function(x, n) {
  as.numeric(TTR::runMean(x, n))
}

test_that("rolling mean matches TTR::runMean", {
  x <- rnorm(100)
  n <- 10

  expect_equal(
    rolling_mean(x, n),
    ref_runMean(x, n),
    tolerance = 1e-12
  )
})

testthat::test_that("rolling mean errors on n=0 (kernel returns error)", {
  x <- 1:5
  testthat::expect_error(cpp_rolling_mean(x, 0))
})

