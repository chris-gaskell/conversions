library(testthat)

# Tests for percentile_vector function
test_that("percentile_vector function returns correct percentiles", {
  data <- c(1, 2, 3, 4, 5)
  expect_equal(percentile_vector(data, method = "a"), c(0, 20, 40, 60, 80))
  expect_equal(percentile_vector(data, method = "b"), c(20, 40, 60, 80, 100))
  expect_equal(percentile_vector(data, method = "c"), c(10, 30, 50, 70, 90))

  data <- c(32, 17, 92, 40, 51, 21, 33, 46)
  expect_equal(percentile_vector(data, method = "a"), c(25, 0, 87.5, 50, 75, 12.5, 37.5, 62.5))
  expect_equal(percentile_vector(data, method = "b"), c(37.5, 12.5, 100, 62.5, 87.5, 25, 50, 75))
  expect_equal(percentile_vector(data, method = "c"), c(31.25, 6.25, 93.75, 56.25, 81.25, 18.75, 43.75, 68.75))

})
