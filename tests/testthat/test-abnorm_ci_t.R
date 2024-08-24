library(testthat)
library(neuropsytools)

test_that("abnorm_ci_t function works correctly", {
  # Test with known values
  result <- abnorm_ci_t(1.5, 30)
  expect_type(result, "list")
  expect_equal(result$lower, 83.367485)
  expect_equal(result$upper, 97.82481)
  expect_true(result$lower > 0 && result$lower < 100)
  expect_true(result$upper > 0 && result$upper < 100)
}
)
