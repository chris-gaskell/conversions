library(testthat)
library(conversions)

test_that("z_to_derived function converts correctly", {
  actual_output <- convert_standard(2, metric = "z", metric.new = "index")
  expect_equal(actual_output, 130)
})

# Test case for negative z_score
test_that("convert_standard handles negative z_score", {
  expect_equal(convert_standard(-2, metric = "z", metric.new = "index"), 70)
})

# Test case for scaled scores
test_that("convert_standard handles negative z_score", {
  expect_equal(convert_standard(-2, metric = "z", metric.new = "scaled"), 4)
})

# Test case for scaled scores
test_that("convert_standard handles negative z_score", {
  expect_equal(convert_standard(0.4, metric = "scaled", metric.new = "z"), -3.2)
})

# Test case for scaled scores
test_that("convert_standard handles negative z_score", {
  expect_equal(convert_standard(110, metric = "t", metric.new = "z"), 6)
})

# Test case for extreme z_score
test_that("z_to_derived handles extreme z_score", {
  expect_equal(convert_standard(1000.3, metric = "z", metric.new = "index"), 15104.5)
})
