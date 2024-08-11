library(testthat)
library(neuropsytools)

test_that("convert_z function converts correctly", {
  actual_output <- neuropsytools::convert_z(2, metric = "index")
  # Verify the result
  expect_equal(actual_output, 130)
})

# Test case for negative z_score
test_that("convert_z handles negative z_score", {
  expect_equal(neuropsytools::convert_z(-2, metric = "index"), 70)
})

# Test case for scaled scores
test_that("convert_z handles negative z_score", {
  expect_equal(neuropsytools::convert_z(-2, metric = "scaled"), 4)
})

# Test case for scaled scores with decimal z_score
test_that("convert_z handles decimal z_score", {
  expect_equal(neuropsytools::convert_z(-3.2, metric = "scaled"), 0.4)
})

# Test case for t scores
test_that("convert_z handles t scores", {
  expect_equal(neuropsytools::convert_z(6, metric = "t"), 110)
})

# Test case for extreme z_score
test_that("convert_z handles extreme z_score", {
  expect_equal(neuropsytools::convert_z(1000.3, metric = "index"), 15104.5)
})

# Test case for percentile conversion
test_that("convert_z converts to percentile correctly", {
  expect_equal(neuropsytools::convert_z(1, metric = "percentile"), 84.13)
})

# Test case for negative z_score to percentile
test_that("convert_z handles negative z_score to percentile", {
  expect_equal(neuropsytools::convert_z(-1, metric = "percentile"), 15.87)
})

# Test case for z_score of zero to percentile
test_that("convert_z handles zero z_score to percentile", {
  expect_equal(neuropsytools::convert_z(0, metric = "percentile"), 50)
})
