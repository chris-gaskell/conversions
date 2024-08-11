library(testthat)
library(conversions)

test_that("percentile function returns correct generate_generate_percentile", {
  data <- c(1, 2, 3, 4, 5)
  # Test percentile method 'c'
  expect_equal(generate_percentile(data, 3, method = "c"), 50)

  # Test percentile method 'a'
  expect_equal(generate_percentile(data, 3, method = "a"), 40)

  # Test percentile method 'b'
  expect_equal(generate_percentile(data, 3, method = "b"), 60)
})
