library(testthat)

test_that("generate_composite returns expected results", {
  result <- generate_composite(
    r = c(0.87, 0.94, 0.94),
    r.between = c(0.74, 0.64, 0.73),
    dp = 3
  )

  expect_equal(result$k, 3)
  expect_equal(result$composite_r, 0.965)
  expect_equal(result$original_mean, 30)
  expect_equal(result$original_sd, 7.482)
  expect_equal(result$transformed.mean, 100)
  expect_equal(result$transformed.sd, 15)
  expect_equal(result$transformed.sem, 2.791)
  expect_equal(result$transformed.semt, 2.695)
})

