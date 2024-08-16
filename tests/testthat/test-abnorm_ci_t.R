test_that("abnorm_ci_t function works correctly", {
  # Test with known values
  result <- abnorm_ci_t(1.5, 30)
  expect_type(result, "list")
  expect_equal(result[['2.5%']], 83.367485)
  expect_equal(result[['97.5%']], 97.82481)
  expect_true(result[['2.5%']] > 0 && result[['2.5%']] < 100)
  expect_true(result[['97.5%']] > 0 && result[['97.5%']] < 100)
}
)
