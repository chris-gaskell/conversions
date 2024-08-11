test_that("prevalence_intervals_t function works correctly", {
  # Test with known values
  result <- prevalence_intervals_t(1.5, 30)
  expect_type(result, "list")
  expect_true(result[['2.5%']] > 0 && result[['2.5%']] < 100)
  expect_true(result[['97.5%']] > 0 && result[['97.5%']] < 100)
}
)
