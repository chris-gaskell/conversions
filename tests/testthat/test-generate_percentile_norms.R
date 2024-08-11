test_data <- c(1, 2, 3, 4, 5, 5, 6, 7, 8, 9, 10)
test_that("generate_percentile_norms calculates percentiles correctly", {
  # Call the function with the test data
  result <- generate_percentile_norms(test_data)

  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the result contains the expected columns
  expect_true(all(c("raw", "count", "definition_a", "definition_c", "definition_b") %in% colnames(result)))

  # Check if the count column sums up to the length of the data
  expect_equal(sum(result$count), length(test_data))

  # Add more specific tests depending on your requirements

  expect_equal(ncol(generate_percentile_norms(test_data, method = "all", range = c(1,22))), 5) # columns
  expect_equal(nrow(generate_percentile_norms(test_data, method = "all", range = c(1,22))), length(1:22)) # columns

})

