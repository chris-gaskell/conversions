library(testthat)
library(stringr)

test_that("sem_to_percentiles works without abnormality", {
  sem <- c(3.000000, 3.354102, 3.674235, 4.743416)
  scores <- c(118, 107, 77, 68)
  names <- c("verbal comprehension", "perceptual reasoning",
             "working memory", "processing speed")

  result <- sem_to_percentiles(scores, sem = sem, conf.level = 0.90, names = names, abnormality = FALSE)

  expect_equal(result$k, 4)
  expect_equal(length(result$ci.lb), 4)
  expect_equal(length(result$ci.ub), 4)
  expect_equal(1, sum(result$rank < 5))
  expect_null(result$abn)
})

test_that("sem_to_percentiles works with abnormality", {
  sem <- c(3.000000, 3.354102, 3.674235, 4.743416)
  R <- matrix(c(1.00, 0.61, 0.64, 0.45,
                0.61, 1.00, 0.62, 0.52,
                0.64, 0.62, 1.00, 0.51,
                0.45, 0.52, 0.51, 1.00), nrow = 4, byrow = TRUE)
  scores <- c(118, 107, 77, 68)
  names <- c("verbal comprehension", "perceptual reasoning",
             "working memory", "processing speed")

  result <- sem_to_percentiles(scores, sem = sem, conf.level = 0.90, names = names, R = R, abnormality = TRUE)

  expect_equal(result$k, 4)
  expect_equal(length(result$ci.lb), 4)
  expect_equal(length(result$ci.ub), 4)
  expect_equal(result$abn.k, sum(as.numeric(result$rank) < 5))
})
