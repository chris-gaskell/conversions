library(testthat)
library(neuropsytools)

test_that("dissoc_single function works correctly", {
  result <- dissoc_single(score = 130, ctrl.mean = 100, ctrl.n = 30, ctrl.sd = 15)

  expect_type(result, "list")
  expect_true(as.numeric(result$t) > 0)
  expect_true(as.numeric(result$p.value) >= 0 && as.numeric(result$p.value) <= 1)
})

test_that("dissoc_single function works correctly", {
  result <- dissoc_single(ctrl.m = 12.78, ctrl.sd = 3.45, ctrl.n = 16, score = 4.0, conf.level = .05, direction = "lower", dp = 2)
  expect_equal(round(result$t, 2), -2.47) # t
  expect_equal(round(result$p.value, 3), .01) # p
  expect_equal(round(result$abn, 2), 1.30) # prev
  expect_equal(round(result$abn.ci.lb, 2), 0.02) # prev
  expect_equal(round(result$abn.ci.ub, 3), 6.56) # prev
  expect_equal(round(result$zcc, 2),  round(-2.545, 2)) # prev
  expect_equal(round(result$zcc.ci.lb, 2), round(-3.561, 2)) # prev
  expect_equal(round(result$zcc.ci.ub, 2), round(-1.509, 2)) # prev
})
