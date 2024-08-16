library(testthat)

# attempt 1: four methods with 95% CI -------------------------------------

test_that("95% confidence interval", {
  t1.score = 100
  norm.t1.mean = 103
  norm.t1.sd = 10
  norm.t2.mean = 109
  norm.t2.sd = 8
  norm.r = 0.76
  norm.n = 100
  t2.score = 120
  ci = .95


  res <- predict_t2(method = "chelune", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 106)
  expect_equal(res$t2.sem, 6.93)
  expect_equal(res$t2.ci.lb, 92.42)
  expect_equal(res$t2.ci.ub, 119.58)

  res <- predict_t2(method = "crawford", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 106.6)
  expect_equal(res$t2.sem, round(6.307883, 2))
  # do confidence intervals
  res <- predict_t2(method = "jacobson", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 100)
  expect_equal(res$t2.sem, 6.93)
  expect_equal(res$t2.ci.lb, 86.42)
  expect_equal(res$t2.ci.ub, 113.58)
  #res <- predict_t2(method = "mcsweeny", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  #expect_equal(res$t2.expected, 107.18)
  #expect_equal(res$t2.sem, 5.2)
  #expect_equal(res$t2.ci.lb, 96.99)
  #expect_equal(res$t2.ci.ub, 117.37)
  res <- predict_t2(method = "iverson", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 106)
  expect_equal(res$t2.sem, 6.27)
  expect_equal(res$t2.ci.lb, 93.71)
  expect_equal(res$t2.ci.ub, 118.29)
  res <- predict_t2(method = "maassen", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 106.6)
  expect_equal(res$t2.sem, 6.27)
  expect_equal(res$t2.ci.lb, 94.31)
  expect_equal(res$t2.ci.ub, 118.89)

})

# attempt 2: four methods with 90% CI -------------------------------------


test_that("90% confidence interval", {
  t1.score = 88
  norm.t1.mean = 113
  norm.t1.sd = 7.2
  norm.t2.mean = 118.7
  norm.t2.sd = 8.3
  norm.r = 0.93
  norm.n = 821
  t2.score = 103
  ci = .9

  res <- predict_t2(method = "chelune", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 93.7)
  expect_equal(res$t2.sem, 2.69)
  expect_equal(res$t2.ci.lb, 89.28)
  expect_equal(res$t2.ci.ub, 98.12)
  res <- predict_t2(method = "crawford", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 89.88)
  expect_equal(res$t2.sem, round(2.930128, 2))
  # do confidence intervals
  res <- predict_t2(method = "jacobson", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 88)
  expect_equal(res$t2.sem, 2.69)
  expect_equal(res$t2.ci.lb, 83.58)
  expect_equal(res$t2.ci.ub, 92.42)
  #res <- predict_t2(method = "mcsweeny", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  #expect_equal(res$t2.expected, 91.9)
  #expect_equal(res$t2.sem, 3.05)
  #expect_equal(res$t2.ci.lb, 86.88)
  #expect_equal(res$t2.ci.ub, 96.92)
  res <- predict_t2(method = "iverson", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 93.7)
  expect_equal(res$t2.sem, 2.91)
  expect_equal(res$t2.ci.lb, 88.91)
  expect_equal(res$t2.ci.ub, 98.49)
  res <- predict_t2(method = "maassen", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)
  expect_equal(res$t2.expected, 89.88)
  expect_equal(res$t2.sem, 2.91)
  expect_equal(res$t2.ci.lb, 85.09)
  expect_equal(res$t2.ci.ub, 94.67)

})

test_that("missing arguments", {
  t1.score = 100
  norm.t1.mean = 103
  norm.t1.sd = 10
  norm.t2.mean = 109
  norm.t2.sd = 8
  norm.r = 0.76
  norm.n = 100
  t2.score = 120
  ci = .95

  expect_error(predict_t2(method = "maassen", norm.t1.mean = norm.t1.mean,  norm.t1.sd = norm.t1.sd, norm.t2.mean = norm.t2.mean, norm.t2.sd = norm.t2.sd, norm.r = norm.r, norm.n = norm.n, conf.level = ci),
               "Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")
  expect_error(predict_t2(method = "maassen", t1.score = t1.score, norm.t1.sd = norm.t1.sd, norm.t2.mean = norm.t2.mean, norm.t2.sd = norm.t2.sd, norm.r = norm.r, norm.n = norm.n, conf.level = ci),
               "Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")
  expect_error(predict_t2(method = "maassen", t1.score = t1.score, norm.t1.mean = norm.t1.mean, norm.t2.mean = norm.t2.mean, norm.t2.sd = norm.t2.sd, norm.r = norm.r, norm.n = norm.n, conf.level = ci),
               "Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")
  expect_error(predict_t2(method = "maassen", t1.score = t1.score, norm.t1.mean = norm.t1.mean,  norm.t2.mean = norm.t2.mean, norm.t2.sd = norm.t2.sd, norm.r = norm.r, norm.n = norm.n, conf.level = ci),
               "Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")
  expect_error(predict_t2(method = "maassen", t1.score = t1.score, norm.t1.mean = norm.t1.mean,  norm.t1.sd = norm.t1.sd, norm.t2.sd = norm.t2.sd, norm.r = norm.r, norm.n = norm.n, conf.level = ci),
               "Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")
  expect_error(predict_t2(method = "maassen", t1.score = t1.score, norm.t1.mean = norm.t1.mean,  norm.t1.sd = norm.t1.sd, norm.t2.mean = norm.t2.mean, norm.r = norm.r, norm.n = norm.n, conf.level = ci),
               "Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")
  expect_error(predict_t2(method = "maassen", t1.score = t1.score, norm.t1.mean = norm.t1.mean,  norm.t1.sd = norm.t1.sd, norm.t2.mean = norm.t2.mean, norm.t2.sd = norm.t2.sd, norm.n = norm.n, conf.level = ci),
               "Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")
  expect_error(predict_t2(method = "maassen", t1.score = t1.score, norm.t1.mean = norm.t1.mean,  norm.t1.sd = norm.t1.sd, norm.t2.mean = norm.t2.mean, norm.r = norm.r, conf.level = ci),
               "Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")

})


# mcsweeney here does not match the mcsweeny SRB in the Duff spreadsheet.
# Speer, Charter, Crawford (98), Temkin,  is not represented in the Duff spreadsheet.

# two different complex SRBs. Crawford required the T2 estimate (from a regression formula), whereas Massen requires this and also the SEE.

# t1.score = 100
# norm.t1.mean = 103
# norm.t1.sd = 10
# norm.t2.mean = 109
# norm.t2.sd = 8
# norm.r = 0.76
# norm.n = 100
# t2.score = 120
# ci = .95
#
#
# predict_t2(method = "chelune", t1.score, norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, ci)

