# t1.score = 88
# norm.t1.mean = 113
# norm.t1.sd = 7.2
# norm.t2.mean = 118.7
# norm.t2.sd = 8.3
# norm.r = 0.93
# norm.n = 821
# t2.score = 103
# ci = .95

test_that("standard error of mean", {
  my_sd =  7.2
  my_n =  0.93
  my_vector = c(1,2,3,4)
  expect_equal(round(se_mean(sd = my_sd, n = my_n), 6), 7.466052)
  expect_equal(round(se_mean(sd = 7.2, n = 0.93), 6), 7.466052)
  expect_equal(round(se_mean(dat = c(7.2, 3.6), n = my_n), 6), 1.8)
  expect_equal(round(se_mean(dat = my_vector, n = 0.93), 6), 0.6454970)
  expect_error(round(se_mean(sd = c(7.2, 7.2), n = 0.93)),
               "If sd has more than one value, please provide 'dat' instead.")
  expect_error(round(se_mean(sd = my_vector, n = 0.93)),
               "If sd has more than one value, please provide 'dat' instead.")
  expect_error(round(se_mean(sd = "charachter", n = 0.93)),
               "sd must be numeric.")
})


test_that("standard error of measurement", {
  my_sd =  7.2
  my_r =  0.93
  my_vector = c(1,2,3,4)
  expect_equal(round(se_measurement(sd = my_sd, r = my_r), 6), 1.904941)
  expect_equal(round(se_measurement(sd = 7.2, r = 0.93), 6), 1.904941)
  expect_equal(round(se_measurement(dat = c(7.2, 3.6), r = 0.93), 6), 0.673498)
  expect_equal(round(se_measurement(dat = my_vector, r = 0.93), 6), 0.341565)
  expect_error(round(se_measurement(sd = c(7.2, 7.2), r = 0.93)),
               "If sd has more than one value, please provide 'dat' instead.")
  expect_error(round(se_measurement(sd = my_vector, r = 0.93)),
               "If sd has more than one value, please provide 'dat' instead.")
  expect_error(round(se_measurement(sd = "charachter", r = 0.93)),
               "sd must be numeric.")
})


test_that("standard error of difference", {
  my_vector_1 = c(1,2,3,4)
  my_vector_2 = c(2,3,4,5)
  my_sd_1 =  7.2
  my_sd_2 =  8.1
  my_r =  0.93
  expect_equal(round(se_difference(sd.1 = 3, sd.2 = 3, r = .95), 6), 0.948683)
  expect_equal(round(se_difference(sd.1 = my_sd_1, sd.2 = my_sd_2, r = my_r), 6), 2.867316)
  expect_equal(round(se_difference(sd.1 = my_vector_1, sd.2 = my_vector_2, r = my_r), 6), 0.483046)
  expect_equal(round(se_difference(sd.1 = c(2,3,4), sd.2 = c(2,3,4), r = my_r), 6), 0.374166)
  expect_error(round(se_difference(sd.1 = "3", sd.2 = 3, r = .95)),"sd.1 must be numeric.")
  expect_error(round(se_difference(sd.1 = c(3,2), sd.2 = 3, r = .95)),"sd.1 and sd.2 must be of equal length.")

})



test_that("standard error of prediction", {
  my_vector_1 = c(1,2,3,4)
  my_vector_2 = c(2,3,4,5)
  my_sd_1 =  7.2
  my_sd_2 =  8.1
  my_r =  0.93
  expect_equal(round(se_prediction(sd.1 = 3, sd.2 = 3, r = .95), 6), 0.948683)
  expect_equal(round(se_prediction(sd.1 = my_sd_1, sd.2 = my_sd_2, r = my_r), 6), 2.867316)
  expect_equal(round(se_prediction(sd.1 = my_vector_1, sd.2 = my_vector_2, r = my_r), 6), 0.483046)
  expect_equal(round(se_prediction(sd.1 = c(2,3,4), sd.2 = c(2,3,4), r = my_r), 6), 0.374166)
  expect_error(round(se_prediction(sd.1 = "3", sd.2 = 3, r = .95)),"sd.1 must be numeric.")
  expect_error(round(se_prediction(sd.1 = c(3,2), sd.2 = 3, r = .95)),"sd.1 and sd.2 must be of equal length.")
})


test_that("standard error of n1 (Predictionw with Crawford adjusted)", {
  my_vector = c(1,2,3,4)
  my_sd_1 =  7.2
  my_sd_2 =  5.2
  my_r =  0.93
  my_n = 100
  my_score_1 = 100
  my_score_2 = 130
  norm.t1.mean = 121
  norm.t1.sd = 8

      expect_equal(
        round(
          se_n1(sd.1 = my_sd_1, sd.2 = my_sd_2, r = my_r, n = my_n,
          t1.score = my_score_1, norm.t1.mean = norm.t1.mean),
          6), 2.459935
    )

})






