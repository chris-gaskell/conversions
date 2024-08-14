#' Calculate abnormality of two test scores with confidence intervals using the modified t-test.
#'
#' @param ctrl.mean.x Mean of the control group for the first test.
#' @param ctrl.sd.x Standard deviation of the control group for the first test.
#' @param ctrl.mean.y Mean of the control group for the second test.
#' @param ctrl.sd.y Standard deviation of the control group for the second test.
#' @param ctrl.r.xy Correlation between the two tests in the control group.
#' @param ctrl.n Size of the control group.
#' @param score.x Test score of the individual for the first test.
#' @param score.y Test score of the individual for the second test.
#' @param conf.level Confidence level for the interval (default is 0.05).
#' @param direction Direction of the test, either "lower" or "higher" (default is "lower").
#' @param dp Number of decimal places for rounding the results (default is 2).
#' @param test.names A vector of two strings representing the names of the tests (default is c("X", "Y")).
#'
#' @return A list containing the z-scores for both tests, control group means and standard deviations, correlation between tests, control group size, scores for both tests, confidence level, direction of the test, t-value, p-value, two-tailed p-value, zdcc, zdcc confidence interval lower bound, zdcc confidence interval upper bound, abnormality percentage, abnormality confidence interval lower bound, and abnormality confidence interval upper bound.
#' @export
#'
#' @examples
#' dissociation_double(100, 15, 110, 10, 0.5, 30, 130, 120, test.names = c("Test A", "Test B"))
dissociation_double <- function(ctrl.mean.x,
                                ctrl.sd.x,
                                ctrl.mean.y,
                                ctrl.sd.y,
                                ctrl.r.xy,
                                ctrl.n,
                                score.x,
                                score.y,
                                conf.level = 0.95,
                                direction = "lower",
                                dp = 4,
                                test.names = c("X", "Y")) {

  z.x <- (score.x - ctrl.mean.x) / ctrl.sd.x
  z.y <- (score.y - ctrl.mean.y) / ctrl.sd.y
  t <- round((z.x - z.y) / sqrt((2 - (2 * ctrl.r.xy)) * ((ctrl.n + 1) / ctrl.n)), 3)
  df <- ctrl.n - 1

  if (direction == "lower") {
    p.one.tailed <- pt(t, df = df, lower.tail = TRUE)
  } else if (direction == "higher") {
    p.one.tailed <- pt(t, df = df, lower.tail = FALSE)
  } else {
    stop("Invalid direction. Use 'lower' or 'higher'.")
  }
  p.two.tailed <- 2 * min(p.one.tailed, 1 - p.one.tailed)
  crit.value <- qt(p = (1 - conf.level), df)

  zdcc <- (z.x - z.y) / sqrt(2 - (2 * ctrl.r.xy))
  ncp <- neuropsytools::prevalence_intervals_t(c = zdcc, n = ctrl.n)
  zdcc.ci.lb <- ncp$delta.lb$root / sqrt(ctrl.n)
  zdcc.ci.ub <- ncp$delta.ub$root / sqrt(ctrl.n)

  c2 <- (z.x - z.y) / sqrt(2 - (2 * ctrl.r.xy))
  p.two.tailed <- 2 * min(p.one.tailed, 1 - p.one.tailed)

  abn <- (abs(p.one.tailed)) * 100


  abn_ci <- neuropsytools::prevalence_intervals_t(c = c2, n = ctrl.n)
  abn.ci.lb <- round(as.numeric(abn_ci$`2.5%`), digits = 5)
  abn.ci.ub <- round(as.numeric(abn_ci$`97.5%`), digits = 5)

  #if (direction == "higher") { abn <- 100 - abn }
  if (direction == "higher") { abn.ci.lb <- 100 - abn.ci.lb }
  if (direction == "higher") { abn.ci.ub <- 100 - abn.ci.ub }

  abn.ci.lb.check <- min(abn.ci.lb, abn.ci.ub)
  abn.ci.ub.check <- max(abn.ci.lb, abn.ci.ub)


  result <- list(
    test.names = test.names,
    z.x = round(z.x, dp),
    z.y = round(z.y, dp),
    ctrl.mean.x = ctrl.mean.x,
    ctrl.sd.x = ctrl.sd.x,
    ctrl.mean.y = ctrl.mean.y,
    ctrl.sd.y = ctrl.sd.y,
    ctrl.r.xy = ctrl.r.xy,
    ctrl.n = ctrl.n,
    score.x = score.x,
    score.y = score.y,
    conf.level = conf.level,
    direction = direction,
    t = round(t, dp),
    p.one.tailed = round(p.one.tailed, dp),
    p.two.tailed = round(p.two.tailed, dp),
    zdcc = round(zdcc, dp),
    zdcc.ci.lb = round(zdcc.ci.lb, dp),
    zdcc.ci.ub = round(zdcc.ci.ub, dp),
    abn = round(abn, dp),
    abn.ci.lb = round(abn.ci.lb.check, dp),
    abn.ci.ub = round(abn.ci.ub.check, dp)
  )

  class(result) <- 'dissociation_double'
  return(result)
}

#' @export
print.dissociation_double <- function(x, ...) {

  # Define input data frame
  input_df <- data.frame(
    test =  x$test.names,
    mean =  c(x$ctrl.mean.x, x$ctrl.mean.y),
    sd =  c(x$ctrl.sd.x, x$ctrl.sd.y),
    n = c(x$ctrl.n, ""),
    r = c(x$ctrl.r.xy, ""),
    value = c(x$score.x, x$score.y),
    stringsAsFactors = FALSE
  )


  # Define output data frame with a separate column for confidence intervals
  output_df <- data.frame(
    Item = c(paste("Effect size (z) for test", x$test.names[1]),
             paste("Effect size (z) for test", x$test.names[2]),
             paste("Effect size (z-dcc) between", x$test.names[1], "and", x$test.names[2]),
             "t value",
             "One-tailed p-value",
             "Two-tailed p-value",
             "Abnormality"
    ),
    Value = c(format(x$z.x, nsmall = 3),
              format(x$z.y, nsmall = 3),
              format(x$zdcc, nsmall = 3),
              format(x$t, nsmall = 3),
              format(x$p.one.tailed, nsmall = 4),
              format(x$p.two.tailed, nsmall = 4),
              paste(format(x$abn, nsmall = 4), " %", sep = "")
    ),
    CI = c("",
           "",
           paste(x$zdcc.ci.lb, " to ", x$zdcc.ci.ub, sep = ""),
           "",
           "",
           "",
           paste(x$abn.ci.lb, " % to ", x$abn.ci.ub, " %", sep = "")
    ), stringsAsFactors = FALSE
  )

  # Create the input and output tables
  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Test", "Mean", "SD", "Sample size", "r", "Case score"))
  output_table <- knitr::kable(output_df, format = "simple", col.names = c("Variable", "Value", paste0(x$conf.level * 100, "% Confidence Interval")))

  # Define the header and description
  header <- "Frequentist Double Dissociation"
  description <- "Frequentist point estimate and confidence limits on the abnormality of test score differences within a single case."
  reference <- "Based on: Crawford, Garthwaite & Howell (1998)."

  # Combine all parts into the final result
  result <- paste(header, "\n\n",
                  description, "\n\n",
                  "INPUTS:", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                  reference, "\n",
                  sep = "")

  # Print the result
  cat(result)
}

