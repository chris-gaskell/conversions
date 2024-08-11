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
#' @param dp Number of decimal places for rounding the results (default is 5).
#'
#' @return A list containing the z-scores for both tests, control group means and standard deviations, correlation between tests, control group size, scores for both tests, confidence level, direction of the test, t-value, p-value, two-tailed p-value, zdcc, zdcc confidence interval lower bound, zdcc confidence interval upper bound, abnormality percentage, abnormality confidence interval lower bound, and abnormality confidence interval upper bound.
#' @export
#'
#' @examples
#' dissociation_double(100, 15, 110, 10, 0.5, 30, 130, 120)
dissociation_double <- function(ctrl.mean.x,
                                ctrl.sd.x,
                                ctrl.mean.y,
                                ctrl.sd.y,
                                ctrl.r.xy,
                                ctrl.n,
                                score.x,
                                score.y,
                                conf.level = 0.05,
                                direction = "lower",
                                dp = 5) {

  z.x <- (score.x-ctrl.mean.x)/ctrl.sd.x
  z.y <- (score.y-ctrl.mean.y)/ctrl.sd.y

  t <- round( (z.x-z.y)/sqrt((2-(2*ctrl.r.xy))*((ctrl.n+1)/ctrl.n)), 3)
  df <- ctrl.n - 1
  alpha <- 1 - conf.level

  crit.value <- qt(p = alpha, df)
  if (direction == "lower") {p <- pt(t, df = df, lower.tail = TRUE)}
  if (direction == "higher") {p <- pt(t, df = df, lower.tail = FALSE)}

  zdcc <- (z.x-z.y)/ sqrt(2-(2*ctrl.r.xy))
  ncp <- neuropsytools::prevalence_intervals_t(c = zdcc, n = ctrl.n)
  zdcc.ci.lb <- ncp$delta.lb$root/sqrt(ctrl.n)
  zdcc.ci.ub <- ncp$delta.ub$root/sqrt(ctrl.n)

  c2 <- (z.x - z.y)/sqrt(2-(2*ctrl.r.xy))
  p.two.tailed <- p*2

  abn <- (abs(p)) * 100

  abn_ci <- neuropsytools::prevalence_intervals_t(c = c2, n = ctrl.n)
  abn.ci.lb <- round(as.numeric(abn_ci$`2.5%`), digits = 5)
  abn.ci.ub <- round(as.numeric(abn_ci$`97.5%`), digits = 5)

  if (direction == "higher") {abn <- 100 - abn}
  if (direction == "higher") {abn.ci.lb <- 100 - abn.ci.lb}
  if (direction == "higher") {abn.ci.ub <- 100 - abn.ci.ub}

  result <- list(
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
    p = round(p, dp),
    p.two.tailed = round(p.two.tailed, dp),
    zdcc = round(zdcc, dp),
    zdcc.ci.lb = round(zdcc.ci.lb, dp),
    zdcc.ci.ub = round(zdcc.ci.ub, dp),
    abn = round(abn, dp),
    abn.ci.lb = round(abn.ci.lb, dp),
    abn.ci.ub = round(abn.ci.ub, dp)
  )

  class(result) <- 'dissociation_double'
  return(result)

}


#' @export
print.dissociation_double <- function(x, ...) {

  report <- glue::glue(
    "Frequentist point estimate and confidence limits on the abn of test score differences within a single case. ",
    "Based on: Crawford, Garthwaite & Howell (1998).\n\n",

    "INPUTS:\n",
    "Data from control sample used to test dissociation:\n",
    "Sample mean for test X:  {x$ctrl.mean.x}\n",
    "Sample SD for test X:    {x$ctrl.sd.x}\n",
    "Sample mean for test Y:  {x$ctrl.mean.y}\n",
    "Sample mean for test Y:  {x$ctrl.sd.y}\n",
    "r between tests X and Y: {x$ctrl.r.xy}\n",
    "Sample size:             {x$ctrl.n}\n",
    "Case's score on test X:  {x$score.x}\n",
    "Case's score on test Y:  {x$score.y}\n\n",

    "OUTPUTS:\n",
    "Effect size (z) for test X:          {x$z.x}\n",
    "Effect size (z) for test Y:          {x$z.y}\n",
    "Effect size (z-dcc) between X and Y: {x$zdcc} (95% CI = {x$zdcc.ci.lb} to {x$zdcc.ci.ub})\n",
    "t value:                             {x$t}\n",
    "One-tailed pval:                     {x$p}\n",
    "Two-tailed pval:                     {x$p.two.tailed}\n",
    "Abnormality: {x$abn} % (CI = {x$abn.ci.lb} % to {x$abn.ci.ub} %) \n\n",

    "note. t value is based on the approach developed by Crawford, Garthwaite & Howell (1998)"
  )


result <- paste(report, sep = "")
cat(result)

}




# Difflims ----------------------------------------------------------------

# Crawford, Garthwaite & Howell 1998

# ctrl.mean.x = 52
# ctrl.sd.x = 10
# ctrl.mean.y = 44
# ctrl.sd.y = 8
# ctrl.r.xy = .73
# ctrl.n = 100
# score.x = 38
# score.y = 56




