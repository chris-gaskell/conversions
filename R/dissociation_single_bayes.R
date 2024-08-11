#' Calculate abnormality of a test score with Bayesian confidence intervals.
#'
#' @param score Test score of the individual.
#' @param ctrl.mean Mean of the control group.
#' @param ctrl.sd Standard deviation of the control group.
#' @param ctrl.n Size of the control group.
#' @param conf.level Confidence level for the interval (default is 0.95).
#' @param sims Number of simulations for the Bayesian estimation (default is 10000).
#' @param treshold Threshold for determining abnormality (default is 0.1).
#' @param direction Direction of the test, either "lower" or "higher" (default is "lower").
#' @param dp Number of decimal places for rounding the results (default is 5).
#'
#' @return A list containing the score, control group mean, control group standard deviation, control group size, confidence level, number of simulations, threshold, direction of the test, degrees of freedom, control group variance, z-value, z-value confidence interval lower bound, z-value confidence interval upper bound, percentile, p-value, two-tailed p-value, abnormality percentage, abnormality confidence interval lower bound, and abnormality confidence interval upper bound.
#' @importFrom stats rchisq
#' @importFrom stats rnorm
#' @export
#'
#' @examples
#' dissociation_single_bayes(130, 100, 15, 30)
dissociation_single_bayes <- function(
    score,
    ctrl.mean,
    ctrl.sd,
    ctrl.n,
    conf.level = .95,
    sims = 10000,
    treshold = 0.1,
    direction = "lower",
    dp = 5) {

  df <-  ctrl.n-1
  ctrl.var <- ctrl.sd^2

  p.sims <- c()
  for (i in 1:sims) {
    # step 1
    psi <- rchisq(1, df = df, ncp = 0)
    o <- (ctrl.n - 1) * ctrl.var / psi

    # step 2
    z.sim <- rnorm(1, 0, 1)
    u <- ctrl.mean + z.sim * sqrt((o / ctrl.n))

    # step 3
    z <- (score - u) / sqrt(o)
    p <- 2 * (1 - pnorm(abs(z), lower.tail = TRUE)) # One-tailed p-value
    p.sims <- c(p.sims, p)
  }



  # z values ----------------------------------------------------------------

  z.scores <- c()
  for (i in 1:sims) {
    # Step 1
    psi <- rchisq(1, df = df, ncp = 0)
    o <- (ctrl.n - 1) * ctrl.var / psi

    # Step 2
    z.sim <- rnorm(1, 0, 1)
    u <- ctrl.mean + z.sim * sqrt((o / ctrl.n))

    # Step 3
    z <- (score - u) / sqrt(o)

    # Store the z-score
    z.scores <- c(z.scores, z)
  }


  # Point estimates ---------------------------------------------------------

  z <- (score - ctrl.mean) / ctrl.sd
  percentile <- pnorm(z)*100
  z.ci <- bayestestR::hdi(z.scores, ci = conf.level / 100)
  z.ci.ub <- z.ci$CI_low
  z.ci.lb <- z.ci$CI_high

  p.sims <- p.sims / 2
  p <- mean(p.sims)
  p.two.tailed <- p*2

  abn <- (1 - p) * 100
  abn.ci <- bayestestR::hdi(p.sims, ci = conf.level)
  abn.ci.ub <- (1 - abn.ci$CI_high)*100
  abn.ci.lb <- (1 - abn.ci$CI_low)*100


  result <- list(
    score = score,
    ctrl.mean = ctrl.mean,
    ctrl.sd = ctrl.sd,
    ctrl.n = ctrl.n,
    conf.level = conf.level,
    sims = sims,
    treshold = treshold,
    direction = direction,
    dp = dp,
    df = df,
    ctrl.var = ctrl.var,
    z = round(z, dp),
    z.ci.lb = round(z.ci.lb, dp),
    z.ci.ub = round(z.ci.ub, dp),
    percentile = round(percentile, dp),
    p = round(p, dp),
    p.two.tailed = round(p.two.tailed, dp),
    abn = round(abn, dp),
    abn.ci.lb = round(abn.ci.lb, dp),
    abn.ci.ub = round(abn.ci.ub, dp)
  )

  class(result) <- 'dissociation_single_bayes'
  return(result)
}



#' @export
print.dissociation_single_bayes <- function(x, ...) {

  report <- glue::glue(
    "ADD HEADER.\n\n",

    "INPUTS:\n",
    "Data from control sample used to test dissociation:\n",
    "Sample mean: {x$ctrl.mean}\n",
    "Sample SD:   {x$ctrl.sd}\n",
    "Sample size: {x$ctrl.n}\n",
    "Case's test score = {x$score}\n\n",

    "OUTPUTS:\n",
    #"t value:            {x$t}\n",
    "One-tailed pval:    {x$p}\n",
    "Two-tailed pval:    {x$p.two.tailed}\n",
    "Effect size (Z-CC): {x$z} (CI = {x$z.ci.lb} to {x$z.ci.ub})\n",
    "Abnormality:        {x$abn} (CI = {x$abn.ci.lb} % to {x$abn.ci.ub} %)\n\n",

    "note. t value is based on ADD REFFFF"
  )


  result <- paste(report, sep = "")
  cat(result)

}



# dissociation_single_bayes(
#   ctrl.mean = 100,
#   ctrl.sd = 15,
#   ctrl.n = 30,
#   score = 110
# )
#
#


