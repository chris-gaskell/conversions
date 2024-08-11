#' Calculate abnormality of a test score with confidence intervals using the modified t-test.
#'
#' @param score Test score of the individual.
#' @param ctrl.mean Mean of the control group.
#' @param ctrl.sd Standard deviation of the control group.
#' @param ctrl.n Size of the control group.
#' @param conf.level Confidence level for the interval (default is 0.05).
#' @param direction Direction of the test, either "lower" or "higher" (default is "lower").
#' @param dp Number of decimal places for rounding the results (default is 5).
#'
#' @return A list containing the score, control group mean, control group standard deviation, control group size, confidence level, direction of the test, number of decimal places, t-value, p-value, two-tailed p-value, zcc, zcc confidence interval lower bound, zcc confidence interval upper bound, abnormality percentage, abnormality confidence interval lower bound, and abnormality confidence interval upper bound.
#' @importFrom stats qt
#' @export
#'
#' @examples
#' dissociation_single(130, 100, 15, 30)
dissociation_single <- function(score,
                                ctrl.mean,
                                ctrl.sd,
                                ctrl.n,
                                conf.level = 0.05,
                                direction = "lower",
                                dp = 5) {
  # Calculate the t-value
  t <- round((score - ctrl.mean) / (ctrl.sd * sqrt((ctrl.n + 1) / ctrl.n)),3)
  df <- ctrl.n - 1
  alpha <- 1 - conf.level

  # Calculate the critical t-value and p-value
  crit.value <- qt(p = alpha, df)
  if (direction == "lower") {p <- pt(t, df = df, lower.tail = TRUE)}
  if (direction == "higher") {p <- pt(t, df = df, lower.tail = FALSE)}


  # Calculate abn percentage
  abn <- (abs(1 - p)) * 100
  if (direction == "higher") {abn <- (1-(abs(1 - p))) * 100}
  p <- round(p, 3)
  p.two.tailed <- p*2

  # Calculate c1 and its normalized value
  zcc <- (score - ctrl.mean) / ctrl.sd  # also the c1
  ncp <- neuropsytools::prevalence_intervals_t(c = zcc, n = ctrl.n)
  zcc.ci.lb <- ncp$delta.lb$root/sqrt(ctrl.n)
  zcc.ci.ub <- ncp$delta.ub$root/sqrt(ctrl.n)

  # Calculate confidence intervals using the t_abn_ci function
  abn.ci.lb <- as.numeric(ncp$`2.5%`)
  abn.ci.ub <- as.numeric(ncp$`97.5%`)

  if (direction == "lower") {abn <- round(100 - abn, 5)}
  if (direction == "higher") {abn.ci.lb <- 100 - abn.ci.lb}
  if (direction == "higher") {abn.ci.ub <- 100 - abn.ci.ub}

  result <- list(
    score = score,
    ctrl.mean = ctrl.mean,
    ctrl.sd = ctrl.sd,
    ctrl.n = ctrl.n,
    conf.level = conf.level,
    direction = direction,
    dp = dp,
    t = round(t, dp),
    p = round(p, dp),
    p.two.tailed = round(p.two.tailed, dp),
    zcc = round(zcc, dp),
    zcc.ci.lb = round(zcc.ci.lb, dp),
    zcc.ci.ub = round(zcc.ci.ub, dp),
    abn = round(abn, dp),
    abn.ci.lb = round(abn.ci.lb, dp),
    abn.ci.ub = round(abn.ci.ub, dp)
  )

  class(result) <- 'dissociation_single'
  return(result)

}


#' @export
print.dissociation_single <- function(x, ...) {

  report <- glue::glue(
    "Frequentist significance test and point & interval estimates of effect size and abn for a patient's score\n\n",
    "Based on: Crawford, Garthwaite, & Porter (2010), Crawford & Garthwaite (2002), and Crawford & Howell (1998).\n\n",

     "INPUTS:\n",
    "Data from control sample used to test dissociation:\n",
    "Sample mean: {x$ctrl.mean}\n",
    "Sample SD:   {x$ctrl.sd}\n",
    "Sample size: {x$ctrl.n}\n",
    "Case's test score = {x$score}\n\n",

    "OUTPUTS:\n",
    "t value:            {x$t}\n",
    "One-tailed pval:    {x$p}\n",
    "Two-tailed pval:    {x$p.two.tailed}\n",
    "Effect size (Z-CC): {x$zcc} (CI = {x$zcc.ci.lb} to {x$zcc.ci.ub})\n",
    "Abnormality:        {x$abn} (CI = {x$abn.ci.lb} to {x$abn.ci.ub})\n"

    )

  result <- paste(report, sep = "")
  cat(result)
}

#dissociation_single(100, 15, 30, 130, direction = "higher")
