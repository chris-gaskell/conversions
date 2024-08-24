#' Assessing For a Deficit in Test Score When Compared to a Control Sample.
#'
#' Utilises classical (frequentist) statistical methods to compare a single
#' case’s score with scores from a control sample. It also provides an interval
#' estimate of the effect size for the difference between the case and the
#' control group.
#'
#' @details Assess for a dissociation between a single test score and a control
#'   sample using the modified paired samples t-test approach of Crawford et al.
#'   (1998). Unlike earlier methods (e.g. Payne & Jones) this method treats data
#'   from the normative same as sample statistics and not population parameters.
#'   The result provided is a t score and associated p value. This approach
#'   helps to reconcile the problem associated with small normative samples.
#'
#'   In addition to determining whether a difference exists it is also important
#'   to understand the magnitude of that difference. Therefore, it is often
#'   recommended that effect sizes are provided alongside p-values to estimate
#'   the size of the observed effect. To this effect, Crawford et al. (1998) has
#'   provided a method for deriving an effect-size in single-case studies using
#'   the case-controls design (z-cc), where a single patient's cognitive
#'   performance is compared to a matched control group. The modified z-score
#'   (z-cc) is provided as both point and interval estimates.
#'
#'   Finally, neuropsychologists often need to determine how abnormal a
#'   patient's test score is. In the case of the modified t-test, the
#'   abnormality can be easily estimated by multiplying the t-value by 100
#'   (Crawford & Howell, 1998). This estimate quantifies the percentage of the
#'   population expected to exhibit a more extreme score. Confidence limits on
#'   the estimate of abnormality are also provided (Crawford & Garthwaite,
#'   2002).
#'
#' @param score Numeric value representing the score of the single case.
#' @param ctrl.mean Numeric value representing the mean of the control group.
#' @param ctrl.sd Numeric value representing the standard deviation of the
#'   control group.
#' @param ctrl.n Integer value representing the sample size of the control
#'   group.
#' @param conf.level Confidence level (default is 0.95 for 95%).
#' @param direction Character. Specifies the direction of the hypothesis.
#'   Options are "lower" (one-tailed), "higher" (one-tailed), or "two.tailed"
#'   (default, two-tailed).
#' @param tail Character. Specifies whether the test is one-tailed or
#'   two-tailed. Options are "one.tailed" and "two.tailed" (default)
#' @param dp Number of decimal places for rounding the results (default is 2).
#'
#' @return A list of statistical input, parameters, and results. Key outputs
#'   include:
#'   - t value: The t-value calculated for the test.
#'   - p value: The p-value for the test, indicating statistical significance.
#'   - effect-size (z-cc): The z-score (effect-size) corrected for the control group.
#'   - abnormality: The percentage of the population expected to score a more extreme score.
#'
#' @importFrom stats qt pt
#'
#' @references
#'   - Crawford, J.R., & Garthwaite, P.H. (2002). Investigation of the single case in neuropsychology: confidence limits on the abnormality of test scores and test score differences. *Neuropsychologia, 40*(2002), 1196–1208.
#'   - Crawford, J.R., Howell, D.C., & Garthwaite, P.H. (1998). Payne and Jones Revisited: Estimating the Abnormality of Test Score Differences Using a Modified Paired Samples t Test. *Journal of Clinical and Experimental Neuropsychology, 20*(6), 898-905.
#'   - Crawford, J.R., & Howell, D.C. (1998). Comparing an individual’s test score against norms derived from small samples. *The Clinical Neuropsychologist, 12*(4), 482-486.
#'   - Crawford, J.R., Garthwaite, P.H., & Porter, S. (2010). Point and interval estimates of effect sizes for the case-controls design in neuropsychology: Rationale, methods, implementations, and proposed reporting standards. *Cognitive Neuropsychology, 27*(3), 245-260.
#'   - Payne, R. W., & Jones, G. (1957). Statistics for the investigation of individual cases. Journal of Clinical Psychology, 13, 115-121.
#' @seealso
#'   - [deficit_bayes()]: For a Bayesian approach to assessing for a dissociation between a single test score and a control sample for a single case.
#'   - [discrep()]: For assessing a dissociation between two test scores for a single case.
#'   - [abnorm_ci_t()]: For generating interval estimates for abnormality using the modified t test.
#' @export
#' @examples
#' # Two-tailed test example:
#' res <- deficit(score = 130, ctrl.mean = 100, ctrl.sd = 15,
#'           ctrl.n = 30, conf.level = 0.95, direction = "lower",
#'           tail = "two.tailed", dp = 2)
#' print(res)
#'
#' # One-tailed test example (higher):
#' res <- deficit(score = 130, ctrl.mean = 100, ctrl.sd = 15,
#'           ctrl.n = 30, conf.level = 0.95, direction = "higher",
#'           tail = "one.tailed", dp = 2)
#' print(res)
#'
#' # One-tailed test example (lower):
#' res <- deficit(score = 130, ctrl.mean = 100, ctrl.sd = 15,
#'           ctrl.n = 30, conf.level = 0.95, direction = "lower",
#'           tail = "one.tailed", dp = 2)
#' print(res)
deficit <- function(score,
                          ctrl.mean,
                          ctrl.sd,
                          ctrl.n,
                          direction = "lower",
                          tail = "one.tailed",
                          conf.level = 0.95,
                          dp = 2) {

  if (direction != "higher" & direction != "lower") {
    stop("Invalid direction. Options are 'higher' or 'lower'.")
  }

# t -----------------------------------------------------------------------


  t <- (score - ctrl.mean) / (ctrl.sd * sqrt((ctrl.n + 1) / ctrl.n))
  df <- ctrl.n - 1

# p -----------------------------------------------------------------------

  # Determine p-value based on direction and tail
  if (tail == "one.tailed") {
    if (direction == "lower") {
      p.value <- pt(t, df = df, lower.tail = TRUE)
    } else if (direction == "higher") {
      p.value <- pt(t, df = df, lower.tail = FALSE)
    } else {
      stop("Invalid direction. Use 'lower' or 'higher'.")
    }
  } else if (tail == "two.tailed") {
    p.value <- 2 * pt(-abs(t), df = df)
  } else {
    stop("Invalid tail. Use 'one.tailed' or 'two.tailed'.")
  }

  # Calculate abnormality based on one-tailed p-value
  if (direction == "lower") {
    abn <- pt(t, df = df, lower.tail = TRUE) * 100
  } else if (direction == "higher") {
    abn <- pt(t, df = df, lower.tail = FALSE) * 100
  }

# abn and z ---------------------------------------------------------------

  zcc <- (score - ctrl.mean) / ctrl.sd  # also the c1
  ncp <- neuropsytools::abnorm_ci_t(c = zcc, n = ctrl.n, conf.level = conf.level)
  zcc.ci.lb <- ncp$delta.lb$root / sqrt(ctrl.n)
  zcc.ci.ub <- ncp$delta.ub$root / sqrt(ctrl.n)
  abn.ci <- c(as.numeric(ncp$lower), as.numeric(ncp$upper))
  if (direction == "higher") {abn.ci <- 100 - abn.ci}
  abn.ci.lb <- min(abn.ci)
  abn.ci.ub <- max(abn.ci)


# rounding ----------------------------------------------------------------

  p.value = round(p.value, dp)
  zcc = round(zcc, dp)
  zcc.ci.lb = round(zcc.ci.lb, dp)
  zcc.ci.ub = round(zcc.ci.ub, dp)
  abn = round(abn, dp)
  abn.ci.lb = round(abn.ci.lb, dp)
  abn.ci.ub = round(abn.ci.ub, dp)

# output ------------------------------------------------------------------

  input_df <- data.frame(
    item = c("Sample mean", "Sample SD", "Sample size", "Case's test score"),
    value = c(ctrl.mean, ctrl.sd, ctrl.n, score),
    stringsAsFactors = FALSE
  )

  output_df <- data.frame(
    item = c("t value", "p value", "Effect size (z-cc)", "Abnormality"),
    value = c(format(t, nsmall = dp), format(p.value, nsmall = dp), format(zcc, nsmall = dp), paste(format(abn, nsmall = dp), " %", sep = "")),
    ci = c("", "",
           paste(
             format(round(zcc.ci.lb, dp), nsmall = dp), "to",
             format(round(zcc.ci.ub, dp), nsmall = dp), sep = " "), paste(format(round(abn.ci.lb, dp), nsmall = dp), " % to", format(round(abn.ci.ub, dp), nsmall = dp), "%", sep = " ")),
    stringsAsFactors = FALSE
  )

  result <- list(
    score = score,
    ctrl.mean = ctrl.mean,
    ctrl.sd = ctrl.sd,
    ctrl.n = ctrl.n,
    conf.level = conf.level,
    direction = direction,
    dp = dp,
    t = t,
    p.value = p.value,
    zcc = zcc,
    zcc.ci.lb = zcc.ci.lb,
    zcc.ci.ub = zcc.ci.ub,
    abn = abn,
    abn.ci.lb = abn.ci.lb,
    abn.ci.ub = abn.ci.ub,
    input_df = input_df,
    output_df = output_df
  )

  class(result) <- 'deficit'
  return(result)
}

#' @export
print.deficit <- function(x, ...) {

  input_table <- knitr::kable(x$input_df, format = "simple", col.names = c("Variable", "Value"))
  output_table <- knitr::kable(x$output_df, format = "simple", col.names = c("Variable", "Value", glue::glue("{x$conf.level*100}% Confidence Interval")
  ))

  header <- "Assessing For a Frequentist Deficit Between a Test Score and a Control Sample."
  footnote <- "See documentation for further information on how scores are computed."

  result <- paste(header, "\n\n",
                  "INPUTS:", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "PARAMETERS:",  "\n\n",
                    paste("Deficit Method:", toupper(x$method)),"\n",
                    paste("Confidence Interval Method:", "Modified T"),"\n",
                    paste("Confidence Intervals:", x$conf.level*100, "%"),"\n",
                    paste("Direction Indicating Impairment: ", x$direction.y,  sep = ""),"\n\n",
                  "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                  footnote, "\n",
                  sep = "")

  cat(result)
}

# Example usage:
# res <- deficit(130, 100, 15, 30, conf.level = 0.95, direction = "lower", dp = 2)
# res
#
# res <- deficit(130, 100, 15, 30, conf.level = 0.95, direction = "lower", dp = 2)
# res

