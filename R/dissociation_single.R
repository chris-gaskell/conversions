#' Assessing For a Frequentist Single Dissociation Between a Test Score and a
#' Control Sample.
#'
#' Assess for a dissociation between a single test score and a control sample
#' using a modified t-test and estimate the abnormality of the test score,
#' including confidence intervals.
#'
#' @details Perform a dissociation between a single test score and a control
#'   sample using the modified paired samples t-test approach of Crawford et al.
#'   (1998). Unlike earlier (e.g. Payne & Jones) this method treats data from
#'   the normative as sample statistics and not population parameters. The
#'   result provided is a t score and associated p value. This approach helps to
#'   reconcile the problem associated with small control samples.
#'
#'   In addition to determining whether a difference exists it is also important
#'   to understand the magnitude of that difference. Therefore, it is often
#'   recommended that effect sizes are provided alongside p-values to estimate
#'   the size of the observed effect. To this effect, Crawford et al. (1998)
#'   provde a method for deriving an effect-size in single-case studies using
#'   the case-controls design, where a single patient's cognitive performance is
#'   compared to a matched control group. The modified z-score (z-cc) is
#'   provided as both point and interval estimates.
#'
#'   Neuropsychologists often need to determine how abnormal a patient's test
#'   score is. In this context, the abnormality can be easily estimated by
#'   multiplying the t-value from the modified t-test by 100, as suggested by
#'   Crawford and Howell (1998). This estimate quantifies the percentage of the
#'   population expected to exhibit a more extreme score. It can be calculated
#'   for a specific direction (i.e., a higher or lower score), or alternatively,
#'   the abnormality in 'either' direction can be determined by dividing the
#'   percentage by two.
#'
#'   Methods have also been provided to calculate confidence limits on the
#'   estimate of abnormality (Crawford & Garthwaite, 2002) which are included.
#'
#' @param score Test score for the individual case.
#' @param ctrl.mean Mean of the control group.
#' @param ctrl.sd Standard deviation of the control group.
#' @param ctrl.n Size of the control group.
#' @param conf.level Confidence level (default is 0.95 for 95%).
#' @param direction Direction of the test, either "lower" or "higher" (default
#'   is "lower").
#' @param dp Number of decimal places for rounding the results (default is 2).
#'
#' @return A list of statistical input, parameters, and results. Key outputs
#'   include:
#'   - t value: The t-value calculated for the test.
#'   - p value: The p-value for the test, indicating statistical significance.
#'   - effect-size (zcc): The z-score (effect-size) corrected for the control group.
#'   - abnormality: The percentage of the population expected to score a more extreme score.
#' @importFrom stats qt pt
#'
#' @references
#'   - Crawford, J.R., & Garthwaite, P.H. (2002). Investigation of the single case in neuropsychology: confidence limits on the abnormality of test scores and test score differences. *Neuropsychologia, 40*(2002), 1196–1208.
#'   - Crawford, J.R., Howell, D.C., & Garthwaite, P.H. (1998). Payne and Jones Revisited: Estimating the Abnormality of Test Score Differences Using a Modified Paired Samples t Test. *Journal of Clinical and Experimental Neuropsychology, 20*(6), 898-905.
#'   - Crawford, J.R., & Howell, D.C. (1998). Comparing an individual’s test score against norms derived from small samples. *The Clinical Neuropsychologist, 12*(4), 482-486.
#'   - Crawford, J.R., Garthwaite, P.H., & Porter, S. (2010). Point and interval estimates of effect sizes for the case-controls design in neuropsychology: Rationale, methods, implementations, and proposed reporting standards. *Cognitive Neuropsychology, 27*(3), 245-260.
#'   - Payne, R. W., & Jones, G. (1957). Statistics for the investigation of individual cases. Journal of Clinical Psychology, 13, 115-121.
#' @seealso
#'   - [dissociation_double()]: For assessing a dissociation between two test scores for a single case.
#'   - [dissociation_bayes_single()]: For a Bayesian approach to assessing for a dissociation between a single test score and a control sample for a single case.
#'   - [prevalence_intervals_t()]: For generating interval estimates for abnormality using the modified t test.
#' @export
dissociation_single <- function(score,
                                ctrl.mean,
                                ctrl.sd,
                                ctrl.n,
                                conf.level = 0.95,
                                direction = "lower",
                                dp = 2) {

  t <- (score - ctrl.mean) / (ctrl.sd * sqrt((ctrl.n + 1) / ctrl.n))
  df <- ctrl.n - 1

  if (direction == "lower") {
    p.one.tailed <- pt(t, df = df, lower.tail = TRUE)
  } else if (direction == "higher") {
    p.one.tailed <- pt(t, df = df, lower.tail = FALSE)
  } else {
    stop("Invalid direction. Use 'lower' or 'higher'.")
  }

  p.two.tailed <- 2 * min(p.one.tailed, 1 - p.one.tailed)

  abn <- (abs(1 - p.one.tailed)) * 100
  if (direction == "higher") {
    abn <- (1 - (abs(1 - p.one.tailed))) * 100
  }

  zcc <- (score - ctrl.mean) / ctrl.sd  # also the c1
  ncp <- neuropsytools::prevalence_intervals_t(c = zcc, n = ctrl.n)
  zcc.ci.lb <- ncp$delta.lb$root / sqrt(ctrl.n)
  zcc.ci.ub <- ncp$delta.ub$root / sqrt(ctrl.n)
  abn.ci.lb <- min(as.numeric(ncp$`2.5%`), as.numeric(ncp$`97.5%`))
  abn.ci.ub <- max(as.numeric(ncp$`2.5%`), as.numeric(ncp$`97.5%`))

  if (direction == "lower") {
    abn <- 100 - abn
  }

  if (direction == "higher") {
    abn.ci.lb <- 100 - abn.ci.lb
    abn.ci.ub <- 100 - abn.ci.ub
  }

  result <- list(
    score = score,
    ctrl.mean = ctrl.mean,
    ctrl.sd = ctrl.sd,
    ctrl.n = ctrl.n,
    conf.level = conf.level,
    direction = direction,
    dp = dp,
    t = round(t, dp),
    p.one.tailed = round(p.one.tailed, dp),
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

    input_df <- data.frame(
    item = c("Sample mean", "Sample SD", "Sample size", "Case's test score"),
    value = c(x$ctrl.mean, x$ctrl.sd, x$ctrl.n, x$score),
    stringsAsFactors = FALSE
  )

  output_df <- data.frame(
    item = c("t value", paste("p-val (", x$direction, ")", sep = ""), "p-val (either direction)", "Effect size (Z-CC)", "Abnormality"),
    value = c(format(x$t, nsmall = x$dp), format(x$p.one.tailed, nsmall = x$dp), format(x$p.two.tailed, nsmall = x$dp), format(x$zcc, nsmall = x$dp), paste(format(x$abn, nsmall = x$dp), " %", sep = "")),
    ci = c("", "","", paste(format(round(x$zcc.ci.lb, x$dp), nsmall = x$dp), "to",    format(round(x$zcc.ci.ub, x$dp), nsmall = x$dp), sep = " "), paste(format(round(x$abn.ci.lb, x$dp), nsmall = x$dp), " % to", format(round(x$abn.ci.ub, x$dp), nsmall = x$dp), "%", sep = " ")),
    stringsAsFactors = FALSE
  )

  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Variable", "Value"))
  output_table <- knitr::kable(output_df, format = "simple", col.names = c("Variable", "Value", glue::glue("{x$conf.level*100}% Confidence Interval")))
  header <- "Frequentist Single Dissociation Between a Test Score and a Control Sample."
  footnote <- "see documentation for further information on computation."

  result <- paste(header, "\n\n",
                  "INPUTS:",  paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                  footnote, "\n",
                  sep = "")

  cat(result)
}

# Example usage:
# res <- dissociation_single(130, 100, 15, 30, conf.level = 0.95, direction = "lower", dp = 2)
# res
