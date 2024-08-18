#' Assessing For a Frequentist Dissociation Between Two Test Scores for an
#' Individual Case.
#'
#' Using the modified t-test approach Assesses whether the difference in scores
#' between two tests for a patient is significantly larger than the differences
#' found in a control or normative sample. The analysis yields a significance
#' test, a point estimate of the abnormality of the score difference, and
#' confidence intervals for the abnormality of the difference.
#'
#' @details Assess for a dissociation between two test scores for a single case
#'   using the modified paired samples t-test approach of Crawford et al. (1998)
#'   by comparison to a normative sample. Unlike earlier methods (e.g. Payne &
#'   Jones) this method treats data from the normative same as sample statistics
#'   and not population parameters. The result provided is a t score and
#'   associated p value. This approach helps to reconcile the problem associated
#'   with small normative samples.
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
#' @inheritParams dissoc_single
#' @param ctrl.mean.x Mean of the control group for the first test.
#' @param ctrl.sd.x Standard deviation of the control group for the first test.
#' @param ctrl.mean.y Mean of the control group for the second test.
#' @param ctrl.sd.y Standard deviation of the control group for the second test.
#' @param ctrl.r.xy Correlation between the two tests in the control group.
#' @param ctrl.n Size of the control group.
#' @param score.x Test score of the individual case for the first test.
#' @param score.y Test score of the individual case for the second test.
#' @param test.names A vector of two strings representing the names of the tests
#'   (default is c("X", "Y")).
#'
#' @return A list of statistical input, parameters, and results. Key outputs
#'   include:
#'   - t value: The t-value calculated for the test.
#'   - p value: The p-value for the test, indicating statistical significance.
#'   - effect-size (z-cc): The z-score (effect-size) corrected for the control group.
#'   - abnormality: The percentage of the population expected to score a more extreme score.
#' @references
#'   - Crawford, J.R., & Garthwaite, P.H. (2002). Investigation of the single case in neuropsychology: confidence limits on the abnormality of test scores and test score differences. *Neuropsychologia, 40*(2002), 1196–1208.
#'   - Crawford, J.R., Howell, D.C., & Garthwaite, P.H. (1998). Payne and Jones Revisited: Estimating the Abnormality of Test Score Differences Using a Modified Paired Samples t Test. *Journal of Clinical and Experimental Neuropsychology, 20*(6), 898-905.
#'   - Crawford, J.R., & Howell, D.C. (1998). Comparing an individual’s test score against norms derived from small samples. *The Clinical Neuropsychologist, 12*(4), 482-486.
#'   - Crawford, J.R., Garthwaite, P.H., & Porter, S. (2010). Point and interval estimates of effect sizes for the case-controls design in neuropsychology: Rationale, methods, implementations, and proposed reporting standards. *Cognitive Neuropsychology, 27*(3), 245-260.
#'   - Payne, R. W., & Jones, G. (1957). Statistics for the investigation of individual cases. Journal of Clinical Psychology, 13, 115-121.
#' @seealso
#'   - [dissoc_single()]: Assessing For a frequentist single dissociation between a test score and a control sample.
#'   - [dissoc_bayes_single()]: For a Bayesian approach to assessing for a dissociation between a single test score and a control sample for a single case.
#'   - [abnorm_ci_t()]: For generating interval estimates for abnormality using the modified t test.
#' @export
#' @examples
#' dissoc_discrep(100, 15, 110, 10, 0.5, 30, 130, 120, test.names = c("Fluency", "Sequencing"))
dissoc_discrep <- function(ctrl.mean.x,
                                ctrl.sd.x,
                                ctrl.mean.y,
                                ctrl.sd.y,
                                ctrl.r.xy,
                                ctrl.n,
                                score.x,
                                score.y,
                                direction = "lower",
                                tail = "one.tailed",
                                test.names = c("X", "Y"),
                                conf.level = 0.95,
                                dp = 2
                           ) {

  z.x <- (score.x - ctrl.mean.x) / ctrl.sd.x
  z.y <- (score.y - ctrl.mean.y) / ctrl.sd.y
  t <- round((z.x - z.y) / sqrt((2 - (2 * ctrl.r.xy)) * ((ctrl.n + 1) / ctrl.n)), 3)
  df <- ctrl.n - 1

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

  crit.value <- qt(p = (1 - conf.level), df)

  zdcc <- (z.x - z.y) / sqrt(2 - (2 * ctrl.r.xy))
  ncp <- neuropsytools::abnorm_ci_t(c = zdcc, n = ctrl.n)
  zdcc.ci.lb <- ncp$delta.lb$root / sqrt(ctrl.n)
  zdcc.ci.ub <- ncp$delta.ub$root / sqrt(ctrl.n)

  c2 <- (z.x - z.y) / sqrt(2 - (2 * ctrl.r.xy))

  abn.ci <- c(as.numeric(ncp$`2.5%`), as.numeric(ncp$`97.5%`))
  if (direction == "higher") {abn.ci <- 100 - abn.ci}
  abn.ci.lb <- min(abn.ci)
  abn.ci.ub <- max(abn.ci)


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
    p.value = round(p.value, dp),
    zdcc = round(zdcc, dp),
    zdcc.ci.lb = round(zdcc.ci.lb, dp),
    zdcc.ci.ub = round(zdcc.ci.ub, dp),
    abn = round(abn, dp),
    abn.ci.lb = round(abn.ci.lb, dp),
    abn.ci.ub = round(abn.ci.ub, dp),
    direction = direction,
    dp = dp
  )

  class(result) <- 'dissoc_discrep'
  return(result)
}

#' @export
print.dissoc_discrep <- function(x, ...) {

  input_df <- data.frame(
    test =  x$test.names,
    mean =  c(x$ctrl.mean.x, x$ctrl.mean.y),
    sd =  c(x$ctrl.sd.x, x$ctrl.sd.y),
    n = c(x$ctrl.n, ""),
    r = c(x$ctrl.r.xy, ""),
    value = c(x$score.x, x$score.y),
    stringsAsFactors = FALSE
  )

  output_df <- data.frame(
    item = c(paste("Effect size (z) for test", x$test.names[1]),
             paste("Effect size (z) for test", x$test.names[2]),
             paste("Effect size (z-dcc) between", x$test.names[1], "and", x$test.names[2]),
             "t value",
             "p-value",
             "Abnormality"
    ),
    value = c(format(x$z.x, nsmall = x$dp),
              format(x$z.y, nsmall = x$dp),
              format(x$zdcc, nsmall = x$dp),
              format(x$t, nsmall = x$dp),
              format(x$p.value, nsmall = x$dp),
              paste(format(x$abn, nsmall = x$dp), " %", sep = "")
    ),
    ci = c("",
           "",
           paste(format(x$zdcc.ci.lb, nsmall = x$dp), " to ", format(x$zdcc.ci.ub, nsmall = x$dp), sep = ""),
           "",
           "",
           paste(x$abn.ci.lb, " % to ", x$abn.ci.ub, " %", sep = "")
    ), stringsAsFactors = FALSE
  )

  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Test", "Mean", "SD", "Sample size", "r", "Case score"))
  output_table <- knitr::kable(output_df, format = "simple", col.names = c("Variable", "Value", paste0(x$conf.level * 100, "% Confidence Interval")))
  header <- "Frequentist Dissociation Between Two Test Scores Compared to a Control Sample."
  footnote <- "See documentation for further information on how scores are computed."

  result <- paste(header, "\n\n",
                  "INPUTS:", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                  footnote, "\n",
                  sep = "")

  cat(result)
}

#dissoc_discrep(100, 15, 110, 10, 0.5, 30, 130, 120, test.names = c("Fluency", "Sequencing"))
