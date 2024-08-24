  #' Assessing for a Frequentist Discrepancy Between Two Test Scores for an
  #' Individual Case.
  #'
  #' This function offers a range of approaches for assess whether the
  #' discrepancy in scores between two tests for a patient is significantly
  #' larger than that which would be expected for a control or normative sample.
  #' The analysis yields a significance test, a point estimate of the
  #' abnormality of the score difference, and confidence intervals for the
  #' abnormality of the difference.
  #'
  #' @details This function assesses a dissociation between two test scores for
  #'   a single case using a range of methods.
  #'
  #'   The modified paired samples t-test approach of Crawford et al. (1998) is
  #'   used by comparison to a normative sample. Unlike earlier methods (e.g.,
  #'   Payne & Jones), this method treats data from the normative sample as
  #'   sample statistics and not population parameters. The result provided is a
  #'   t score and associated p value. This approach helps to address the
  #'   problem associated with small normative samples.
  #'
  #'   RSDT
  #'
  #'   USDT
  #'
  #'   check this... "rsdt" (revised standardized difference test), "usdt"
  #'   (uncorrected standardized difference test), or "difflims" (difference
  #'   limits method).
  #'
  #'   In addition to determining whether a difference exists, it is also
  #'   important to understand the magnitude of that difference. Therefore, it
  #'   is often recommended that effect sizes are provided alongside p-values to
  #'   estimate the size of the observed effect. To this effect, Crawford et al.
  #'   (1998) provided a method for deriving an effect size in single-case
  #'   studies using the case-controls design (z-cc), where a single patient's
  #'   cognitive performance is compared to a matched control group. The
  #'   modified z-score (z-cc) is provided as both point and interval estimates.
  #'
  #'   Finally, neuropsychologists often need to determine how abnormal a
  #'   patient's test score is. In the case of the modified t-test, the
  #'   abnormality can be easily estimated by multiplying the t-value by 100
  #'   (Crawford & Howell, 1998). This estimate quantifies the percentage of the
  #'   population expected to exhibit a more extreme score. Confidence limits on
  #'   the estimate of abnormality are also provided (Crawford & Garthwaite,
  #'   2002).
  #'
  #' @inheritParams deficit
  #' @param ctrl.mean.x Mean of the control group for the first test.
  #' @param ctrl.sd.x Standard deviation of the control group for the first
  #'   test.
  #' @param ctrl.mean.y Mean of the control group for the second test.
  #' @param ctrl.sd.y Standard deviation of the control group for the second
  #'   test.
  #' @param ctrl.r.xy Correlation between the two tests in the control group.
  #' @param ctrl.n Size of the control group.
  #' @param score.x Test score of the individual case for the first test.
  #' @param score.y Test score of the individual case for the second test.
  #' @param direction.x Specifies the expected direction for the first test
  #'   score. Use "lower" if a lower score indicates worse performance or
  #'   "higher" if a higher score indicates worse performance (default:
  #'   "lower").
  #' @param direction.y Specifies the expected direction for the second test
  #'   score. Use "lower" if a lower score indicates worse performance or
  #'   "higher" if a higher score indicates worse performance (default:
  #'   "lower").
  #' @param method Character string specifying the method used to calculate the
  #'   discrepancy: "rsdt" (revised standardized difference test), "usdt"
  #'   (uncorrected standardized difference test), or "difflims" (difference
  #'   limits method). Default is "rsdt".
  #' @param test.names A vector of two strings representing the names of the
  #'   tests (default is c("X", "Y")).
  #'
  #' @return A list containing statistical inputs, parameters, and results. Key
  #'   outputs include:
  #'   - t value: The t-value calculated for the test.
  #'   - p value: The p-value for the test, indicating statistical significance.
  #'   - effect size (z-cc): The z-score (effect size) corrected for the control group.
  #'   - abnormality: The percentage of the population expected to score a more extreme score.
  #' @references
  #'   - Crawford, J.R., & Garthwaite, P.H. (2002). Investigation of the single case in neuropsychology: confidence limits on the abnormality of test scores and test score differences. *Neuropsychologia, 40*(2002), 1196–1208.
  #'   - Crawford, J.R., Howell, D.C., & Garthwaite, P.H. (1998). Payne and Jones Revisited: Estimating the Abnormality of Test Score Differences Using a Modified Paired Samples t Test. *Journal of Clinical and Experimental Neuropsychology, 20*(6), 898-905.
  #'   - Crawford, J.R., & Howell, D.C. (1998). Comparing an individual’s test score against norms derived from small samples. *The Clinical Neuropsychologist, 12*(4), 482-486.
  #'   - Crawford, J.R., Garthwaite, P.H., & Porter, S. (2010). Point and interval estimates of effect sizes for the case-controls design in neuropsychology: Rationale, methods, implementations, and proposed reporting standards. *Cognitive Neuropsychology, 27*(3), 245-260.
  #'   - Payne, R. W., & Jones, G. (1957). Statistics for the investigation of individual cases. *Journal of Clinical Psychology, 13*, 115-121.
  #' @seealso
  #'   - [deficit()]: Assessing for a frequentist single dissociation between a test score and a control sample.
  #'   - [deficit_bayes()]: For a Bayesian approach to assessing for a dissociation between a single test score and a control sample for a single case.
  #'   - [abnorm_ci_t()]: For generating interval estimates for abnormality using the modified t-test.
  #' @export
  #' @examples
  #' discrep(100, 15, 110, 10, 0.5, 30, 130, 120, direction.x = "lower",
  #'   direction.y = "higher", test.names = c("Fluency", "Sequencing"))
  discrep <- function(ctrl.mean.x, ctrl.sd.x,
                      ctrl.mean.y, ctrl.sd.y,
                      ctrl.r.xy, ctrl.n,
                      score.x, score.y,
                      direction.x = "lower",
                      direction.y = "lower",
                      tail = "one.tailed",
                      test.names = c("test X", "test Y"),
                      conf.level = 0.95,
                      dp = 2,
                      method = "rsdt") {


  # z -----------------------------------------------------------------------

    z.x <- (score.x - ctrl.mean.x) / ctrl.sd.x
    if (direction.x == "higher") { z.x <- -z.x }

    z.y <- (score.y - ctrl.mean.y) / ctrl.sd.y
    if (direction.y == "higher") { z.y <- -z.y }

    df <- ctrl.n - 1


  # t -----------------------------------------------------------------------

    diff.x <- score.x - ctrl.mean.x
    diff.y <- score.y - ctrl.mean.y
    if (direction.x == "higher") {
      diff.x <- -diff.x
    }
    if (direction.y == "higher") {
      diff.y <- -diff.y
    }

    if (method == "rsdt") {
      a <- (1 + ctrl.r.xy) * (1 - ctrl.r.xy^2)
      b <- (1 - ctrl.r.xy) * (
        4*(ctrl.n - 1)^2 + 4*(1 + ctrl.r.xy)*(ctrl.n - 1) + (1 + ctrl.r.xy)*(5 + ctrl.r.xy)
      )
      c <- -2 * (
        (diff.x / ctrl.sd.x - diff.y / ctrl.sd.y)^2 *
          (ctrl.n * (ctrl.n - 1)^2) / (ctrl.n + 1)
      )
      t <- sqrt((-b + sqrt(b^2 - 4*a*c)) / (2*a))

    } else if (method == "usdt") {
      var.x <- ctrl.sd.x^2
      var.y <- ctrl.sd.y^2
      cov <- ctrl.sd.x * ctrl.sd.y * ctrl.r.xy
      t <- (diff.x - diff.y) /
        sqrt((var.x + var.y - 2*cov) * ((ctrl.n + 1) / ctrl.n))

    } else if (method == "difflims") {
      t <- round((z.x - z.y) / sqrt((2 - (2 * ctrl.r.xy)) * ((ctrl.n + 1) / ctrl.n)), 3)
    }

  # p -----------------------------------------------------------------------

      if (tail == "one.tailed") {
      p <- pt(t, df = df, lower.tail = FALSE)
    } else if (tail == "two.tailed") {
      p <- 2 * pt(-abs(t), df = df)
    } else {
      stop("Invalid tail. Use 'one.tailed' or 'two.tailed'.")
    }

  # abnormality -------------------------------------------------------------

    if (tail == "one.tailed") {
      abn <- p * 100
    } else if (tail == "two.tailed") {
      abn <- p * 100 / 2
    } else {
      stop("Invalid tail. Use 'one.tailed' or 'two.tailed'.")
    }

    zdcc <- (z.x - z.y) / sqrt(2 - 2*ctrl.r.xy)
    ncp <- neuropsytools::abnorm_ci_t(c = zdcc, n = ctrl.n, conf.level = conf.level)
    zdcc.ci.lb <- ncp$delta.lb$root / sqrt(ctrl.n)
    zdcc.ci.ub <- ncp$delta.ub$root / sqrt(ctrl.n)

    abn.ci <- c(as.numeric(ncp$lower), as.numeric(ncp$upper))
    abn.ci <- 100 - abn.ci
    abn.ci.lb <- min(abn.ci)
    abn.ci.ub <- max(abn.ci)


  # rounding ----------------------------------------------------------------

    t = round(t, dp)
    p = round(p, dp)
    z.x = round(z.x, dp)
    z.y = round(z.y, dp)
    zdcc = round(zdcc, dp)
    zdcc.ci.lb = round(zdcc.ci.lb, dp)
    zdcc.ci.ub = round(zdcc.ci.ub, dp)
    abn = round(abn, dp)
    abn.ci.lb = round(abn.ci.lb, dp)
    abn.ci.ub = round(abn.ci.ub, dp)

  # output ------------------------------------------------------------------

    input_df <- data.frame(
      test =  test.names,
      mean =  c(ctrl.mean.x, ctrl.mean.y),
      sd =  c(ctrl.sd.x, ctrl.sd.y),
      n = c(ctrl.n, ""),
      r = c(ctrl.r.xy, ""),
      value = c(score.x, score.y),
      stringsAsFactors = FALSE
    )

    output_df <- data.frame(
      item = c(paste("Effect size (z) for ", test.names[1]),
               paste("Effect size (z) for ", test.names[2]),
               paste("Effect size (z-dcc) between", test.names[1], "and", test.names[2]),
               "t value",
               "p value",
               "% of controls expected to show a higher discrepancy"
      ),
      value = c(format(z.x, nsmall = dp),
                format(z.y, nsmall = dp),
                format(zdcc, nsmall = dp),
                format(t, nsmall = dp),
                format(p, nsmall = dp),
                paste(format(abn, nsmall = dp), " %", sep = "")
      ),
      ci = c("",
             "",
             paste(format(zdcc.ci.lb, nsmall = dp), " to ", format(zdcc.ci.ub, nsmall = dp), sep = ""),
             "",
             "",
             paste(abn.ci.lb, " % to ", abn.ci.ub, " %", sep = "")
      ), stringsAsFactors = FALSE
    )

    result <- list(
      test.names = test.names,
      ctrl.mean.x = ctrl.mean.x,
      ctrl.sd.x = ctrl.sd.x,
      ctrl.mean.y = ctrl.mean.y,
      ctrl.sd.y = ctrl.sd.y,
      ctrl.r.xy = ctrl.r.xy,
      ctrl.n = ctrl.n,
      score.x = score.x,
      score.y = score.y,
      conf.level = conf.level,
      direction.x = direction.x,
      direction.y = direction.y,
      t = round(t, dp),
      p = round(p, dp),
      z.x = round(z.x, dp),
      z.y = round(z.y, dp),
      zdcc = round(zdcc, dp),
      zdcc.ci.lb = round(zdcc.ci.lb, dp),
      zdcc.ci.ub = round(zdcc.ci.ub, dp),
      abn = round(abn, dp),
      abn.ci.lb = round(abn.ci.lb, dp),
      abn.ci.ub = round(abn.ci.ub, dp),
      dp = dp,
      method = method,
      input_df = input_df,
      output_df = output_df
    )

    class(result) <- 'discrep'
    return(result)
  }

  #' @export
  print.discrep <- function(x, ...) {


    input_table <-  knitr::kable(x$input_df, format = "simple", col.names = c("Test", "Mean", "SD", "Sample size", "r", "Case score"))
    output_table <- knitr::kable(x$output_df, format = "simple", col.names = c("Variable", "Value", paste0(x$conf.level * 100, "% CI")))

    method <- gsub("discrep_", "", class(x)[1])
    header <- "Testing for a Frequentist Discrepency Between Two Test Scores Compared to a Control Sample"
    footnote <- "See documentation for further information on how scores are computed and how to cite methods."

    result <- paste(header, "\n\n",
                    "INPUTS:", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                    "PARAMETERS:",  "\n\n",
                      paste("Method:", toupper(x$method)),"\n",
                      paste("Confidence Intervals:", x$conf.level*100, "%"),"\n",
                      paste("Direction Indicating Impairment ", "(", x$test.names[1], "): " , x$direction.x,  sep = ""),"\n",
                      paste("Direction Indicating Impairment ", "(", x$test.names[2], "): " , x$direction.y,  sep = ""),
                    "\n\n",

                    "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                    footnote, "\n",
                    sep = "")

    cat(result)
  }

  # usdt <- discrep(ctrl.mean.x = 100, ctrl.sd.x = 15, ctrl.mean.y = 110, ctrl.sd.y = 10,
  #              ctrl.r.xy = 0.5, ctrl.n = 30, score.x = 130, score.y = 120,
  #              test.names = c("Fluency", "Sequencing"),
  #              direction = "lower", tail = "one.tailed", method = "usdt")
  # rsdt <- discrep(ctrl.mean.x = 100, ctrl.sd.x = 15, ctrl.mean.y = 110, ctrl.sd.y = 10,
  #              ctrl.r.xy = 0.5, ctrl.n = 30, score.x = 130, score.y = 120,
  #              test.names = c("Fluency", "Sequencing"),
  #              tail = "one.tailed", method = "rsdt")
  # stand <- discrep(ctrl.mean.x = 100, ctrl.sd.x = 15, ctrl.mean.y = 110, ctrl.sd.y = 10,
  #              ctrl.r.xy = 0.5, ctrl.n = 30, score.x = 130, score.y = 120,
  #              test.names = c("Fluency", "Sequencing"),
  #              direction = "lower", tail = "one.tailed", method = "difflims")
  # usdt$output_df
  #rsdt
  # stand
