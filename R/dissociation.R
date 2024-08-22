#' Dissociation Analysis Between Two Test Scores
#'
#' @description This function assesses dissociations between two test scores (X and Y) compared to control data.
#'   It helps determine if a single case's scores on two tests are abnormally divergent relative to a control sample.
#'   Both frequentist (t-test) and Bayesian approaches are available for analysis.
#'
#' @inheritParams deficit
#' @inheritParams deficit_bayes
#' @inheritParams discrep
#' @param deficit_method Character. Method for the deficit analysis. Options are 't' for frequentist t-test and 'bayes' for Bayesian analysis.
#' @param discrep_method Character. Method for the discrepancy analysis. Default is 'rsdt' (revised standardized difference test).
#'
#' @return A list containing the results of the dissociation analysis. Key outputs include:
#'   - `x.res` and `y.res`: Results for each test score, including t-values, p-values, effect sizes, and abnormality percentages.
#'   - `discrepancy.res`: Results of a discrepancy analysis between the two test scores (if applicable).
#'
#' @importFrom bayestestR hdi
#' @importFrom stats rchisq rnorm pnorm
#'
#' @references
#'   Crawford, J.R., & Garthwaite, P.H. (2007). Comparison of a single case to a control or normative sample in neuropsychology: Development of a Bayesian approach. *Cognitive Neuropsychology, 24*(4), 343-372.
#'
#' @seealso
#'   - [deficit()]: For a frequentist single dissociation test.
#'   - [deficit_bayes()]: For a Bayesian approach to single dissociation.
#'   - [discrep()]: For assessing a dissociation between two test scores.
#'   - [abnorm_ci_t()]: For generating interval estimates for abnormality.
#'
#' @examples
#' # Example usage
#' dissociation(
#'   ctrl.mean.x = 100, ctrl.sd.x = 15, ctrl.mean.y = 105, ctrl.sd.y = 10,
#'   ctrl.r.xy = 0.8, ctrl.n = 30, score.x = 130, score.y = 90
#' )
#'
#' @export
dissociation <- function(
    ctrl.mean.x,
    ctrl.sd.x,
    ctrl.mean.y,
    ctrl.sd.y,
    ctrl.r.xy,
    ctrl.n,
    score.x,
    score.y,
    direction = "lower",
    tail = "one.tailed",
    test.names = c("test X", "test Y"),
    conf.level = 0.95,
    dp = 2,
    deficit_method = "t",
    discrep_method = "rsdt"
) {

  # x deficit analysis
  x.res <- if (deficit_method == "bayes") {
    deficit_bayes(
      score = score.x,
      ctrl.mean = ctrl.mean.x,
      ctrl.sd = ctrl.sd.x,
      ctrl.n = ctrl.n,
      direction = direction,
      tail = tail,
      conf.level = conf.level,
      dp = dp
    )
  } else {
    deficit(
      score = score.x,
      ctrl.mean = ctrl.mean.x,
      ctrl.sd = ctrl.sd.x,
      ctrl.n = ctrl.n,
      direction = direction,
      tail = tail,
      conf.level = conf.level,
      dp = dp
    )
  }

  # y deficit analysis
  y.res <- if (deficit_method == "bayes") {
    deficit_bayes(
      score = score.y,
      ctrl.mean = ctrl.mean.y,
      ctrl.sd = ctrl.sd.y,
      ctrl.n = ctrl.n,
      direction = direction,
      tail = tail,
      conf.level = conf.level,
      dp = dp
    )
  } else {
    deficit(
      score = score.y,
      ctrl.mean = ctrl.mean.y,
      ctrl.sd = ctrl.sd.y,
      ctrl.n = ctrl.n,
      direction = direction,
      tail = tail,
      conf.level = conf.level,
      dp = dp
    )
  }

  # discrepancy analysis
  discrep.res <- discrep(
    score.x = score.x,
    score.y = score.y,
    ctrl.mean.x = ctrl.mean.x,
    ctrl.sd.x = ctrl.sd.x,
    ctrl.mean.y = ctrl.mean.y,
    ctrl.sd.y = ctrl.sd.y,
    ctrl.r.xy = ctrl.r.xy,
    ctrl.n = ctrl.n,
    method = discrep_method,
    conf.level = conf.level,
    direction = direction,
    tail = tail,
    test.names = test.names,
    dp = dp
  )

  # Compile results into a data frame
  deficits.df <- data.frame(
    test = test.names,
    t =   round(c(x.res$t, y.res$t), dp),
    p =   round(c(x.res$p, y.res$p), dp),
    zcc = round(c(x.res$zcc, y.res$zcc), dp),
    zcc.ci = c(
      paste(
        format(round(x.res$zcc.ci.lb, dp), nsmall = dp), "to",
        format(round(x.res$zcc.ci.ub, dp), nsmall = dp), sep = " "
      ),
      paste(
        format(round(y.res$zcc.ci.lb, dp), nsmall = dp), "to",
        format(round(y.res$zcc.ci.ub, dp), nsmall = dp), sep = " ")
    ),
    abn = round(c(x.res$abn, y.res$abn), dp),
    abn.ci = c(
      paste(
        format(round(x.res$abn.ci.lb, dp), nsmall = dp), "to",
        format(round(x.res$abn.ci.ub, dp), nsmall = dp), sep = " "
      ),
      paste(
        format(round(y.res$abn.ci.lb, dp), nsmall = dp), "to",
        format(round(y.res$abn.ci.ub, dp), nsmall = dp), sep = " ")
    ),
    deficit = c(x.res$p < 0.05, y.res$p < 0.05)
  )

  # Determine if a dissociation exists
  dissociation_exists <- (
    sum(deficits.df$deficit) == 1 &&
      (discrep.res$p < 0.05)
  )

  result <- list(
    x.res = x.res,
    y.res = y.res,
    deficits.df = deficits.df,
    discrep.res = discrep.res,
    dissociation_exists = dissociation_exists,
    discrep_method = discrep_method,
    deficit_method = deficit_method
  )

  class(result) <- 'dissociation'
  return(result)
}


#' @export
print.dissociation <- function(x, ...) {

  input_table    <- knitr::kable(x$discrep.res$input_df, format = "simple", col.names = c("Test", "Mean", "SD", "n", "r", "Case score"))
  deficits_table <- knitr::kable(x$deficits.df, format = "simple", col.names = c("Test", "t", "p-val", "zcc", glue::glue("{x$discrep.res$conf.level*100}% CI"), "Abnormality", glue::glue("{x$discrep.res$conf.level*100}% CI"), "Deficit"))
  discrep_table  <- knitr::kable(x$discrep.res$output_df, format = "simple", col.names = c("Statistic", "Value", glue::glue("{x$discrep.res$conf.level*100}% CI")))

  header <- "Assessing For a Dissociation Between a Test Score and a Control Sample."
  footnote <- "See documentation for further information on how scores are computed."

  dissociation_statement <- if (x$dissociation_exists) {
    "A dissociation exists between the test scores."
  } else {
    "No significant dissociation was found between the test scores."
  }

  result <- paste(
    header,"\n\n",
    "INPUTS:", "\n\n", paste(input_table, collapse = "\n"), "\n\n",
    "PARAMATERS:",  "\n\n",
      paste("Deficit Method:", x$deficit_method),"\n",
      paste("Deficit Method:", x$discrep_method),"\n",
      paste("Confidence Intervals:", x$discrep.res$conf.level*100, "%"),"\n\n",
    "OUTPUTS:", "\n\n",
    "1) DEFICIT ANALYSIS:", "\n\n", paste(deficits_table, collapse = "\n"), "\n\n",
    "2) DISCREPANCY ANALYSIS:", "\n\n",paste(discrep_table, collapse = "\n"), "\n\n",
    "3) DISSOCIATION ANALYSIS:", "\n\n",dissociation_statement, "\n\n",
    footnote, "\n",
    sep = ""
  )

  cat(result)
}



# test <- dissociation(ctrl.mean.x = 100, ctrl.sd.x = 15, ctrl.mean.y = 110, ctrl.sd.y = 10,
#         ctrl.r.xy = 0.5, ctrl.n = 30, score.x = 140, score.y = 120,
#         deficit_method = "t", direction = "higher",
#         test.names = c("Fluency", "Sequencing"), dp = 2
#         )
# test
#print.default(test)


#deficit(130, 100, 15, 30, conf.level = 0.95, direction = "higher", dp = 2)
