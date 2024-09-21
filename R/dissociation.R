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
    direction.x = "lower",
    direction.y = "lower",
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
      direction = direction.x,
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
      direction = direction.x,
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
      direction = direction.y,
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
      direction = direction.y,
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
    direction.x = direction.x,
    direction.y = direction.y,
    tail = tail,
    test.names = test.names,
    dp = dp
  )

  # Compile results into a data frame
  deficits.df <-
    data.frame(
    test = test.names,
    t =   c(x.res$t, y.res$t),
    p =   c(x.res$p.value, y.res$p.value),
    zcc = c(x.res$zcc, y.res$zcc),
    zcc.ci = c(
      paste(
        format(x.res$zcc.ci.lb, nsmall = dp), "to",
        format(x.res$zcc.ci.ub, nsmall = dp), sep = " "
      ),
      paste(
        format(y.res$zcc.ci.lb, nsmall = dp), "to",
        format(y.res$zcc.ci.ub, nsmall = dp), sep = " ")
    ),
    abn = c(x.res$abn, y.res$abn),
    abn.ci = c(
      paste(
        format(x.res$abn.ci.lb, nsmall = dp), "to",
        format(x.res$abn.ci.ub, nsmall = dp), sep = " "
      ),
      paste(
        format(y.res$abn.ci.lb, nsmall = dp), "to",
        format(y.res$abn.ci.ub, nsmall = dp), sep = " ")
    ),
    deficit = c(x.res$p.value < 0.05, y.res$p.value < 0.05)
  )


  adj_discrep_method <- if (discrep_method == "rsdt") {
    "RSDT (Crawford & Garthwaite, 2005)"
  } else if (discrep_method == "usdt") {
    "USDT (Crawford & Garthwaite, 2005)"
  } else if (discrep_method == "difflims") {
    "Difflims (Crawford et al. 2002)"
  } else {
    "Unknown method"
  }

  parameters_df <- data.frame(
    item = c("Deficit Method",
             "Discrepancy Method",
             "Confidence Interval Method",
             "Confidence Intervals",
             "Hypothesis",
             paste("Direction Indicating Impairment ", "(", test.names[1], ")",  sep = ""),
             paste("Direction Indicating Impairment ", "(", test.names[2], ")",  sep = "")
    ),
    value = c(
      "Modified T (Crawford & Howell, 1998)",
      adj_discrep_method,
      "Modified T (Crawford & Garthwaite, 2002)",
      paste(conf.level * 100, "%", sep = ""),
      stringr::str_to_title(gsub("\\.", "-", tail)),
      stringr::str_to_title(direction.x),
      stringr::str_to_title(direction.y)
    ),
    stringsAsFactors = FALSE
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
    deficit_method = deficit_method,
    parameters_df = parameters_df
  )

  class(result) <- 'dissociation'
  return(result)
}


#' @export
print.dissociation <- function(x, ...) {

  input_table    <- knitr::kable(x$discrep.res$input_df, format = "simple", col.names = c("Test", "Mean", "SD", "Sample size", "r", "Case score"))
  parameters_table <- knitr::kable(x$parameters_df, format = "simple",
                                   #col.names = c("Parameter", "Value")
                                   )
  deficits_table <- knitr::kable(x$deficits.df, format = "simple",
                                 col.names = c("Test", "t-value", "p-value", "z-dcc", glue::glue("{x$discrep.res$conf.level*100}% CI"), "Abnormality", glue::glue("{x$discrep.res$conf.level*100}% CI"), "Deficit")
                                 )
  discrep_table  <- knitr::kable(x$discrep.res$output_df, format = "simple", col.names = c("Statistic", "Value", glue::glue("{x$discrep.res$conf.level*100}% CI")))

  header <- "Testing for a Frequentist Dissociation Between Two Test Scores Compared to a Control Sample."
  footnote <- "See documentation for further information on how scores are computed."
  key <- paste("- Abnormality = The percentage of controls expected to show a higher deficit.", "\n",
               "- z-dcc = Z  discrepancy for the case control.", sep = ""
  )


  dissociation_statement <- if (x$dissociation_exists) {
    "A dissociation exists between the test scores."
  } else {
    "No significant dissociation was found between the test scores."
  }

  result <- paste(
    header,"\n\n",
    "INPUTS:", "\n\n", paste(input_table, collapse = "\n"), "\n\n",
    "PARAMETERS:", paste(capture.output(parameters_table), collapse = "\n"), "\n\n",
    "OUTPUTS:", "\n\n",
    "1) DEFICIT ANALYSIS:", "\n\n", paste(deficits_table, collapse = "\n"), "\n\n",
    "2) DISCREPANCY ANALYSIS:", "\n\n",paste(discrep_table, collapse = "\n"), "\n\n",
    "3) DISSOCIATION ANALYSIS:", "\n\n",dissociation_statement, "\n\n",
    "Note.", "\n", key, "\n\n",
    footnote, "\n",
    sep = ""
  )

  cat(result)
}


# test.names = c("Fluency", "Sequencing")
#
# dissociation(ctrl.mean.x = 100, ctrl.sd.x = 15, ctrl.mean.y = 110, ctrl.sd.y = 10,
#         ctrl.r.xy = 0.5, ctrl.n = 30, score.x = 140, score.y = 120,
#         deficit_method = "t", direction.x = "higher", direction.y  = "higher",
#         test.names = test.names, dp = 2
#         )

#print.default(test)


#deficit(130, 100, 15, 30, conf.level = 0.95, direction = "higher", dp = 2)
