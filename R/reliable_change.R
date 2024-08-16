utils::globalVariables(c("discrep", "ci.lb", "ci.ub"))

#' Discrepncy Between Predicted and Observed Time 2 Test Scores (Reliable
#' Change).
#'
#' This function compares stability methods given the scores of two tests.
#'
#' @name reliable_change
#' @param t1.score Scores of the first test.
#' @param t2.score Scores of the second test.
#' @param norm.t1.mean Mean of the normative sample at time 1.
#' @param norm.t1.sd Standard deviation of the normative sample at time 1.
#' @param norm.t2.mean Mean of the normative sample at time 2.
#' @param norm.t2.sd Standard deviation of the normative sample at time 2.
#' @param norm.r Reliability of the normative sample.
#' @param norm.n Sample size of the normative sample.
#' @param conf.level Confidence level for stability assessment.
#' @param methods A character vector specifying which methods to calculate.
#'   Default is "all" methods.
#'
#' @importFrom utils capture.output globalVariables
#' @importFrom glue glue
#' @importFrom knitr kable
#' @importFrom dplyr relocate
#' @export
#'
#' @examples
#' # Example data
#' t1.score <- 88
#' norm.t1.mean <- 113
#' norm.t1.sd <- 7.2
#' norm.t2.mean <- 118.7
#' norm.t2.sd <- 8.3
#' norm.r <- 0.93
#' norm.n <- 821
#' t2.score <- 103
#' ci <- 0.95
#'
#' # Using all methods
#' result_all <- reliable_change(t1.score, t2.score,
#' norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n,
#' ci)
#' print(result_all)
#'
#' # Using selected methods
#' result_selected <- reliable_change(t1.score, t2.score,
#' norm.t1.mean, norm.t1.sd,norm.t2.mean, norm.t2.sd, norm.r,
#' norm.n, ci, methods = c("crawford", "maassen"))
#' print(result_selected)


utils::globalVariables(c("method", "expected", "actual"))

reliable_change <- function(
    t1.score, t2.score = NULL,
    norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd,
    norm.r, norm.n, conf.level = .9,
    methods = "all", predictions_only = FALSE) {

  all_methods <- c("jacobson", "speer", "chelune", "mcsweeny", "charter", "CH", "temkin", "iverson", "maassen", "crawford")

  if (any(methods == "all")) {
    methods <- all_methods
  }

  tab <- sapply(methods, function(method) {
    rci.tab <- predict_t2(method, t1.score,
                              norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd,
                              norm.r, norm.n, conf.level = conf.level)
    c(expected = rci.tab$t2.expected,
      sem = rci.tab$t2.sem,
      ci.lb = rci.tab$t2.ci.lb,
      ci.ub = rci.tab$t2.ci.ub)
  })

  tab <- as.data.frame(t(tab))
  tab$method <- row.names(tab)

  if (!predictions_only) {
    tab$actual <- t2.score
    tab$z <- round((tab$actual - tab$expected) / tab$sem, 2)
    tab$discrep <- tab$actual - tab$expected
    tab <- tab |> dplyr::relocate(method, expected, ci.lb, ci.ub, actual, discrep)
  } else {
    tab <- tab |> dplyr::relocate(method, expected, ci.lb, ci.ub)
  }

  rownames(tab) <- NULL

  result <- list(
    tab = tab,
    t1.score = t1.score,
    norm.t1.mean = norm.t1.mean,
    norm.t1.sd = norm.t1.sd,
    norm.t2.mean = norm.t2.mean,
    norm.t2.sd = norm.t2.sd,
    norm.r = norm.r,
    norm.n = norm.n,
    conf.level = conf.level,
    predictions_only = predictions_only
  )

  if (!predictions_only) {
    result$t2.score <- t2.score
  }

  class(result) <- 'reliable_change'
  return(result)
}

#' @export
print.reliable_change <- function(x, ...) {
  header <- "Discrepncy Between Predicted and Observed Time 2 Test Scores (Reliable Change)\n\n"
  footnote <- "\nSee documentation for further information on how scores are computed."

  # Input Data Frame with customizable names
  input_df <- data.frame(
    Variable = c("Time 1", "Time 2"),
    Mean = c(x$norm.t1.mean, x$norm.t2.mean),
    SD = c(x$norm.t1.sd, x$norm.t2.sd),
    `Case's Score` = c(x$t1.score, if (!x$predictions_only) x$t2.score else ""),
    n = c(x$norm.n, ""),
    r = c(x$norm.r, ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Variable", "Mean", "SD", "Case's Score", "n", "r"))

  input <- paste(
    "INPUT:",
    paste(capture.output(input_table), collapse = "\n"), "\n",
    sep = ""
  )

  params <- paste(
    "\n\nPARAMS:", "\n",
    "Confidence level: ", x$conf.level*100, "%\n\n",
    sep = ""
  )

  output_table <- if (x$predictions_only) {
    knitr::kable(x$tab, format = "simple",
                 col.names = c("Method", "Predicted", "CI lb", "CI ub", "Error")
    )
  } else {
    knitr::kable(x$tab, format = "simple",
                 col.names = c("Method", "Predicted", "CI lb", "CI ub", "Observed", "Discrepency", "Error", "Z")
    )
  }

  output <- paste(
    "OUTPUT:",
    paste(capture.output(output_table), collapse = "\n"), "\n",
    sep = ""
  )

  result <- paste(header,
                  input,
                  params,
                  output,
                  footnote,
                  sep = "")
  cat(result)
}

# # Example usage
# t1.score <- 88
# norm.t1.mean <- 113
# norm.t1.sd <- 7.2
# norm.t2.mean <- 118.7
# norm.t2.sd <- 8.3
# norm.r <- 0.73
# norm.n <- 821
# t2.score <- 103
# ci <- 0.95
#
# # Example with predictions only
# result_pred_only <- reliable_change(t1.score, norm.t1.mean = norm.t1.mean, norm.t1.sd = norm.t1.sd,
#                                              norm.t2.mean = norm.t2.mean, norm.t2.sd = norm.t2.sd,
#                                              norm.r = norm.r, norm.n = norm.n, conf.level = ci, methods = c("crawford", "all"),
#                                              predictions_only = TRUE)
#
# print.default(result_pred_only)
# print(result_pred_only)
#
# # Example with full comparison
# result_full <- reliable_change(t1.score, t2.score,
#                                         norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd,
#                                         norm.r, norm.n, conf.level = ci, methods = "all")
#
# print(result_full)
