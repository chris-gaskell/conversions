#' Calculate confidence intervals and percentile ranks
#'
#' This function calculates the confidence intervals for test scores and their percentile ranks.
#'
#' @param x A numeric vector of test scores.
#' @param R A correlation matrix for the test scores.
#' @param sem A numeric vector of standard error of measurement (SEM) values.
#' @param dp Number of decimal places for rounding in the results. Default is 2.
#' @param names Optional character vector of names for the test scores. If not provided, default names will be used.
#' @param conf.level Confidence level for the confidence interval. Default is 0.90.
#' @param abnorm_level Threshold percentile for classifying scores as abnormally low. Default is 5.
#' @param threshold Z-score threshold for classifying abnormal scores. Default is -1.645.
#' @param abnormality Logical, whether to calculate abnormality statistics. Default is TRUE.
#'
#' @return A list containing various statistics including:
#' \item{tests}{Names of the test scores.}
#' \item{x}{Original test scores.}
#' \item{ci.lb}{Lower bound of the confidence interval.}
#' \item{ci.ub}{Upper bound of the confidence interval.}
#' \item{rank}{Percentile rank of the test scores.}
#' \item{rank.ci.lb}{Percentile rank of the lower bound of the confidence interval.}
#' \item{rank.ci.ub}{Percentile rank of the upper bound of the confidence interval.}
#'
#' @importFrom stats pnorm qnorm
#' @importFrom stringr str_to_upper
#' @export
#'
#' @examples
#' # Example usage with test scores and SEM values
#' R <- diag(4)
#' sem <- c(3.000000, 3.354102, 3.674235, 4.743416)
#' scores <- c(118, 107, 77, 68)
#' sem_to_percentiles(scores, R = R, sem = sem, conf.level = 0.90)
sem_to_percentiles <- function(x, R = NULL, sem, dp = 2, names = NULL, conf.level = 0.90, threshold = -1.645, abnormality = TRUE) {
  k <- length(x)
  if (is.null(names)) {
    names <- as.character(seq(1, k))
  } else {
    names <- stringr::str_to_upper(abbreviate(names, minlength = 2, method = "left.kept"))
  }

  z.sd <- qnorm(1 - ((1 - conf.level) / 2))
  ci.lb <- x - sem * z.sd
  ci.ub <- x + sem * z.sd
  rank <- neuropsytools::convert_standard(score = x, metric = "index", metric.new = "percentile")
  rank.ci.lb <- neuropsytools::convert_standard(score = ci.lb, metric = "index", metric.new = "percentile")
  rank.ci.ub <- neuropsytools::convert_standard(score = ci.ub, metric = "index", metric.new = "percentile")

  abn.k <- sum(rank < round(pnorm(threshold)*100, 2) )

  if (abnormality) {
    if (is.null(R)) stop("Correlation matrix R is required when abnormality is TRUE.")
    abn.rates <- neuropsytools::aborm_j_battery(R = R, threshold = threshold)
    abn <- abn.rates$props[abn.k]
    if (abn.k == 0) {
      abn <- 100
    }
  } else {
    abn <- NULL
  }

  result <- list(
    x = round(x, 2),
    k = round(k, 2),
    z.sd = round(z.sd, 2),
    names = names,
    abn.k = abn.k,
    abn = abn,
    ci.lb = round(ci.lb, 2),
    ci.ub = round(ci.ub, 2),
    rank = round(rank, 2),
    rank.ci.lb = round(rank.ci.lb, 2),
    rank.ci.ub = round(rank.ci.ub, 2),
    conf.level = round(conf.level, 2),
    abnormality = abnormality
  )

  class(result) <- 'sem_to_percentiles'
  return(result)
}

#' @export
print.sem_to_percentiles <- function(x, ...) {

  # Create input data frame
  input_df <- data.frame(
    stringsAsFactors = FALSE
  )

  # Create output data frame
  output_df <- data.frame(
    Test = x$names,
    Score = format(round(x$x, 2), nsmall = 2),
    CI = paste0(format(round(x$ci.lb, 2), nsmall = 2), " - ", format(round(x$ci.ub, 2), nsmall = 2)),
    Rank = format(round(x$rank, 2), nsmall = 2),
    `Rank CI` = paste0(format(round(x$rank.ci.lb, 2), nsmall = 2), " - ", format(round(x$rank.ci.ub, 2), nsmall = 2)),
    stringsAsFactors = FALSE
  )

  # Create abnormality data frame if abnormality is TRUE
  if (x$abnormality) {
    abnorms_text <- data.frame(
      Item = "Number of abnormally low scores",
      Value = x$abn.k,
      Abnormality = paste(format(x$abn, nsmall = 2), "%"),
      stringsAsFactors = FALSE
    )
  } else {
    abnorms_text <- data.frame(
      Item = "Number of abnormally low scores",
      Value = x$abn.k,
      stringsAsFactors = FALSE
    )
  }

  #input_table <- knitr::kable(input_df, format = "simple", col.names = c("Test", "Score", "Score CI"))
  output_table <- knitr::kable(output_df, format = "simple", col.names = c("Test",  "Score", paste(x$conf.level*100, "% CI", sep = ""), "Rank",  paste(x$conf.level*100, "% CI", sep = "")))
  abnorm_table <- knitr::kable(abnorms_text, format = "simple", col.names = c("", "Value", ifelse(x$abnormality, "Population Abnormality (%)", "")))

  header <- "Confidence Intervals as Percentile Ranks"

  result <- paste(
    header, "\n\n",
    #"INPUTS:\n", paste(capture.output(input_table), collapse = "\n"), "\n\n",
    "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
    if (x$abnormality) "ABNORMALITY:" else "", paste(capture.output(abnorm_table), collapse = "\n"), "\n\n",
    sep = ""
  )

  cat(result)
}


# Example usage
# sem <- c(3.000000, 3.354102, 3.674235, 4.743416)
# R <- matrix(c(1.00, 0.61, 0.64, 0.45, # correlation matrix for WAIS-IV comps
#               0.61, 1.00, 0.62, 0.52,
#               0.64, 0.62, 1.00, 0.51,
#               0.45, 0.52, 0.51, 1.00), nrow = 4, byrow = TRUE)
# scores <- c(118, 107, 77, 68)
# names <- c("verbal comprehension", "perceptual reasoning",
#            "working memory", "processing speed")
#
# result <- sem_to_percentiles(scores, sem = sem, conf.level = 0.90, abnormality = TRUE, R = R)
# print(result)
# print.default(result)
