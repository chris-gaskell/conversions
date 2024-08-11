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
sem_to_percentiles <- function(x, R = NULL, sem, dp = 2, names = NULL, conf.level = 0.90, abnorm_level = 5, threshold = -1.645, abnormality = TRUE) {
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

  abn.k <- sum(rank < abnorm_level)

  if (abnormality) {
    if (is.null(R)) stop("Correlation matrix R is required when abnormality is TRUE.")
    abn.rates <- neuropsytools::prevalence_of_j_abnormal_scores(R = R, threshold = threshold)
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
  width_test <- 10
  width_score <- 10
  width_ci <- 18
  width_rank <- 10
  width_rank_ci <- 10
  dp <- 2

  header <- "Confidence Intervals as Percentile Ranks\n"
  line <- "________________________________________________________________________________\n"

  column_headers <- paste(
    sprintf('%-*s', width_test, "Test"),
    sprintf('%-*s', width_score + 4, "Score"),
    sprintf('%-*s', width_ci - 6, "Score CI"),
    sprintf('%-*s', width_rank, "Rank"),
    sprintf('%-*s', width_rank_ci, "Rank CI"),
    "\n"
  )

  abnorms_text <- if (x$abnormality) {
    paste("\n",
          "NUMBER of case's Index scores classified as abnormally low = ",
          x$abn.k, ".",
          "\n",
          "\nPERCENTAGE of normal population expected to exhibit this number or more of abnormally low scores = ", x$abn, "%", "\n", sep = ""
    )
  } else {
    paste("\n",
          "NUMBER of case's Index scores classified as abnormally low = ",
          x$abn.k, ".",
          "\n", sep = "")
  }

  # Initialize output as an empty string
  output <- ""

  # Loop over the data to create the rows of output
  for (i in seq_along(x$names)) {
    output <- paste(
      output,
      sprintf('%-*s', width_test, x$names[i]),
      sprintf('%-*s', width_score, format(round(x$x[i], dp), nsmall = dp)),
      sprintf('%-*s', width_ci, paste0(format(round(x$ci.lb[i], dp), nsmall = dp), " - ", format(round(x$ci.ub[i], dp), nsmall = dp))),
      sprintf('%-*s', width_rank, format(round(x$rank[i], dp), nsmall = dp)),
      sprintf('%-*s', width_rank_ci, paste0(format(round(x$rank.ci.lb[i], dp), nsmall = dp), " - ", format(round(x$rank.ci.ub[i], dp), nsmall = dp))),
      "\n",
      sep = ""
    )
  }

  # Combine everything into the final result string
  formatted_output <- paste(header, line, column_headers, line, output, abnorms_text, sep = "")
  cat(formatted_output)
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
