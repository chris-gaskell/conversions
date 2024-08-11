#' Calculate the percentage of the normative population with j or more abnormal test scores
#'
#' This function calculates the percentage of the normative population that would exhibit
#' j or more abnormal test scores in a battery of tests. The method uses multivariate normal simulation.
#'
#' @param R A correlation matrix for the tests.
#' @param sims Number of simulations to perform. Default is 100000.
#' @param threshold Threshold for abnormal test scores. Default is -1.645 (corresponding to the 5th percentile in a standard normal distribution).
#'
#' @return A list containing:
#' \item{props}{Proportion of individuals with more than j-1 abnormal scores.}
#' \item{props.two.tail}{Proportion of individuals with more than j-1 abnormal scores, adjusted for a two-tailed test.}
#'
#' @importFrom MASS mvrnorm
#' @export
#'
#' @examples
#' # Example usage with a 4x4 identity matrix as the correlation matrix
#' R <- diag(4)
#' prevalence_of_j_abnormal_scores(R)
prevalence_of_j_abnormal_scores <- function(R, sims = 100000, threshold = -1.645) {

  k <- nrow(R)
  mean <- rep(0, k)
  data <- MASS::mvrnorm(sims, mean, R)

  sim_abns <- rowSums(data <= threshold)
  abn_counts <- sum(sim_abns)

  props <- numeric(k)

  # Calculate props
  for (i in 1:k) {
    props[i] <- sum(sim_abns > (i - 1)) / sims * 100
  }

  # Adjust for two-tailed test
  props.two.tail <- props * 2

  # Create the result list
  result <- list(
    k = k,
    abn_counts = abn_counts,
    sims = sims,
    threshold = threshold,
    props = props,
    props.two.tail = props.two.tail
  )

  class(result) <- 'ab_j_batt'
  return(result)
}

#' @export
print.ab_j_batt <- function(x, ...) {
  header <- "Abnormality of j abnormal scores in a battery of tests.\n"

  parameters <- paste(
    "Parameters:\n\n",
    "k:                         ", x$k, "\n",
    "Number of simulations:     ", x$sims, "\n",
    "Abnormality threshold: z = ", x$threshold,
    sep = ""
  )

  output.one.tailed <- "\nOutput (One-Tailed):\n\nProportion of people with more than:\n"
  for (i in 1:x$k) {
    output.one.tailed <- paste(output.one.tailed, sprintf("%d abnormal scores: %.2f%%\n", i - 1, x$props[i]), sep = "")
  }

  output.two.tailed <- "Output (Two-Tailed):\n\nProportion of people with more than:\n"
  for (i in 1:x$k) {
    output.two.tailed <- paste(output.two.tailed, sprintf("%d abnormal scores: %.2f%%\n", i - 1, x$props.two.tail[i]), sep = "")
  }

  results <- paste(header, parameters, output.one.tailed, output.two.tailed, sep = "\n")
  cat(results)
}
