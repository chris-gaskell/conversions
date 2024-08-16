#' Calculate Prevalence
#'
#' This function calculates the prevalence based on a given z-score.
#'
#' @param z A numeric value representing the z-score.
#' @param method A character string specifying the method to calculate prevalence.
#'   It must be one of "percentage", "proportion", or "ratio" (default: "ratio").
#' @param two.tailed Logical indicating whether to consider both tails of the distribution
#'   when calculating prevalence (default: FALSE).
#' @param dp Integer specifying the number of decimal places for the result (default: 5).
#'
#' @return The prevalence calculated based on the specified method.
#' @examples
#' prevalence(z = -1, method = "proportion", two.tailed = TRUE, dp = 5)
#' prevalence(z = -1, method = "proportion", two.tailed = FALSE, dp = 5)
#' @export
prevalence <- function(z, method = "proportion", two.tailed = FALSE, dp = 5) {
  if (!method %in% c("percentage", "proportion", "ratio")) {
    stop("Invalid method. Please choose one of 'percentage', 'proportion', or 'ratio'.")
  }

  if (two.tailed) {
    prevalence <- 2 * (1 - stats::pnorm(abs(z)))
  } else {
    prevalence <- 1 - stats::pnorm(abs(z))
  }

  result <- switch(method,
                   "percentage" = round(prevalence * 100, dp),
                   "proportion" = round(prevalence, dp),
                   "ratio" = round(1 / prevalence, dp))

  return(result)
}
