#' Calculate the Mahalanobis Distance Index (MDI)
#'
#' This function calculates the Mahalanobis Distance Index (MDI) for a given test score vector,
#' a correlation matrix, and the normative mean. It also computes the p-value and the percentage
#' of the normative population expected to exhibit a more unusual profile.
#'
#' @param R A correlation matrix for the tests.
#' @param x A numeric vector of test scores.
#' @param norm.m A numeric vector of normative means.
#' @param norm.sd A numeric vector of normative SDs.
#' @param dp Number of decimal places for rounding in the results. Default is 7.
#'
#' @return A list containing:
#' \item{mdi}{The Mahalanobis Distance Index.}
#' \item{p_value}{The p-value associated with the MDI.}
#' \item{prev}{The percentage of the normative population expected to exhibit a more unusual profile.}
#'
#' @importFrom stats pchisq
#' @export
#'
#' @examples
#' # Example usage with a correlation matrix, test scores, and normative means
#' R <- diag(4) # Example correlation matrix
#' x <- c(118, 107, 77, 68)
#' norm.m <- c(100, 100, 100, 100)
#' norm.sd <- c(15, 15, 15, 15)
#' mdi_results <- mdi(R, x, norm.m = norm.m, norm.sd = norm.sd)
#' print(mdi_results)
mdi <- function(R, x, norm.m, norm.sd, dp = 7) {
  scalar_quant <- norm.sd * norm.sd
  sigma <- scalar_quant * R # covariance matrix (Σ)
  sigma_inv <- solve(sigma) # inverse of the covariance matrix (Σ⁻¹)
  diff <- x - norm.m
  mdi_value <- as.numeric(t(diff) %*% sigma_inv %*% diff)

  df <- ncol(R)
  p_value <- 1 - pchisq(mdi_value, df)
  prev <- p_value * 100

  result <- list(
    mdi_value = round(mdi_value, 3),
    df = df,
    p_value = round(p_value, 7),
    prev = round(prev, 4)
  )

  class(result) <- 'mdi'
  return(result)
}


#' Custom Print Method for MDI
#'
#' This function provides a custom print method for displaying the results
#' of the Mahalanobis Distance Index (MDI) calculation in a formatted way.
#'
#' @param x An object containing the MDI results, which includes the
#' chi-square value, degrees of freedom (df), and p-value.
#' @param ... Additional arguments passed to other methods (currently not used).
#'
#' @return The function prints the formatted MDI result but does not return a value.
#' @export
print.mdi <- function(x, ...) {
  # Print the results
  name <- "MAHALANOBIS DISTANCE Index of the overall abnormality of the case's Index score profile: "
  mdi_str <- glue::glue("Chi-square = {x} on {x$df} df, p value = {x$p_value}")
  p_value_str <- glue::glue("Percentage of normative population expected to exhibit a more unusual profile = {x$prev}%")

  # Combine everything into the final result string
  result <- paste(name, mdi_str, p_value_str, sep = "\n\n")
  cat(result)
}
