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
#' \item{mdi_value}{The Mahalanobis Distance Index.}
#' \item{df}{Degrees of freedom used in the calculation.}
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
  if (!is.matrix(R) || ncol(R) != length(x)) stop("R must be a square matrix with dimensions matching the length of the test scores vector")
  if (length(x) != length(norm.m) || length(x) != length(norm.sd)) stop("x, norm.m, and norm.sd must have the same length")

  scalar_quant <- norm.sd^2
  sigma <- diag(scalar_quant) %*% R # covariance matrix (Σ)
  sigma_inv <- solve(sigma) # inverse of the covariance matrix (Σ⁻¹)
  diff <- x - norm.m
  mdi_value <- as.numeric(t(diff) %*% sigma_inv %*% diff)

  df <- ncol(R)
  p_value <- 1 - pchisq(mdi_value, df)
  prev <- p_value * 100

  result <- list(
    mdi_value = round(mdi_value, 3),
    df = df,
    p_value = round(p_value, dp),
    prev = round(prev, 4)
  )

  class(result) <- 'mdi'
  return(result)
}


#' @export
print.mdi <- function(x, ...) {
  # Print the results
  name <- "MAHALANOBIS DISTANCE Index of the overall abnormality of the case's Index score profile:"
  mdi_str <- glue::glue("Chi-square = {x$mdi_value} on {x$df} df, p value = {x$p_value}")
  p_value_str <- glue::glue("Percentage of normative population expected to exhibit a more unusual profile = {x$prev}%")

  # Combine everything into the final result string
  result <- paste(name, mdi_str, p_value_str, sep = "\n\n")
  cat(result, "\n")
}
