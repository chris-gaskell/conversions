#' Sequential Bonferroni Correction
#'
#' Applies the Sequential Bonferroni correction to a vector of p-values.
#'
#' @param p.vals A numeric vector of p-values to be adjusted.
#' @param alpha A numeric value representing the significance level (default is 0.05).
#'
#' @return A list with the following components:
#' \item{sig}{A logical vector indicating which p-values are significant after correction.}
#' \item{adj.p.vals}{A numeric vector of adjusted p-values.}
#'
#' @details This function applies the Sequential Bonferroni correction to control the
#' family-wise error rate (FWER) when performing multiple hypothesis tests. It adjusts
#' the significance threshold for each p-value based on its rank in the ordered list of p-values.
#'
#' @examples
#' # Example p-values
#' p.vals <- c(0.001, 0.04, 0.003, 0.08)
#'
#' # Apply the sequential Bonferroni correction
#' results <- sequential_bonferroni(p.vals)
#'
#' # Print significant results
#' results$sig
#'
#' # Print adjusted p-values
#' results$adj.p.vals
#'
#' @export
sequential_bonferroni <- function(p.vals, alpha = 0.05) {
  k <- length(p.vals)

  sorted.indices <- order(p.vals)
  sorted.p.vals <- p.vals[sorted.indices]

  sig <- rep(FALSE, k)
  adj.p.vals <- rep(NA, k)

  for (i in 1:k) {
    adj.alpha <- alpha / (k - i + 1)

    adj.p.vals[sorted.indices[i]] <- sorted.p.vals[i] * (k - i + 1)

    if (sorted.p.vals[i] <= adj.alpha) {
      sig[sorted.indices[i]] <- TRUE
    } else {
      break
    }
  }

  adj.p.vals[adj.p.vals > 1] <- 1

  return(list(sig = sig, adj.p.vals = adj.p.vals))
}
