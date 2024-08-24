#' Confidence intervals for the abnormality of a test score using a modified t-test.
#'
#' @param c An observation from a non-central t-distribution on N − 1 d.f.
#' @param n Size of the normative sample.
#' @param conf.level Confidence level (default is 0.95 for 95%).
#'
#' @return A list containing the lower and upper bounds of the 95% confidence interval for the abnormality of the test score.
#' @importFrom stats uniroot
#'
#' @examples
#' abnorm_ci_t(1.5, 30)
#' abnorm_ci_t(2, 10)
#' abnorm_ci_t(0.5, 100)
#'
#' @export
abnorm_ci_t <- function(c, n, conf.level = 0.95) {
    # Calculate the corresponding percentiles for the confidence level
    alpha <- (1 - conf.level) / 2
    pr_lower <- 1 - alpha
    pr_upper <- alpha

    # Define the function to find the non-central parameter
    f <- function(delta, pr, x, df) pt(x, df = df, ncp = delta) - pr

    # Calculate the lower and upper bounds for delta
    delta.lb <- suppressWarnings(try(uniroot(f, lower = -500, upper = 500, pr = pr_lower, x = c * (n^0.5), df = n - 1)))
    delta.ub <- suppressWarnings(try(uniroot(f, lower = -500, upper = 500, pr = pr_upper, x = c * (n^0.5), df = n - 1)))

    # Calculate the confidence interval bounds
    ci.lb <- pnorm(delta.lb$root / (n^0.5)) * 100
    ci.ub <- pnorm(delta.ub$root / (n^0.5)) * 100

    # Return the results as a list
    output <- list(delta.lb = delta.lb, delta.ub = delta.ub, lower = ci.lb, upper = ci.ub)

    return(output)
  }

