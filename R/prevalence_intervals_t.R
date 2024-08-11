#' Confidence intervals for the abnormality of a test score using a modified t-test.
#'
#' @param c An observation from a non-central t-distribution on N − 1 d.f.
#' @param n Size of the normative sample.
#'
#' @return A list containing the lower and upper bounds of the 95% confidence interval for the abnormality of the test score.
#' @importFrom stats uniroot
#' @export
#'
#' @examples
#' prevalence_intervals_t(1.5, 30)
#' prevalence_intervals_t(2, 10)
#' prevalence_intervals_t(0.5, 100)
prevalence_intervals_t <- function(c,n)
{
  #finding the non central parameter
  f <- function(delta, pr, x, df) pt(x, df = df, ncp = delta) - pr
  delta.lb <- suppressWarnings(try(uniroot(f, lower=-500, upper=500, pr = 0.975, x = c*(n^0.5), df = n-1)))
  delta.ub <- suppressWarnings(try(uniroot(f, lower=-500, upper=500, pr = 0.025, x = c*(n^0.5), df = n-1)))
  ci.lb <- pnorm(delta.lb$root/(n^0.5)) * 100
  ci.ub <- pnorm(delta.ub$root/(n^0.5)) * 100

  output <- list(delta.lb = delta.lb, delta.ub = delta.ub, '2.5%' = ci.lb, '97.5%' = ci.ub)
  #output <- c('2.5%' = CI_L, '97.5%' = CI_U)

  return(output)
}
