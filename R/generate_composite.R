#' Generate Composite Scores
#'
#' This function calculates composite scores based on subtest correlations and their inter-correlations.
#'
#' @param r A numeric vector of correlations for the included tests.
#' @param r.between A numeric vector of inter-correlations between the included tests.
#' @param mean A numeric value specifying the mean of the included tests. Default is 10.
#' @param sd A numeric value specifying the standard deviation of the included tests. Default is 3.
#' @param comp.mean A numeric value specifying the mean of the composite score. Default is 100.
#' @param comp.sd A numeric value specifying the standard deviation of the composite score. Default is 15.
#' @param dp An integer specifying the number of decimal places for rounding results. Default is 4.
#'
#' @return A list of class 'gen_comp' containing:
#' \item{k}{Number of subtests.}
#' \item{r}{Vector of correlations for the included tests.}
#' \item{r.between}{Vector of inter-correlations between the included tests.}
#' \item{composite_r}{Composite correlation.}
#' \item{original_mean}{Original mean of the composite score.}
#' \item{original_sd}{Original standard deviation of the composite score.}
#' \item{original_sem}{Original standard error of measurement.}
#' \item{original_semt}{Original standard error of the mean transformed.}
#' \item{transformed.mean}{Transformed mean of the composite score.}
#' \item{transformed.sd}{Transformed standard deviation of the composite score.}
#' \item{transformed.sem}{Transformed standard error of measurement.}
#' \item{transformed.semt}{Transformed standard error of the mean transformed.}
#'
#' @examples
#' result <- generate_composite(r = c(.87, .94, .94),
#'                              r.between = c(0.74, 0.64, 0.73),
#'                              dp = 3)
#' print(result)
#'
#' @export
generate_composite <- function(r,
                               r.between,
                               mean = 10,
                               sd = 3,
                               comp.mean = 100,
                               comp.sd = 15,
                               dp = 4) {

  k <- length(r)  # Number of subtests
  k.pairwise <- choose(k, 2)

  if (length(r.between) != k.pairwise) {
    stop(
      glue::glue("Error: The number of inter-correlations provided ({length(r.between)}) does not match the number of pairwise comparisons ({k.pairwise}) based on the number of ({k}) correlations provided.")
    )
  }

  R <- matrix(NA, nrow = length(r), ncol = length(r))
  diag(R) <- 1
  R[lower.tri(R)] <- r.between
  R[upper.tri(R)] <- r.between
  comp.r <- 1 - ((k - sum(r)) / sum(R))

  covar <- r.between * sd^2
  comp.var <- sd^2 + sd^2 + 2 * sum(covar)
  comp.sd <- sqrt(comp.var)
  comp.mean <- mean * k

  comp.sem <- comp.sd * sqrt(1 - comp.r)
  comp.semt <- comp.r * comp.sem

  transformed.mean <- 100
  transformed.sd <- 15
  transformed.sem <- transformed.sd * sqrt(1 - comp.r)
  transformed.semt <- comp.r * transformed.sem

  result <- list(
    k = k,
    r = r,
    r.between = r.between,
    composite_r = round(comp.r, dp),
    original_mean = round(comp.mean, dp),
    original_sd = round(comp.sd, dp),
    original_sem = round(comp.sem, dp),
    original_semt = round(comp.semt, dp),
    transformed.mean = round(transformed.mean, dp),
    transformed.sd = round(transformed.sd, dp),
    transformed.sem = round(transformed.sem, dp),
    transformed.semt = round(transformed.semt, dp)
  )

  class(result) <- 'gen_comp'
  return(result)
}


#' Custom Print Method for 'gen_comp' Class
#'
#' This method formats and prints the output for objects of class 'gen_comp'.
#'
#' @param x An object of class 'gen_comp'.
#' @param ... Additional arguments (not used).
#'
#' @export
print.gen_comp <- function(x, ...) {

  header <- paste("Generate Composite Score", "\n")

  input_table <- paste(
    "Input Data:\n\n",
    "number of tests included:  ", x$k, "\n",
    "r for included tests:      ", paste(round(x$r, 4), collapse = ", "), "\n",
    "r between included tests:  ", paste(round(x$r.between, 4), collapse = ", "), "\n",
    sep = ""
  )

  result_table <- paste(
    "Output Data:\n\n",
    "Composite prior to transformation:\n",
    "r:                         ", x$composite_r, "\n",
    "Mean:                      ", x$original_mean, "\n",
    "SD:                        ", x$original_sd, "\n",
    "SEM:                       ", x$original_sem, "\n",
    "SEMt (of the  score) true: ", x$original_semt, "\n\n",

    "Composite following transformation:\n",
    "r:                         ", x$composite_r, "\n",
    "Mean:                      ", x$transformed.mean, "\n",
    "SD:                        ", x$transformed.sd, "\n",
    "SEM:                       ", x$transformed.sem, "\n",
    "SEMt (of the  score) true: ", x$transformed.semt, "\n",
    sep = ""
  )

  output <- paste(header, input_table, result_table, sep = "\n")
  cat(output)
}




# # Example usage
# result <- generate_composite(r = c(.87, .94, .94),
#                              r.between = c(0.74, 0.64, 0.73),
#                              dp = 3)
# result

