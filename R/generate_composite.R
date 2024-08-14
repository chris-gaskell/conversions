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


#' @export
print.gen_comp <- function(x, ...) {

  # Create input data frame
  input_df <- data.frame(
    Item = c("Number of tests included", "r for included tests", "r between included tests"),
    Value = c(x$k,
              paste(round(x$r, 4), collapse = ", "),
              paste(round(x$r.between, 4), collapse = ", ")),
    stringsAsFactors = FALSE
  )

  # Create output data frame with original and transformed values in separate columns
  output_df <- data.frame(
    Item = c("Composite r",
             "Mean",
             "SD",
             "SEM",
             "SEMt (true)"),
    Original = c(format(x$composite_r, nsmall = 4),
                 format(x$original_mean, nsmall = 4),
                 format(x$original_sd, nsmall = 4),
                 format(x$original_sem, nsmall = 4),
                 format(x$original_semt, nsmall = 4)),
    Transformed = c("", #format(x$composite_r, nsmall = 4),  # Composite r remains the same
                    format(x$transformed.mean, nsmall = 4),
                    format(x$transformed.sd, nsmall = 4),
                    format(x$transformed.sem, nsmall = 4),
                    format(x$transformed.semt, nsmall = 4)),
    stringsAsFactors = FALSE
  )

  # Create the input and output tables
  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Inputs", "Value"))
  output_table <- knitr::kable(output_df, format = "simple", col.names = c("Outputs", "Original", "Transformed"))

  # Define the header
  header <- "Generate Composite Score"

  # Combine all parts into the final result
  result <- paste(header, "\n\n",
                  "INPUTS:\n", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "OUTPUTS:\n", paste(capture.output(output_table), collapse = "\n"), "\n",
                  sep = "")

  # Print the result
  cat(result)
}




# # Example usage
result <- generate_composite(r = c(.87, .94, .94, .94),
                             r.between = c(0.74, 0.64, 0.73, 0.73, 0.73, 0.73),
                             dp = 3)
result

