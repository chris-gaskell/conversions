#' Convert Z Score to Alternative Standardized Score.
#'
#' This function takes a Z score and converts it to one of a number of alternative
#' derived scores (e.g., T score, index score, scaled score, sten score, stanine score, SAT score, percentile).
#'
#' The function is flexible, allowing for custom mean and standard deviation
#' inputs for the new metric, making it suitable for a wide range of
#' standardised testing applications.
#'
#' @details The conversion process uses a linear transformation based on the
#' properties of the normal distribution. Specifically, the conversion is
#' performed using the formula (as described in Crawford, 2004):
#'
#' \deqn{
#' X_{\text{new}} = \frac{\sigma_{\text{new}}}{\sigma_{\text{old}}} \times
#' (X_{\text{old}} - \bar{X}_{\text{old}}) + \bar{X}_{\text{new}}
#' }
#'
#' Where:
#'
#' \eqn{X_{\text{new}}} is the converted score,
#'
#' \eqn{X_{\text{old}}} is the original score,
#'
#' \eqn{\bar{X}_{\text{old}}} and \eqn{\sigma_{\text{old}}} are the mean and
#' standard deviation of the original metric.
#'
#' \eqn{\bar{X}_{\text{new}}} and \eqn{\sigma_{\text{new}}} are the mean and
#' standard deviation of the new metric.
#'
#' Below is a table summarizing the default mean and standard deviation for each
#' metric:
#'
#' | Metric       | Mean           | Standard Deviation             |
#' |--------------|----------------|--------------------------------|
#' | Z-score      | 0              | 1                              |
#' | T-score      | 50             | 10                             |
#' | Index Score  | 100            | 15                             |
#' | Scaled Score | 10             | 3                              |
#' | Sten Score   | 5.5            | 2                              |
#' | Stanine      | 5              | 2                              |
#' | SAT Score    | 500            | 100                            |
#' | Percentile   | 50             | N/A (non-linear)               |
#'
#' @param z Numeric input (or vector) specifying the Z score for conversion.
#' @param metric Character string specifying the metric to convert Z scores to.
#'   Choose from "z", "t", "index", "scaled", "sten", "stanine", "sat", or
#'   "percentile". Default is "z".
#' @param mean.new Optional numeric value specifying the mean of the new metric.
#'   If provided, this takes precedence over `metric`.
#' @param sd.new Optional numeric value specifying the standard deviation of the
#'   new metric. If provided, this takes precedence over `metric`.
#' @param dp Integer specifying the number of decimal places to round the
#'   converted scores to. Default is 2.
#' @return A numeric value or vector of converted scores.
#' @examples
#' convert_z(z = 2, metric = "index")
#' convert_z(z = 0.32, metric = "sten")
#' convert_z(z = 1, metric = "percentile")
#' convert_z(z = 1, mean.new = 60, sd.new = 15)
#' @return A numeric value
#' @importFrom stats pnorm
#'
#' @seealso [convert_standard()] for converting between different standardised metrics.
#' @export
convert_z <- function(z, metric = "index", mean.new = NULL, sd.new = NULL, dp = 2) {
  mean_tab <- c(
    "index" = 100, "scaled" = 10, "z" = 0, "t" = 50, "sten" = 5.5,
    "stanine" = 5, "sat" = 500
  )
  sd_tab <- c(
    "index" = 15,  "scaled" = 3,  "z" = 1, "t" = 10, "sten" = 2,
    "stanine" = 2, "sat" = 100
  )

  if (!(metric %in% c(names(mean_tab), "percentile"))) {
    stop(glue::glue("'{metric}' is not a valid derived score. Please select from: {glue::glue_collapse(c(names(mean_tab), 'percentile'), ', ', last = ', or ')}."))
  }

  if (!is.numeric(z)) {
    stop("Invalid z-score. Please provide a numeric score.")
  }

  if (is.null(mean.new) || is.null(sd.new)) {
    if (metric == "percentile") {
      result <- as.numeric(round(pnorm(z) * 100, dp))
      return(result)
    } else {
      mean.new <- mean_tab[metric]
      sd.new <- sd_tab[metric]
    }
  } else {
    if (!is.numeric(mean.new) || !is.numeric(sd.new)) {
      stop("Both mean.new and sd.new must be numeric values.")
    }
  }

  result <- as.numeric(round( (sd.new / 1) * (z - 0) + mean.new, dp))
  return(result)
}
