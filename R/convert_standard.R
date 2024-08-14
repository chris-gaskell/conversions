#' Convert Between Standardised Test Scores.
#'
#' This function converts standardised scores between various metrics commonly
#' used in psychological and educational testing. The conversion supports
#' several score types including Z scores, T scores, index scores, scaled
#' scores, sten scores, stanine scores, SAT scores, and percentile ranks.
#'
#' The function is flexible, allowing for custom mean and standard deviation
#' inputs for the new metric, making it suitable for a wide range of
#' standardised testing applications.
#'
#' @details The conversion process uses a linear transformation based on the
#' properties of the normal distribution. Specifically, the conversion is
#' performed using the formula (as described in Crawford, 2004):
#'
#' \deqn{X_{\text{new}} = \frac{\sigma_{\text{new}}}{\sigma_{\text{old}}} \times (X_{\text{old}} - \bar{X}_{\text{old}}) + \bar{X}_{\text{new}}}
#'
#'Where:
#'
#' \eqn{X_{\text{new}}} is the converted score,
#'
#' \eqn{X_{\text{old}}} is the original score,
#'
#' \eqn{\bar{X}_{\text{old}}} and \eqn{\sigma_{\text{old}}} are the mean and standard deviation of the original metric.
#'
#' \eqn{\bar{X}_{\text{new}}} and \eqn{\sigma_{\text{new}}} are the mean and standard deviation of the new metric.
#'
#' Below is a table summarizing the default mean and standard deviation for each metric:
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
#'
#' @param score Numeric value (or vector) containing the standardised scores to
#'   be converted.
#' @param metric Character string specifying the metric of the input scores.
#'   ("z", "t", "index", "scaled", "sten", "stanine", "sat", "percentile").
#'   Default is "z".
#' @param metric.new Character string specifying the metric to convert input
#'   scores to. Choose from "z", "t", "index", "scaled", "sten", "stanine",
#'   "sat", "percentile". Default is "z".
#' @param mean.new Optional numeric value specifying the mean of the new metric.
#'   If provided, this takes precedence over `metric.new`.
#' @param sd.new Optional numeric value specifying the standard deviation of the
#'   new metric. If provided, this takes precedence over `metric.new`.
#' @param dp Integer specifying the number of decimal places to round the
#'   converted scores to. Default is 2.
#' @return A numeric value or vector of converted scores.
#'
#' @note Ensure that the `metric` and `metric.new` are correctly specified, as
#' incorrect metrics may lead to inappropriate score conversions. The function
#' assumes that the input scores are correctly standardised according to the
#' specified input metric.
#'
#' @examples
#' # Convert a Z score to a T score
#' convert_standard(score = 2, metric = "z", metric.new = "t", dp = 2)
#'
#' # Convert a Z score to a Percentile Rank
#' convert_standard(score = 2, metric = "z", metric.new = "percentile", dp = 2)
#'
#' # Convert a Z score to a custom metric with mean 60 and standard deviation 15
#' convert_standard(score = 2, metric = "z", mean.new = 60, sd.new = 15, dp = 2)
#'
#' # Convert multiple scores from T scores to scaled scores
#' convert_standard(score = c(50, 55, 60), metric = "t", metric.new = "scaled", dp = 2)
#'
#' @references
#' - Crawford, J. R. (2013). Quantitative aspects of neuropsychological assessment. In L. H. Goldstein & J. E. McNeil (Eds.), Clinical neuropsychology: A practical guide to assessment and management for clinicians (2nd ed., pp. 97-121). Wiley-Blackwell.
#'
#' @seealso [convert_z()]
#'
#' @export
convert_standard <- function(score, metric = "z", metric.new = "index", mean.new = NULL, sd.new = NULL, dp = 2) {

  mean_table <- c(
    "index" = 100, "scaled" = 10, "z" = 0, "t" = 50, "sten" = 5.5,
    "stanine" = 5, "sat" = 500
  )
  sd_table <- c(
    "index" = 15,  "scaled" = 3,  "z" = 1, "t" = 10, "sten" = 2,
    "stanine" = 2, "sat" = 100
  )

  if (!(metric %in% names(mean_table))) {
    stop(paste("Invalid metric: ", metric, ". Please specify a valid metric from", paste(names(mean_table), collapse=", "), ".", sep = ""))
  }

  if (is.null(mean.new) || is.null(sd.new)) {
    if (!(metric.new %in% c(names(mean_table), "percentile"))) {
      stop(paste("Invalid metric.new: ", metric.new, ". Please specify a valid new metric from ", paste(c(names(mean_table), "percentile"), collapse=", "), ".", sep = ""))
    }
    mean.new <- mean_table[metric.new]
    sd.new <- sd_table[metric.new]
  } else {
    if (!is.numeric(mean.new) || !is.numeric(sd.new)) {
      stop("Both mean.new and sd.new must be numeric values.")
    }
  }

  if (!is.numeric(score)) {
    stop("Invalid score. Please provide a numeric input.")
  }

  mean <- mean_table[metric]
  sd <- sd_table[metric]
  z <- (score - mean) / sd

  if (metric.new == "percentile") {
    result <- neuropsytools::convert_z(z = z, metric = "percentile", dp = dp)
  } else {
    result <- as.numeric(round((sd.new * z) + mean.new, dp))
  }

  return(result)
}
