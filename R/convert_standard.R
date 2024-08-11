#' Convert a derived score to an alternative derived score.
#'
#' This function takes a derived score and converts it to one of a number of alternative
#' derived scores (Z score, T score, index score, scaled score, sten score, stanine score).
#'
#' @param score Numeric value (or vector) containing the index scores to be converted to descriptors.
#' @param metric Character string specifying the metric of the input scores. ("z", "t", "index", "scaled", "sten", "stanine", "sat"). Default is "index"
#' @param metric.new Character string specifying the metric to convert input scores to. Choose from "z", "t", "index", "scaled", "sten", "stanine", "sat", or "percentile". Default is "index".
#' @param dp Integer specifying the number of decimal places to round the input scores to. Default is 2.
#' @examples
#' convert_standard(score = 2, metric = "z", metric.new = "t", dp = 2)
#' convert_standard(score = 2, metric = "z", metric.new = "percentile", dp = 2)
#' @return a numeric value
#' @export

convert_standard <- function(score, metric = "index", metric.new = "index", dp = 2) {
  # Define lookup tables for mean and standard deviation
  mean_table <- c(
    "index" = 100, "scaled" = 10, "z" = 0, "t" = 50, "sten" = 5.5,
    "stanine" = 5, "sat" = 500
  )
  sd_table <- c(
    "index" = 15,  "scaled" = 3,  "z" = 1, "t" = 10, "sten" = 2,
    "stanine" = 2, "sat" = 100
  )

  # Check if input types are valid
  if (!(metric %in% names(mean_table)) || !(metric.new %in% names(mean_table)) && metric.new != "percentile") {
    stop("Invalid derived score. Please specify a valid score (z, t, index, scaled, sten, stanine, sat, percentile).")
  }

  # Check if score is numeric
  if (!is.numeric(score)) {
    stop("Invalid score value. Please provide a numeric score.")
  }

  # Retrieve old mean and SD
  mean <- mean_table[metric]
  sd <- sd_table[metric]

  # Convert score to z-score
  z <- (score - mean) / sd

  if (metric.new == "percentile") {
    # Convert z-score to percentile
    result <- convert_z(z = z, metric = "percentile", dp = dp)
  } else {
    # Retrieve new mean and SD
    mean.new <- mean_table[metric.new]
    sd.new <- sd_table[metric.new]

    # Calculate the derived score
    result <- as.numeric(round((sd.new * z) + mean.new, dp))
  }

  return(result)
}
