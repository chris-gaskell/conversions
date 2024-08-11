#' Convert a z score to an alternative derived score.
#'
#' This function takes a z score and converts it to one of a number of alternative
#' derived scores (e.g., T score, index score, scaled score, sten score, stanine score, percentile).
#'
#' @param z numeric input (or vector) specifying the Z score for conversion.
#' @param metric Character string specifying the metric to convert input scores to. Choose from "z", "t", "index", "scaled", "sten", "stanine", "sat", or "percentile". Default is "index".
#' @param dp Integer specifying the number of decimal places to round the input scores to. Default is 2.
#' @examples
#' convert_z(z = 2, metric = "index")
#' convert_z(z = 0.32, metric = "sten")
#' convert_z(z = 1, metric = "percentile")
#' @return a numeric value
#' @importFrom stats pnorm
#' @export
convert_z <- function(z, metric = "index", dp = 2) {
  # Define lookup tables for mean and standard deviation
  mean_tab <- c(
    "index" = 100, "scaled" = 10, "z" = 0, "t" = 50, "sten" = 5.5,
    "stanine" = 5, "sat" = 500
  )
  sd_tab <- c(
    "index" = 15,  "scaled" = 3,  "z" = 1, "t" = 10, "sten" = 2,
    "stanine" = 2, "sat" = 100
  )

  # Check if input types are valid
  if (!(metric %in% c(names(mean_tab), "percentile"))) {
    stop(glue::glue("'{metric}' is not a valid derived score. Please select from: {glue::glue_collapse(c(names(mean_tab), 'percentile'), ', ', last = ', or ')}."))
  }

  # Check if score is numeric
  if (!is.numeric(z)) {
    stop("Invalid z-score. Please provide a numeric score.")
  }

  # Calculate the derived score
  if (metric == "percentile") {
    result <- as.numeric(round(pnorm(z) * 100, dp))
  } else {
    mean <- mean_tab[metric]
    sd <- sd_tab[metric]
    result <- as.numeric(round((mean + (sd * z)), dp))
  }

  return(result)
}
