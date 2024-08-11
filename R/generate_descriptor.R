#' Generate a descriptor for a derived score.
#'
#' This function converts index scores to descriptive labels based on predefined threshold values for various descriptor systems such as AAN, WISC, WAIS, NEPSY, Q.Simple, Groth-Marnat, and Heaton.
#'
#' @param score Numeric value (or vector) containing the index scores to be converted to descriptors.
#' @param metric Character string specifying the metric of the input scores. ("z", "t", "index", "scaled", "sten", "stanine", "sat"). Default is "index".
#' @param system Character string specifying the descriptor system to use. Default is "aan".
#' @return A character vector containing the descriptors corresponding to the input index scores.
#' @details The function uses predefined threshold values and descriptor labels for each descriptor system to convert index scores into descriptive categories. It allows customization of the descriptor system and rounding of input scores.
#' @examples
#' # Convert index scores to descriptors using the AAN descriptor system
#' generate_descriptor(score = c(85, 105, 120), metric = "index", system = "aan")
#'
#' # Convert index scores to descriptors using the NEPSY descriptor system
#' generate_descriptor(score = c(75, 95, 110), metric = "index", system = "nepsy")
#' @export

generate_descriptor <- function(score, metric = "index", system = "aan") {
  # Define threshold values and descriptor labels for each descriptor system
  thresholds <- list(
    "aan"  =         c(69, 79, 89, 109, 119, 129),
    "wisc" =         c(69, 79, 89, 109, 119, 129),
    "wais" =         c(69, 79, 89, 109, 119, 129),
    "groth.marnat" = c(69, 79, 89, 109, 119, 129),
    "nepsy" =        c(64, 74, 89, 109),
    "q.simple" =     c(76, 84, 89, 109, 119, 129),
    "heaton" =       c(54, 61, 69, 76, 84, 91, 106, 114, 129)
  )
  descriptor_labels <- list(
    "aan" = c("Exceptionally Low", "Below Average", "Low Average", "Average", "High Average", "Above Average", "Exceptionally High"),
    "wisc" = c("Extremely Low", "Very Low", "Low Average", "Average", "High Average", "Very High", "Extremely High"),
    "wais" = c("Extremely Low", "Very Low", "Low Average", "Average", "High Average", "Superior", "Very Superior"),
    "nepsy" = c("Well Below Expected Level", "Below Expected Level", "Borderline", "At Expected Level", "Above Expected Level"),
    "q.simple" = c("Abnormal", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"),
    "groth.marnat" = c("Lower Extreme", "Well Below Average", "Low Average", "Average", "High Average", "Well Above Average", "Upper Extreme"),
    "heaton" = c("Severely Impaired", "Moderately to Severely Impaired", "Moderately Impaired", "Mildly to Moderately Impaired", "Mildly Impaired",
                 "Below Average", "Average", "Above Average", "Superior", "Very Superior")
  )

  # Check if the descriptor system is valid
  if (!(system %in% names(thresholds))) {
    stop("Invalid descriptor system. Choose from the available systems: ", paste(names(thresholds), collapse = ", "), ".")
  }

  # Convert input score to index if necessary
  if (metric != "index") {
    score <- convert_standard(score, metric, "index", 0)
  }

  # Handle NA values
  if (any(is.na(score))) {
    warning("NA values detected in score. These will be returned as NA in the output.")
  }

  # Find appropriate descriptor label
  result <- cut(score, breaks = c(-Inf, thresholds[[system]], Inf), labels = descriptor_labels[[system]], include.lowest = TRUE, right = TRUE)

  return(as.character(result))
}
