#' Calculate the percentiles of a vector of values
#'
#' This function calculates the percentiles of a vector of values using one of three methods: "a", "b", or "c".
#'
#' @param data A numeric vector representing the dataset.
#' @param method The method to use for calculating the percentiles.
#'   Options are "a", "b", or "c". Defaults to "c".
#'
#' @return A numeric vector containing the percentiles of the input data.
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' generate_percentile_vector(data, method = "c")
#'
#' @export
generate_percentile_vector <- function(data, method = "c") {
  if (length(data) == 1) {
    return(generate_percentile(data, data, method))
  } else {
    return(sapply(data, function(x) generate_percentile(data, x, method)))
  }
}
