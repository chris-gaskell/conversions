#' Calculate the percentile of a value in a dataset
#'
#' This function calculates the percentile of a given value in a dataset using
#' one of three methods: "a", "b", or "c".
#'
#' @param data A numeric vector representing the dataset.
#' @param x The value for which to calculate the percentile.
#' @param method The method to use for calculating the percentile. Options are
#' "a", "b", or "c". Defaults to "c".
#'
#' @return The percentile of the value in the dataset.
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' percentile(data, 3, method = "c")
#'
#' @export
percentile <- function(data, x, method = "c") {
  sorted_data <- sort(data)
  n <- length(sorted_data)
  m <- sum(sorted_data < x)
  k <- sum(sorted_data == x)
  half_k <- k / 2

  if (method == "a") {
    result <- (m / n) * 100
  } else if (method == "b") {
    result <- ((m + k) / n) * 100
  } else if (method == "c") {
    result <- ((m + half_k) / n) * 100
  } else {
    stop("Invalid method. Choose 'a', 'b', or 'c'.")
  }

  return(result)
}



