#' Calculate Percentile Norms
#'
#' This function calculates percentile norms for a given dataset.
#'
#' @param dat A numeric vector representing the dataset for which percentile norms are to be calculated.
#' @param method A character string specifying the method for calculating percentiles: "a" for method a, "b" for method b, "c" for method c, or "all" for all methods. Default is "all".
#' @param range An optional numeric vector specifying the range of values over which to calculate percentiles, or a range specified as a sequence (e.g., (1:12)). Default is the range from the minimum to the maximum value in the dataset.
#'
#' @return A data frame containing the following columns:
#'   \describe{
#'     \item{raw}{The raw values for which percentiles are calculated.}
#'     \item{count}{The count of occurrences of each raw value in the dataset.}
#'     \item{definition_a}{The percentile values calculated using method "a".}
#'     \item{definition_b}{The percentile values calculated using method "b".}
#'     \item{definition_c}{The percentile values calculated using method "c".}
#'   }
#'
#' @export
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' data <- rnorm(100)
#'
#' # Calculate percentile norms using all methods
#' generate_percentile_norms(data, method = "all")
#'
#' # Calculate percentile norms using a specific method
#' generate_percentile_norms(data, method = "c")
#'
#' # re-create Crawford (2009) table
#' crawford_2009 <- c(rep(0:12, c(0, 0, 0, 0, 0, 2, 4, 4, 14, 16, 20, 30, 10)))
#' generate_percentile_norms(crawford_2009)

generate_percentile_norms <- function(dat, method = "all", range = NULL) {
  # Check if dat is a numeric vector
  if (!is.numeric(dat)) {
    stop("Input 'dat' must be a numeric vector.")
  }

  # If range is specified as a sequence (e.g., (1:12)), convert it to numeric vector
  if (is.null(range)) {
    range <- c(min(dat), max(dat))
  } else if (is.character(range) && grepl(":", range)) {
    range <- eval(parse(text = range))
  }

  raw <- seq(range[1], range[2])

  if (method == "all") {
    methods <- c("a", "b", "c")
  } else {
    methods <- method
  }

  results_df <- data.frame(raw = raw, count = sapply(raw, function(x) sum(dat == x)))

  for (m in methods) {
    results_df[[paste0("definition_", m)]] <- sapply(raw, function(x) generate_percentile(data = dat, x = x, method = m))
  }

  return(results_df)
}
