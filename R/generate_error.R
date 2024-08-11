#' Standard Error of the Mean
#'
#' Calculate the standard error of the mean given the standard deviation
#' and sample size, or calculate from data.
#'
#'
#' @param dat (numeric) A numeric vector from which to calculate standard
#'   deviation and sample size. If provided, sd and n will be ignored.
#' @param sd (numeric) The standard deviation of the data. Ignored if dat is
#'   provided.
#' @param n (numeric) The sample size. Ignored if dat is provided.
#'
#' @return The standard error of the mean.
#'
#' @examples
#' # If dat is provided, calculate sd and n from dat
#' se_mean(c(1, 2, 3, 4, 5))
#'
#' # If sd and n are provided
#' se_mean(sd = 3, n = 30)
#'
#' # If dat is not provided but sd and n are provided
#' se_mean(sd = 3, n = 30)
#'
#' @export
se_mean <- function(dat = NULL, sd = NULL, n = NULL) {
  if (!is.null(dat)) {
    if (is.vector(dat)) {
      sd <- sd(dat)
      n <- length(dat)
    } else {
      stop("dat must be a vector.")
    }
  } else if (is.null(sd) || is.null(n)) {
    stop("If dat is not provided, both sd and n must be provided.")
  }

  if (!is.null(sd) && !is.numeric(sd)) {stop("sd must be numeric.")}
  if (!is.null(dat) && !is.numeric(dat)) {stop("dat must be numeric.")}
  if (!is.null(n) && !is.numeric(n)) {stop("n must be numeric.")}
  if (!is.null(sd) && length(sd) > 1) {
    stop("If sd has more than one value, please provide 'dat' instead.")
  }

  return(sd/sqrt(n))
}






#'Standard Error of Measurement (SEM)
#'
#'Calculate the standard error of measurement (SEM) given the reliability and
#'standard deviation of scores
#'
#'The Standard Error of Measurement is the standard deviation of an infinite
#'number of hypothetical scores around the true score. The SEM is calculated as
#'the standard deviation (SD) multiplied by the square root of 1 minus the test
#'retest reliability (r) of the measurement tool.
#'
#'
#'@param dat (numeric) A numeric vector of measurements from which to calculate
#'  the standard deviation. Ignored if r and SD are provided.
#'@param sd (numeric) The standard deviation of the measurements. If dat is
#'  provided, sd will be calculated from dat.
#'@param r (numeric) The reliability of the test.
#'
#'@return The standard error of measurement (SEM).
#'
#' @examples
#' # If dat and r are provided, calculate sd from dat and then SEM
#' se_measurement(dat = c(1, 2, 3, 4, 5), r = 0.4)
#'
#' # If sd and r are provided, calculate SEM
#' se_measurement(sd = 3, r = 0.4)
#'
#'@export
se_measurement <- function(dat = NULL, sd = NULL, r = NULL) {
  if (!is.null(dat) && !is.null(sd)) {
    stop("Provide either 'dat' as a vector or 'sd' as a single numeric value, not both.")
  }
  if (is.null(dat) && is.null(sd)) {stop("Either 'dat' or 'sd' must be provided.")}
  if (!is.null(sd) && !is.numeric(sd)) {stop("sd must be numeric.")}
  if (!is.null(dat) && !is.numeric(dat)) {stop("dat must be numeric.")}
  if (!is.null(r) && !is.numeric(r)) {stop("r must be numeric.")}
  if (!is.null(sd) && length(sd) > 1) {
    stop("If sd has more than one value, please provide 'dat' instead.")
  }

  if (!is.null(dat)) {
    if (is.vector(dat)) {
      sd_value <- sd(dat)
    } else if (is.numeric(dat)) {
      sd_value <- dat
    } else {
      stop("dat must be either a vector or a numeric value.")
    }
  } else {
    sd_value <- sd
  }

  if (is.null(r)) {
    stop("r (reliability of the test) is missing.")
  }

  return(sd_value * sqrt(1 - r))
}




#' Standard Error of Difference Between Means
#'
#' Calculate the standard error of the difference (SED) between means.
#'
#' The SED is the expected spread of the distribution of change scores if no
#' actual change had occurred. This is calculated using the approach offered by
#' Iverson (2001) which uses the SEM of the normative data for each testing
#' interval.
#'
#' @param sd.1 (numeric) Standard deviation of the first group or vector of
#'   standard deviations for each observation in the first group.
#' @param sd.2 (numeric) Standard deviation of the second group or vector of
#'   standard deviations for each observation in the second group.
#' @param r (numeric) The reliability of the test used to obtain the standard
#'   deviations.
#'
#' @return The standard error of difference between means.
#'
#' @examples
#' # If sd.1 and sd.2 are single values
#' se_difference(sd.1 = 3, sd.2 = 4, r = 0.5)
#'
#' # If sd.1 and sd.2 are vectors of the same length
#' se_difference(sd.1 = c(3, 4, 5), sd.2 = c(4, 5, 6), r = 0.5)
#'
#' @export
se_difference <- function(sd.1, sd.2, r) {
  # using both SEM 1 and SEM 2
  if (is.null(sd.1) && is.null(sd.2) && is.null(r)) {stop("'sd.1' 'sd.2' and 'r' must be provided.")}
  if (!is.null(sd.1) && !is.numeric(sd.1)) {stop("sd.1 must be numeric.")}
  if (!is.null(sd.2) && !is.numeric(sd.2)) {stop("sd.2 must be numeric.")}
  if (!is.null(r)    && !is.numeric(r))    {stop("r must be numeric.")}
  if (!is.null(sd.1) && !is.null(sd.2) && length(sd.1) != length(sd.2)) {
    stop("sd.1 and sd.2 must be of equal length.")
  }

  if (length(sd.1) == 1 && length(sd.2) == 1) {
    se_measure.1 <- se_measurement(sd = sd.1, r = r)
    se_measure.2 <- se_measurement(sd = sd.2, r = r)
  } else {
    se_measure.1 <- se_measurement(dat = sd.1, r = r)
    se_measure.2 <- se_measurement(dat = sd.2, r = r)
  }

  se_difference <- sqrt((se_measure.1^2) + (se_measure.2^2))
  return(se_difference)
}


#' Standard Error of Prediction
#'
#' This function calculates the standard error of prediction based on the
#' provided standard deviations and the reliability coefficient.
#'
#' @param sd.1 Standard deviation of the first variable.
#' @param sd.2 Standard deviation of the second variable.
#' @param r Reliability coefficient (reliability of the test).
#'
#' @return The standard error of prediction.
#'
#' @examples
#' # Single values
#' se_prediction(3, 4, 0.8)
#'
#' # Vectors
#' se_prediction(c(3, 2, 4), c(4, 3, 5), 0.8)
#'
#' @importFrom stats sd
#' @export
se_prediction <- function(sd.1, sd.2, r) {
  if (missing(sd.1) || missing(sd.2) || missing(r)) {
    stop("'sd.1', 'sd.2', and 'r' must be provided.")
  }
  if (!is.numeric(sd.1)) stop("sd.1 must be numeric.")
  if (!is.numeric(sd.2)) stop("sd.2 must be numeric.")
  if (!is.numeric(r)) stop("r must be numeric.")
  if (length(sd.1) != length(sd.2) && length(sd.1) > 1) {
    stop("sd.1 and sd.2 must be of equal length.")
  }

  # Use standard deviation function from stats package
  if (length(sd.1) == 1 && length(sd.2) == 1) {
    se_prediction <- sqrt(((sd.1^2) + (sd.2^2)) * (1 - r))
  } else {
    sd_1 <- sd(sd.1)
    sd_2 <- sd(sd.2)
    se_prediction <- sqrt(((sd_1^2) + (sd_2^2)) * (1 - r))
  }

  return(se_prediction)
}




#' Standard Error for n1
#'
#' This function calculates the standard error of the predicted score with
#' Crawford adjustment.
#'
#' @param sd.1 Standard deviation of the first variable.
#' @param sd.2 Standard deviation of the second variable.
#' @param r Reliability coefficient (reliability of the test).
#' @param n Sample size.
#' @param t1.score Score of the first test.
#' @param t2.score Score of the second test.
#' @param norm.t1.mean Mean of the norm group for the first test.
#'
#' @return The standard error for sample size n=1.
#'
#' @examples
#' # Example usage:
#' se_n1(sd.1 = 3, sd.2 = 4, r = 0.8, n = 100, t1.score = 75, norm.t1.mean = 70)
#'
#' @export
se_n1 <- function(sd.1, sd.2, r, n, t1.score, t2.score, norm.t1.mean) {

    se_prediction <- se_prediction(sd.1 = sd.1, sd.2 = sd.2, r = r)
    se_n1 <- se_prediction* sqrt(1+(1/n)+((t1.score-norm.t1.mean)*(t1.score-norm.t1.mean))/(sd.1*sd.1*(n-1))
    )

  return(se_n1)
}


#' Regression Based Estimate for Stability
#'
#' This function estimates stability regularization using the provided
#' parameters.
#'
#' @param b A numeric value representing the beta
#' @param score A numeric value representing the score.
#' @param c A numeric value representing the constant.
#'
#' @return Returns the estimate of the time 2 score.
#' @examples
#' stability_reg_est(0.5, 10, 3)
#'
#' @export
stability_reg_est <- function(b, score, c) {
  round(b * (score + c), 2)
}



